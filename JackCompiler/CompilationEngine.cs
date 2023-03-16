using System.Xml;

namespace JackCompiler
{
    public class CompilationEngine
    {
        private JackTokenizer tokenizer;
        private XmlWriter xw;
        private VMWriter vw;
        private SymbolTable st;

        private readonly string currentClassName;
        private static readonly string OS_MALLOC_FUNCTION = "Memory.alloc";
        private static readonly string OS_STRING_FUNCTION = "String.new";

        private static int IFS = -1;
        private static int WHILES = -1;

        public CompilationEngine(JackTokenizer tokenizer, string outputFilePath)
        {
            var settings = new XmlWriterSettings()
            {
                Indent = true,
                OmitXmlDeclaration = true,
                ConformanceLevel = ConformanceLevel.Fragment,
                //IndentChars = "",
                Encoding = new System.Text.UTF8Encoding(false),
                //NewLineHandling = NewLineHandling.None,
            };

            xw = XmlWriter.Create(outputFilePath, settings);
            vw = new VMWriter(outputFilePath.Replace(".xml", ".vm"));

            this.tokenizer = tokenizer;
            this.currentClassName = Path.GetFileName(outputFilePath).Replace(".xml", "");
        }

        /// <summary>
        /// Compiles a complete class.
        /// </summary>
        public void CompileClass()
        {
            st = new SymbolTable();
            xw.WriteStartElement("class");
            CompileKeyword("class");

            // Class name should be the same as file name
            if (tokenizer.currentToken != currentClassName)
            {
                throw new Exception($"expected class identifier should have same name as {currentClassName}");
            }

            CompileIdentifier(tokenizer.currentToken); // className
            CompileSymbol("{");

            // classVarDec*
            CompileClassVarDec();
            CompileSubroutineDec();

            CompileSymbol("}");
            xw.WriteEndElement(); // end class
            xw.WriteString("\n");
        }

        /// <summary>
        /// Compiles a static/field declaration (classVarDec*)
        /// </summary>
        private void CompileClassVarDec()
        {
            if (tokenizer.currentToken != "static" &&
                tokenizer.currentToken != "field")
            {
                return;
            }

            string kindString, name, type;

            xw.WriteStartElement("classVarDec");

            // static | field
            kindString = tokenizer.currentToken;
            CompileKeyword(kindString);
            type = tokenizer.currentToken; // int, Animal, boolean, etc.
            CompileType(type);
            name = tokenizer.currentToken; // foo, bar

            bool CouldNotParseForSomeReason = Enum.TryParse(kindString.ToUpper(), out SymbolTable.Kind kind) == false;

            if (CouldNotParseForSomeReason)
            {
                throw new Exception("Could not parse for some reason");
            }

            st.Define(name, type, kind);

            CompileIdentifier(name, true); // varName

            // field int x, y;
            while (tokenizer.currentToken == ",")
            {
                CompileSymbol(",");
                name = tokenizer.currentToken;
                st.Define(name, type, kind);
                CompileIdentifier(name, true); // varName2, etc
            }

            CompileSymbol(";");
            xw.WriteEndElement();
            CompileClassVarDec();
        }

        /// <summary>
        /// Compiles a complete method, function, or constructor
        /// </summary>
        private void CompileSubroutineDec()
        {
            // e.g. function void main()
            if (tokenizer.currentToken != "constructor" &&
                tokenizer.currentToken != "function" &&
                tokenizer.currentToken != "method")
            {
                // TODO: log error/ throw exception
                return;
            }

            st.StartSubroutine();
            IFS = -1;
            WHILES = -1;

            // function, constructor, method
            string subroutineType = tokenizer.currentToken;

            if (subroutineType == "method")
            {
                // Initialize this (if method) and args
                st.Define("this", currentClassName, SymbolTable.Kind.ARG);
            }

            xw.WriteStartElement("subroutineDec");

            CompileKeyword(subroutineType);

            // method Employee getEmployee()
            if (tokenizer.TokenType() == JackTokenizer.Token.KEYWORD)
            {
                // void | int | char | boolean
                CompileKeyword(tokenizer.currentToken);
            } else
            {
                // className
                CompileType(tokenizer.currentToken);
            }

            var funcName = tokenizer.currentToken;

            CompileIdentifier(funcName); // getEmployee
            CompileSymbol("(");
            CompileParameterList();
            CompileSymbol(")");

            xw.WriteStartElement("subroutineBody");

            CompileSymbol("{");

            // varDec*
            CompileVarDec();

            // after compiling variable declarations, the ST contains all local variables
            var numLocals = st.VarCount(SymbolTable.Kind.VAR);
            vw.WriteFunction($"{currentClassName}.{funcName}", numLocals);

            if (subroutineType == "constructor")
            {
                // create space in the heap for object with number of field variables
                vw.WritePush(VMWriter.Segment.CONSTANT, st.VarCount(SymbolTable.Kind.FIELD));
                vw.WriteCall(OS_MALLOC_FUNCTION, 1);

                // let 'this' point to the base address of the returned memory block
                // pop this 0
                vw.WritePop(VMWriter.Segment.POINTER, 0);
            }

            // correctly set the 'this' pointer to passed in baseAddr
            if (subroutineType == "method")
            {
                vw.WritePush(VMWriter.Segment.ARGUMENT, 0);
                vw.WritePop(VMWriter.Segment.POINTER, 0);
            }

            CompileStatements();
            CompileSymbol("}");

            xw.WriteEndElement(); // end subRoutineBody
            xw.WriteEndElement(); // end subRoutineDec
            CompileSubroutineDec();
        }

        /// <summary>
        /// Compiles a (possibly empty) parameter list. Not including the enclosing ()
        /// e.g. int aX, int aY
        /// </summary>
        private void CompileParameterList()
        {
            string name, type;

            xw.WriteStartElement("parameterList");

            while (tokenizer.currentToken != ")")
            {
                type = tokenizer.currentToken;
                CompileType(type);

                name = tokenizer.currentToken;
                st.Define(name, type, SymbolTable.Kind.ARG);
                CompileIdentifier(name);

                if (tokenizer.currentToken == ",")
                {
                    CompileSymbol(",");
                }
            }

            xw.WriteFullEndElement(); // end parameterList
        }

        /// <summary>
        /// Compiles a var declaration
        /// e.g. var int x;
        /// </summary>
        private void CompileVarDec()
        {
            if (tokenizer.currentToken != "var")
            {
                return;
            }

            string name, type;

            xw.WriteStartElement("varDec");
            CompileKeyword("var");

            type = tokenizer.currentToken;
            CompileType(type);

            name = tokenizer.currentToken;
            st.Define(name, type, SymbolTable.Kind.VAR);

            CompileIdentifier(name, true);

            while (tokenizer.currentToken == ",")
            {
                CompileSymbol(",");
                name = tokenizer.currentToken;
                st.Define(name, type, SymbolTable.Kind.VAR);
                CompileIdentifier(name, true);
            }

            CompileSymbol(";");
            xw.WriteEndElement(); // endVarDec
            CompileVarDec();
        }

        /// <summary>
        /// Compiles a sequence of statements
        /// </summary>
        private void CompileStatements()
        {
            xw.WriteStartElement("statements");

            // loop through all statements
            while (tokenizer.currentToken != ";" && tokenizer.currentToken != "}")
            {
                switch (tokenizer.currentToken)
                {
                    case "let":
                        CompileLet();
                        break;
                    case "if":
                        CompileIf(++IFS);
                        break;
                    case "while":
                        CompileWhile(++WHILES);
                        break;
                    case "do":
                        CompileDo();
                        break;
                    case "return":
                        CompileReturn();
                        break;
                    default:
                        throw new InvalidDataException("Not a valid statement");
                }
            }

            xw.WriteFullEndElement(); // end statements
        }

        // Compiles a 'do' statement
        private void CompileDo()
        {
            xw.WriteStartElement("doStatement");
            CompileKeyword("do");
            // compileExpression will call compileSubroutine
            CompileExpression();
            // do Output.printInt(10);
            // All return values after a do Statement are disregarded.
            vw.WritePop(VMWriter.Segment.TEMP, 0);
            CompileSymbol(";");
            xw.WriteEndElement(); // end doStatement
        }

        // Compiles a 'let' statement
        private void CompileLet()
        {
            xw.WriteStartElement("letStatement");
            CompileKeyword("let");
            var varName = tokenizer.Identifier();
            CompileIdentifier(varName);
            bool isAssignmentToArray = false;

            // We're compiling an array
            if (tokenizer.currentToken == "[")
            {
                isAssignmentToArray = true;
                CompileSymbol("[");
                // push arr
                CompileExpression();
                vw.WritePush(st.KindOf(varName).GetVMSegment(), st.IndexOf(varName));
                // push i
                vw.WriteArithmetic(VMWriter.Command.ADD);
                CompileSymbol("]");
                // add
            }
            // a[1]
            //CompileTerm();

            CompileSymbol("=");
            // 500
            // This result will be stored result in temp 0
            CompileExpression();

            if (isAssignmentToArray)
            {
                vw.WritePop(VMWriter.Segment.TEMP, 0);
                vw.WritePop(VMWriter.Segment.POINTER, 1);
                vw.WritePush(VMWriter.Segment.TEMP, 0);
                vw.WritePop(VMWriter.Segment.THAT, 0);
            } else
            {
                // ... then assign it to variable above
                var vmSegment = st.KindOf(varName).GetVMSegment();
                vw.WritePop(vmSegment, st.IndexOf(varName));
            }

            CompileSymbol(";");
            xw.WriteEndElement(); // end letStatement
        }

        // Compiles a 'while' statement
        private void CompileWhile(int count)
        {
            xw.WriteStartElement("whileStatement");
            CompileKeyword("while");
            CompileSymbol("(");
            vw.WriteLabel($"WHILE_EXP{count}");
            CompileExpression();
            vw.WriteArithmetic(VMWriter.Command.NOT);
            CompileSymbol(")");
            vw.WriteIf($"WHILE_END{count}");
            CompileSymbol("{");
            CompileStatements();
            vw.WriteGoto($"WHILE_EXP{count}");
            CompileSymbol("}");
            xw.WriteEndElement();
            vw.WriteLabel($"WHILE_END{count}");
        }

        // Compiles a 'return' statement
        private void CompileReturn()
        {
            xw.WriteStartElement("returnStatement");
            CompileKeyword("return");

            if (tokenizer.currentToken != ";")
            {
                CompileExpression();
            }
            // return; (void function).
            // we push 0 onto the stack and then call return
            else
            {
                vw.WritePush(VMWriter.Segment.CONSTANT, 0);
            }

            CompileSymbol(";");
            xw.WriteEndElement(); // end returnStatement
            vw.WriteReturn();
        }

        /// <summary>
        /// Compiles an 'if' statement, with a possible trailing 'else'
        /// </summary>
        /// <param name="count">counter used for recursive calls.</param>
        /// <returns>Describe return value.</returns>
        private void CompileIf(int count)
        {
            xw.WriteStartElement("ifStatement");
            CompileKeyword("if");
            CompileSymbol("(");
            CompileExpression();
            CompileSymbol(")");
            vw.WriteIf($"IF_TRUE{count}");
            vw.WriteGoto($"IF_FALSE{count}");
            CompileSymbol("{");
            vw.WriteLabel($"IF_TRUE{count}");
            CompileStatements();
            CompileSymbol("}");

            // If there's an else block...
            if (tokenizer.currentToken == "else")
            {
                vw.WriteGoto($"IF_END{count}");
                vw.WriteLabel($"IF_FALSE{count}");
                CompileKeyword("else");
                CompileSymbol("{");
                CompileStatements();
                CompileSymbol("}");
                vw.WriteLabel($"IF_END{count}");
            } else
            {
                vw.WriteLabel($"IF_FALSE{count}");
            }

            xw.WriteEndElement(); // end ifStatement
        }

        /// <summary>
        /// Compiles an expression.
        /// expr = term (op term)*
        /// </summary>
        private void CompileExpression()
        {
            xw.WriteStartElement("expression");
            CompileTerm();
            char op;

            while (tokenizer.TokenType() == JackTokenizer.Token.SYMBOL &&
                tokenizer.Symbol().IsValidOp())
            {
                op = tokenizer.Symbol();
                CompileSymbol(tokenizer.currentToken);
                CompileTerm();

                if (op == '*')
                {
                    vw.WriteCall("Math.multiply", 2);
                    break;
                }

                if (op == '/')
                {
                    vw.WriteCall("Math.divide", 2);
                    break;
                }

                var cmd = op.GetCommandFromOperand();
                vw.WriteArithmetic(cmd);
            }

            xw.WriteEndElement(); // endExpression
        }

        /// <summary>
        /// If the current token is an identifier, the routine must resolve
        /// it into a variable, arrayelement or subroutine call.
        /// term = intConst | strConst | kwConst | varName | varName[expression]
        ///         | subroutineCall | '('expression')' | unaryOp term
        /// </summary>
        private void CompileTerm()
        {
            var tokenType = tokenizer.TokenType();

            xw.WriteStartElement("term");

            // Compile '('expression')' e.g (500 / 10)
            if (tokenizer.currentToken == "(")
            {
                CompileSymbol("(");
                CompileExpression();
                CompileSymbol(")");
                xw.WriteEndElement(); // end term
                return;
            }

            // Compile UnaryOp term =>  ~(10 > 2), -100
            if (tokenizer.currentToken == "-" ||
                tokenizer.currentToken == "~")
            {
                var op = tokenizer.Symbol();
                CompileSymbol(tokenizer.currentToken);
                CompileTerm();

                vw.WriteArithmetic(op.GetCommandFromOperand(true));
            }

            // TODO: No default ?
            switch (tokenType)
            {
                case JackTokenizer.Token.INT_CONST:
                    xw.WriteStartElement("integerConstant");
                    xw.WriteString(tokenizer.GetKeyWord());
                    xw.WriteEndElement();
                    vw.WritePush(VMWriter.Segment.CONSTANT, tokenizer.IntVal());
                    tokenizer.Advance();
                    break;
                case JackTokenizer.Token.STRING_CONST:
                    var str = tokenizer.StringVal();
                    xw.WriteStartElement("stringConstant");
                    xw.WriteString(str);
                    xw.WriteEndElement();
                    tokenizer.Advance();
                    // call String constructor
                    vw.WritePush(VMWriter.Segment.CONSTANT, str.Length);
                    vw.WriteCall(OS_STRING_FUNCTION, 1);

                    // initialize the returned object with characters bycalling appendchar
                    foreach (char c in str)
                    {
                        vw.WritePush(VMWriter.Segment.CONSTANT, c);
                        // account for 'this' already pushed onto stack by OS_STRING_FUNCTION
                        vw.WriteCall("String.appendChar", 2);
                    }

                    break;
                // Todo: change from keyword to kwconstant?
                case JackTokenizer.Token.KEYWORD:
                    xw.WriteStartElement("keyword");
                    xw.WriteString(tokenizer.GetKeyWord());
                    xw.WriteEndElement();

                    string keyword = tokenizer.GetKeyWord();

                    if (!keyword.IsKeywordConstant())
                    {
                        throw new Exception($"{keyword} is not a valid keyword constant");
                    }

                    if (keyword == "null" || keyword == "false")
                    {
                        vw.WritePush(VMWriter.Segment.CONSTANT, 0);
                    }

                    // put -1 (1111 1111 1111 1111) on the stack
                    if (keyword == "true")
                    {
                        //vw.WritePush(VMWriter.Segment.CONSTANT, 1);
                        //vw.WriteArithmetic(VMWriter.Command.NEG);

                        vw.WritePush(VMWriter.Segment.CONSTANT, 0);
                        vw.WriteArithmetic(VMWriter.Command.NOT);
                    }

                    if (keyword == "this")
                    {
                        vw.WritePush(VMWriter.Segment.POINTER, 0);
                    }

                    tokenizer.Advance();
                    break;
                // unaryOp term / '(' expression ')'
                case JackTokenizer.Token.SYMBOL:
                    if (tokenizer.Symbol() == '(')
                    {
                        CompileSymbol("(");
                        CompileExpression();
                        CompileSymbol(")");
                    }

                    if (tokenizer.Symbol().IsUnaryOp())
                    {
                        char unaryOp = tokenizer.Symbol();
                        CompileTerm();
                        vw.WriteArithmetic(unaryOp.GetCommandFromOperand(true));
                    }
                    break;
                case JackTokenizer.Token.IDENTIFIER:

                    // varName / varName[expression] / square.moveUp()
                    if (st.IsIdentifierDefined(tokenizer.Identifier()))
                    {
                        var varName = tokenizer.Identifier();

                        CompileIdentifier(varName); // varName / square

                        // TODO: move all here. clean up below
                        //vw.WritePush(st.KindOf(varName).GetVMSegment(), st.IndexOf(varName));

                        // square.moveUp()
                        if (tokenizer.currentToken == ".")
                        {
                            // Array, Point, etc
                            if (!varName.IsValidType(out bool isCustomType) && isCustomType)
                            {
                                throw new Exception($"Expected custom type, but {varName} is not.");
                            }

                            CompileSymbol(".");

                            //CompileSubroutineCall();
                            var methodName = tokenizer.currentToken;
                            CompileIdentifier(methodName); // moveUp
                            CompileSymbol("(");
                            // TODO: move this up, after compiling arrays
                            // push the current 'this' variable onto the stack before calling function
                            vw.WritePush(st.KindOf(varName).GetVMSegment(), st.IndexOf(varName));
                            var numArgs = CompileExpressionList() + 1; // account for already pushed 'this'
                            CompileSymbol(")");


                            vw.WriteCall($"{st.TypeOf(varName)}.{methodName}", numArgs);
                        } else if (tokenizer.currentToken == "[")
                        // RHS of an assignment. let x = a[i];
                        // LHS is handled inside CompileLet
                        {
                            CompileSymbol("[");
                            CompileExpression();
                            vw.WritePush(st.KindOf(varName).GetVMSegment(), st.IndexOf(varName));
                            vw.WriteArithmetic(VMWriter.Command.ADD);
                            CompileSymbol("]");

                            // put a[i] on stack
                            vw.WritePop(VMWriter.Segment.POINTER, 1);
                            vw.WritePush(VMWriter.Segment.THAT, 0);

                        } else
                        // just varName.
                        {
                            // put the value of the variable varName on the stack
                            vw.WritePush(st.KindOf(varName).GetVMSegment(), st.IndexOf(varName));
                        }
                    }
                    else
                    {
                        // subroutineCall Math.add(expressionList) | add(expressionList) aka this.add(expressionList) | square.moveUp()
                        // Math.add | add | this.add |
                        CompileSubroutineCall();
                    }

                    break;
            }

            xw.WriteEndElement(); // end term
        }

        // Compile constructor
        // Point.new(2, 3)

        // Compile subroutine call e.g.
        // add(3 + 5, 2) => this.add(3 + 5, 2) | Math.add(3, 7)

        // Helper method
        private void CompileSubroutineCall()
        {
            var subroutineName = tokenizer.currentToken;
            int numArgs = 0;

            CompileIdentifier(subroutineName);

            // Math.add()
            if (tokenizer.currentToken == ".")
            {
                subroutineName += ".";
                CompileSymbol(".");
                subroutineName += tokenizer.currentToken;

                // Constructor
                if (tokenizer.currentToken == "new")
                {
                }

                CompileIdentifier(tokenizer.currentToken);
            } else
            // add(3, 2), We transform this to this.add(3,2).
            {
                // push this onto stack
                vw.WritePush(VMWriter.Segment.POINTER, 0);
                // indicate pushed value
                numArgs++;
                subroutineName = $"{currentClassName}.{subroutineName}";
            }

            if (tokenizer.currentToken == "(")
            {
                CompileSymbol("(");
                numArgs += CompileExpressionList();
                CompileSymbol(")");
            }

            // Write function call
            vw.WriteCall(subroutineName, numArgs);
        }

        /// <summary>
        /// Compiles a possibly empty comma-separated list of expressions.
        /// </summary>
        /// <returns>The number of expressions in the list</returns>
        private int CompileExpressionList()
        {
            xw.WriteStartElement("expressionList");

            int i = 0;

            if (tokenizer.currentToken == ")")
            {
                xw.WriteFullEndElement(); // end expressionList
                return i;
            }

            CompileExpression();
            i++;

            // Go through the rest of the expressions
            while (tokenizer.currentToken == ",")
            {
                CompileSymbol(",");
                CompileExpression();
                i++;
            }

            xw.WriteFullEndElement(); // end expressionList
            return i;
        }

        // Helper method
        private void CompileIdentifier(string identifier, bool isDeclaration = false)
        {
            if (!identifier.IsValidIdentifier())
            {
                throw new Exception($"{identifier} is not a valid identifier.");
            }

            xw.WriteStartElement("identifier");

            xw.WriteStartElement("name");
            xw.WriteString(identifier);
            xw.WriteEndElement(); // end name

            bool IsIdentifierDefined = st.IsIdentifierDefined(identifier);
            if (IsIdentifierDefined)
            {
                var kind = st.KindOf(identifier);

                // e.g. Dog.Bark(); these are identifiers that do not need to be stored in the ST.
                bool isClassOrSubroutineIdentifier = (kind == SymbolTable.Kind.CLASS
                    || kind == SymbolTable.Kind.SUBROUTINE);

                // Show the running index of the identifier
                if (!isClassOrSubroutineIdentifier)
                {
                    // The identifier's category: var, argument, static, field, class, subroutine
                    xw.WriteStartElement("category");
                    xw.WriteString(st.KindOf(identifier).ToString());
                    xw.WriteEndElement(); // end category

                    xw.WriteStartElement("type");
                    xw.WriteString(st.TypeOf(identifier));
                    xw.WriteEndElement(); // end type

                    xw.WriteStartElement("IsDeclaration");
                    xw.WriteString(isDeclaration.ToString());
                    xw.WriteEndElement(); // end usedOrDeclared

                    xw.WriteStartElement("index");
                    xw.WriteString(st.IndexOf(identifier).ToString());
                    xw.WriteEndElement(); // end index
                }
            }

            xw.WriteEndElement(); // end identifer
            tokenizer.Advance();
        }

        // Helper method
        private void CompileType(string type)
        {
            bool isCustomType;

            if (!type.IsValidType(out isCustomType))
            {
                throw new Exception($"{type} is not a valid type.");
            }

            if (isCustomType)
            {
                CompileIdentifier(type);
                return;
            }

            xw.WriteStartElement("keyword");
            xw.WriteString(tokenizer.GetKeyWord());
            xw.WriteEndElement();

            tokenizer.Advance();
        }

        // Helper method
        private void CompileSymbol(string symbol)
        {
            if (tokenizer.currentToken != symbol)
            {
                throw new Exception($"Invalid token! Expected {symbol} got {tokenizer.currentToken}");
            }

            if (tokenizer.TokenType() != JackTokenizer.Token.SYMBOL)
            {
                throw new Exception($"{symbol} is not a valid symbol");
            }

            xw.WriteStartElement("symbol");
            xw.WriteString(tokenizer.GetKeyWord());
            xw.WriteEndElement();

            tokenizer.Advance();
        }

        // Helper method
        private void CompileKeyword(string kw)
        {
            if (tokenizer.currentToken != kw)
            {
                throw new Exception($"Invalid token! Expected {kw} got {tokenizer.currentToken}");
            }

            if (tokenizer.TokenType() != JackTokenizer.Token.KEYWORD)
            {
                throw new Exception($"{kw} is not a valid keyword");
            }

            xw.WriteStartElement("keyword");
            xw.WriteString(tokenizer.GetKeyWord());
            xw.WriteEndElement();

            tokenizer.Advance();
        }

        // Closes the output file
        public void ShutDown()
        {
            tokenizer.fs.Close();
            vw.Close();
            xw.Close();
        }
    }
}
