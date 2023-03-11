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
            CompileSubRoutineDec();

            CompileSymbol("}");
            xw.WriteEndElement(); // end class
            xw.WriteString("\n");
        }

        // Compiles a static/field declaration (classVarDec*)
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

        // Compiles a complete method, function, or constructor
        private void CompileSubRoutineDec()
        {
            // e.g. function void main()
            if (tokenizer.currentToken != "constructor" &&
                tokenizer.currentToken != "function" &&
                tokenizer.currentToken != "method")
            {
                return;
            }

            st.StartSubroutine();

            if (tokenizer.currentToken == "method")
            {
                // Initialize this (if method) and args
                st.Define("this", currentClassName, SymbolTable.Kind.ARG);
            }

            xw.WriteStartElement("subroutineDec");

            CompileKeyword(tokenizer.currentToken); // function, constructor, method

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

            CompileIdentifier(tokenizer.currentToken); // getEmployee
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

            CompileStatements();
            CompileSymbol("}");

            xw.WriteEndElement(); // end subRoutineBody
            xw.WriteEndElement(); // end subRoutineDec
            CompileSubRoutineDec();
        }

        // Compiles a (possibly empty) parameter list. Not including the enclosing ()
        // int aX, int aY
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

        // Compiles a var declaration
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

        // Compiles a sequence of statements
        private void CompileStatements(int numIfs = -1, int numWhiles = -1)
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
                        CompileIf(++numIfs);
                        break;
                    case "while":
                        CompileWhile(++numWhiles);
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
            CompileSubroutineCall();
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
            CompileIdentifier(tokenizer.currentToken);

            // array [ i + 1 ]
            if (tokenizer.currentToken == "[")
            {
                CompileSymbol("[");
                CompileExpression();
                CompileSymbol("]");
            }

            CompileSymbol("=");

            // compile expression to place value on the stack...
            CompileExpression();
            // ... then assign it to variable above
            var vmSegment = st.KindOf(varName).GetVMSegment();
            vw.WritePop(vmSegment, st.IndexOf(varName));
            CompileSymbol(";");
            xw.WriteEndElement(); // end letStatement
        }

        // Compiles a 'while' statement
        private void CompileWhile(int numWhiles)
        {
            xw.WriteStartElement("whileStatement");
            CompileKeyword("while");
            CompileSymbol("(");
            vw.WriteLabel($"WHILE_EXPR{numWhiles}");
            CompileExpression();
            vw.WriteArithmetic(VMWriter.Command.NOT);
            CompileSymbol(")");
            vw.WriteIf($"WHILE_END{numWhiles}");
            CompileSymbol("{");
            CompileStatements();
            vw.WriteGoto($"WHILE_EXPR{numWhiles}");
            CompileSymbol("}");
            xw.WriteEndElement();
            vw.WriteLabel($"WHILE_END{numWhiles}");
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

        // Compiles an 'if' statement
        // With a possible trailing 'else'
        private void CompileIf(int numIfs)
        {
            xw.WriteStartElement("ifStatement");
            CompileKeyword("if");
            CompileSymbol("(");
            CompileExpression();
            CompileSymbol(")");
            vw.WriteArithmetic(VMWriter.Command.NOT);
            // vw.WriteIf($"IF_FALSE{CURRENT_IF}");
            vw.WriteIf($"IF_FALSE{numIfs}");
            CompileSymbol("{");
            CompileStatements(numIfs);
            CompileSymbol("}");

            //vw.WriteGoto($"IF_END{CURRENT_IF}");
            vw.WriteGoto($"IF_END{numIfs}");

            //vw.WriteLabel($"IF_FALSE{CURRENT_IF}");
            vw.WriteLabel($"IF_FALSE{numIfs}");

            // If there's an else block...
            if (tokenizer.currentToken == "else")
            {
                CompileKeyword("else");
                CompileSymbol("{");
                CompileStatements(numIfs);
                CompileSymbol("}");
            }

            //vw.WriteLabel($"IF_END{CURRENT_IF}");
            vw.WriteLabel($"IF_END{numIfs}");

            // vw.WriteLabel($"IF_END{CURRENT_IF}");
            xw.WriteEndElement(); // end ifStatement

            //CURRENT_IF++;
            //numIfs++;
        }

        private void CompileExpression()
        {
            xw.WriteStartElement("expression");
            CompileTerm();
            char op;

            while (tokenizer.TokenType() == JackTokenizer.Token.SYMBOL &&
                tokenizer.Symbol().IsValidOp())
            {
                op = tokenizer.Symbol();
                // TODO: op?
                CompileSymbol(tokenizer.currentToken);
                CompileTerm();
                //vw.WritePush(VMWriter.Segment.CONSTANT, tokenizer.IntVal());
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
                // tokenizer.Advance();
            }

            xw.WriteEndElement(); // endExpression
        }

        // TODO: Document
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
                    xw.WriteStartElement("stringConstant");
                    xw.WriteString(tokenizer.StringVal());
                    xw.WriteEndElement();
                    tokenizer.Advance();
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

                    vw.WritePush(VMWriter.Segment.CONSTANT, 0);

                    // put -1 (1111 1111 1111 1111) on the stack
                    if (keyword == "true")
                        vw.WriteArithmetic(VMWriter.Command.NOT);

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
                // subroutineCall /
                case JackTokenizer.Token.IDENTIFIER:

                    if (st.IsIdentifierDefined(tokenizer.Identifier()))
                    {
                        // varName / varName[expression]
                        var varName = tokenizer.Identifier();
                        // put the value of the variable on the stack

                        //st.IndexOf(varName);
                        // pop local
                        // TODO: pop/push?
                        vw.WritePush(st.KindOf(varName).GetVMSegment(), st.IndexOf(varName));
                        CompileIdentifier(tokenizer.currentToken);
                    }
                    else
                    {
                        CompileSubroutineCall();
                    }

                    break;
            }

            xw.WriteEndElement(); // end term
        }

        // Compile constructor
        // Point.new(2, 3)

        // Compile subroutine call e.g.
        // add(3 + 5, 2) | Math.add(3, 7) | square.moveUp()

        // Compile array e.g.
        // names[0 + 1], fruits[i]

        // Helper method
        private void CompileSubroutineCall()
        {
            var subroutineName = tokenizer.currentToken;
            int numExpressions = 0;

            CompileIdentifier(subroutineName);

            if (tokenizer.currentToken == ".")
            {
                subroutineName += ".";
                CompileSymbol(".");
                subroutineName += tokenizer.currentToken;
                CompileIdentifier(tokenizer.currentToken);

                // Constructor
                if (tokenizer.currentToken == "new")
                {

                }
            }

            if (tokenizer.currentToken == "(")
            {
                CompileSymbol("(");
                numExpressions = CompileExpressionList();
                CompileSymbol(")");
            }

            // Actually just an array
            if (tokenizer.currentToken == "[")
            {
                CompileSymbol("[");
                CompileExpression();
                CompileSymbol("]");
            } else
            {
                // Is Function call, for real
                vw.WriteCall(subroutineName, numExpressions);
            }
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
                bool isClassOrSubroutineIdentifier = (kind == SymbolTable.Kind.CLASS || kind == SymbolTable.Kind.SUBROUTINE);

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
                throw new System.Exception($"Invalid token! Expected {symbol} got {tokenizer.currentToken}");
            }

            if (tokenizer.TokenType() != JackTokenizer.Token.SYMBOL)
            {
                throw new System.Exception($"{symbol} is not a valid symbol");
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
                throw new System.Exception($"Invalid token! Expected {kw} got {tokenizer.currentToken}");
            }

            if (tokenizer.TokenType() != JackTokenizer.Token.KEYWORD)
            {
                throw new System.Exception($"{kw} is not a valid keyword");
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
