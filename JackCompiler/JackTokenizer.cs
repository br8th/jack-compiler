using System.Text;

namespace JackCompiler
{
    public class JackTokenizer
    {
        public enum Token
        {
            KEYWORD = 0,
            SYMBOL = 1,
            IDENTIFIER = 2,
            INT_CONST = 3,
            STRING_CONST = 4,
        }

        public enum KeyWord
        {
            CLASS,
            METHOD,
            FUNCTION,
            CONSTRUCTOR,
            INT,
            BOOLEAN,
            CHAR,
            VOID,
            VAR,
            STATIC,
            FIELD,
            LET,
            DO,
            IF,
            ELSE,
            WHILE,
            RETURN,
            TRUE,
            FALSE,
            NULL,
            THIS,
        }

        internal static HashSet<string> keywords = new HashSet<string>
        {
             "class", "method", "function", "constructor", "int", "boolean", "char", "void", "var", "static", "field", "let", "do", "if", "else", "while", "return", "true", "false", "null", "this"
        };

        private HashSet<char> symbols = new HashSet<char> {
            '{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~'
        };

        public FileStream fs;
        public string currentToken;
        private long currentFsPosition;

        /* Opens the input file/stream and gets ready to tokenize it*/
        public JackTokenizer(string inputFilePath)
        {
            fs = new FileStream(inputFilePath, FileMode.Open, FileAccess.Read);
            currentFsPosition = 0;
            Advance(); // Move to initialize currentToken
        }

        /* Do we have more tokens in the input? */
        public bool HasMoreTokens()
        {
            return fs.Position < fs.Length;
        }

        /* Gets the next token from the input and makes it the current token */
        public void Advance()
        {
            if (!fs.CanSeek)
            {
                // https://www.youtube.com/watch?v=-Jc_ONhBC_E
                throw new Exception("Hold up, wait a minute. Something ain't right.");
            }


            fs.Seek(currentFsPosition, SeekOrigin.Begin);

            char c = (char) fs.ReadByte();

            // Skip \r, \n, whitespace
            if (Char.IsWhiteSpace(c) && HasMoreTokens())
            {
                int numSpaces = 0;

                while (Char.IsWhiteSpace(c))
                {
                    numSpaces++;
                    c = (char) fs.ReadByte();
                }

                currentFsPosition = fs.Position - 1;
                Advance();
                return;
            }

            // Handle comments
            if (c == '/')
            {
                var temp = (char) fs.ReadByte();

                // This is the symbol '/' followed by whitespace / a different symbol
                if (temp != '/' && temp != '*')
                {
                    currentToken = "/";
                    currentFsPosition = fs.Position;
                    return;
                }

                // Single line comments
                if (temp == '/')
                {
                    while (temp != '\n' && HasMoreTokens())
                    {
                        temp = (char) fs.ReadByte();
                    }

                    currentFsPosition = fs.Position - 1;
                    Advance();
                    return;
                }

                // Skip multi line comments
                if (temp == '*')
                {
                    keepSearching:

                    while (temp != '*' && HasMoreTokens())
                    {
                        temp = (char) fs.ReadByte();
                    }

                    temp = (char) fs.ReadByte();

                    if (temp != '/')
                    {
                        // TODO: Yes, yes, goto is terrible. This should be refactored into a separate method.
                        goto keepSearching;
                    } else
                    {
                        currentFsPosition = fs.Position;
                        Advance();
                    }

                    return;
                }

            }

            // Current token is symbol
            if (symbols.Contains(c))
            {
                currentToken = c.ToString();
                currentFsPosition = fs.Position;
                return;
            }

            byte[] buffer;

            // Current token is a string constant
            if (c == '"')
            {
                int strLength = 2;
                c = (char) fs.ReadByte();

                while (c  != '"')
                {
                    strLength++;
                    c = (char) fs.ReadByte();
                }

                buffer = new byte[strLength];
                fs.Seek(-(strLength), SeekOrigin.Current);
                fs.Read(buffer, 0, strLength);

                currentFsPosition = fs.Position;
                currentToken = Encoding.UTF8.GetString(buffer);
                return;
            }

            // keywords and identifiers ([class] || [_varName, var_name, var1])
            if (Char.IsLetter(c) ||  c == '_' || Char.IsDigit(c))
            {
                // Read until we find a space or symbol
                int strLength = 0;

                while (c != ' ' && !symbols.Contains(c))
                {
                    strLength++;
                    c = (char) fs.ReadByte();
                }

                buffer = new byte[strLength];
                fs.Seek(-(strLength + 1), SeekOrigin.Current);
                fs.Read(buffer, 0, strLength);

                currentFsPosition = fs.Position;
                currentToken = Encoding.UTF8.GetString(buffer);
                return;
            }

        }

        /* Returns the type of the current token */
        public Token TokenType()
        {
            if (currentToken.Length == 1 && symbols.Contains(currentToken.ToCharArray()[0]))
            {
                return Token.SYMBOL;
            }

            if (keywords.Contains(currentToken))
            {
                return Token.KEYWORD;
            }

            if (int.TryParse(currentToken, out _))
            {
                return Token.INT_CONST;
            }

            if (currentToken.StartsWith("\""))
            {
                return Token.STRING_CONST;
            }

            // TODO: Use regex to figure out if it's actually an identifier.
            // _var_name, varName1, 1invalidVarname, var__name1;
            // [A-Za-z_][A-Za-z0-9\_]

            return Token.IDENTIFIER;
        }

        /* Returns the keyword which is the current token.
         * Called only when current tokentype == Keyword */
        public string GetKeyWord()
        {
            return currentToken;
        }

        /* Returns the character which is the current token.
         * Called only when current tokentype == Symbol */
        public char Symbol()
        {
            return currentToken.ToCharArray()[0];
        }

        /* Returns the String which is the current token.
         * Called only when current tokentype == Identifier */
        public string Identifier()
        {
            return currentToken;
        }

        /* Returns the Integer which is the current token.
         * Called only when current tokentype == Int_Const */
        public int IntVal()
        {
            return Int32.Parse(currentToken);
        }

        /* Returns the String which is the current token.
         * Called only when current tokentype == String_Const */
        public string StringVal()
        {
            return currentToken.Replace("\"", "");
        }

    }
}
