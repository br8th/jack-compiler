using System.Text.RegularExpressions;
using static JackCompiler.VMWriter;

namespace JackCompiler
{
    // Misc extensions
    public static class Utils
    {
        private static string pattern = @"^[a-zA-Z0-9\\_]+$";

        private static Regex regex = new Regex(pattern, RegexOptions.Compiled);

        private static readonly HashSet<string> builtInTypes = new HashSet<string>
        {
            "int", "char", "boolean"
        };

        private static HashSet<char> ops = new HashSet<char>
        {
            '+', '-', '*', '/', '&', '|', '<', '>', '='
        };

        private static HashSet<char> unaryOps = new HashSet<char>
        {
            '~', '-' // not, negation e.g. ~(1 > 2),  -1
        };

        // these keywords can be assigned to a variable e.g foo = true;
        private static HashSet<string> kwConstants = new HashSet<string>
        {
            "true", "false", "null", "this"
        };

        public static Command GetCommandFromOperand(this char operand, bool isUnary = false)
        {
            switch (operand)
            {
                case '+':
                    return Command.ADD;
                case '-':
                    return isUnary ? Command.NEG : Command.SUB;
                case '=':
                    return Command.EQ;
                case '>':
                    return Command.GT;
                case '<':
                    return Command.LT;
                case '&':
                    return Command.AND;
                case '|':
                    return Command.OR;
                case '~':
                    return Command.NOT;
                default:
                    throw new Exception($"Invalid operand {operand} encountered.");
            }
        }
        public static bool IsUnaryOp(this char c)
        {
            return unaryOps.Contains(c);
        }

        public static bool IsValidIdentifier(this string identifier)
        {
            if (String.IsNullOrWhiteSpace(identifier))
            {
                return false;
            }

            // Should not start with a digit
            if (Char.IsDigit(identifier.ToCharArray()[0]))
            {
                return false;
            }

            if (JackTokenizer.keywords.Contains(identifier))
            {
                return false;
            }

            return regex.IsMatch(identifier);
        }

        public static bool IsValidType(this string type, out bool isCustomType)
        {
            isCustomType = false;

            if (String.IsNullOrWhiteSpace(type))
            {
                return false;
            }

            if (builtInTypes.Contains(type))
            {
                isCustomType = false;
                return true;
            }

            bool isValidIdentifier = IsValidIdentifier(type);

            // custom types e.g Rectangle, Animal
            if (isValidIdentifier)
            {
                isCustomType = true;
            }

            return isValidIdentifier;
        }

        public static bool IsValidOp(this char op)
        {
            return ops.Contains(op);
        }

        public static bool IsKeywordConstant(this string kw)
        {
            return kwConstants.Contains(kw);
        }
    }
}
