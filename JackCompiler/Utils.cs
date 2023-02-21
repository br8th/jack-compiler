using System.Text.RegularExpressions;

namespace JackCompiler
{
    // Misc extensions
    public static class Utils
    {
        private static string pattern = @"^[a-zA-Z0-9\\_]+$";

        private static Regex regex = new Regex(pattern, RegexOptions.Compiled);

        private static HashSet<string> validTypes = new HashSet<string>
        {
            "int", "char", "boolean"
        };

        private static HashSet<string> operands = new HashSet<string>
        {
            "+", "-", "*", "/", "&", "|", "<", ">", "="
        };

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

            if (validTypes.Contains(type))
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

        public static bool IsValidOperand(this string op)
        {
            if (String.IsNullOrWhiteSpace(op))
            {
                return false;
            }

            return operands.Contains(op);
        }
    }
}
