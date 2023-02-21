namespace JackCompiler
{
    internal class SymbolTable
    {
        private Dictionary<string, int> hashTable1;
        private Dictionary<string, int> hashTable2;

        private static int hashtableCount = 0;

        private enum Kind
        {
            STATIC,
            FIELD,
            ARG,
            VAR,
            NONE
        }

        public SymbolTable() { }

        // Starts a new subroutine scope
        private void StartSubroutine()
        {

        }

        /* Defines a new identifier of a given
         * @param name, @param type, and @param kind and assigns it
         * a running index */
        private void Define(string name, string type, Kind kind)
        {
        }

        /* Returns the number of variables of the given kind already
         * defined in the given scope */
        private int VarCount(Kind k)
        {
            return -1;
        }

        /* Returns the kind of the named identifier in the current scope.
         * If the identifer is unknown in the current scope, it returns none */
        private Kind KindOf(string name)
        {
            return Kind.NONE;
        }

        // Returns the type of the named identifier in the current scope.
        private String TypeOf(string name)
        {
            return "";
        }

        // Returns the index assigned to the named identifier
        private int IndexOf(string name)
        {
            return -1;
        }


    }
}
