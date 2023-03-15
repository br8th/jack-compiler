using static JackCompiler.SymbolTable;
using static JackCompiler.VMWriter;

namespace JackCompiler
{
    public static class EnumExtensions
    {
        /// <summary>
        /// Helper method that maps a SymbolTable Kind to a VM segment
        /// </summary>
        /// <param name="str">SymbolTable kind</param>
        /// <returns>VM Segment</returns>
        public static VMWriter.Segment GetVMSegment(this Kind kind)
        {
            return kind switch
            {
                Kind.VAR => Segment.LOCAL,
                Kind.FIELD => Segment.THIS,
                Kind.STATIC => Segment.STATIC,
                Kind.ARG => Segment.ARGUMENT,
                _ => throw new NotSupportedException(),
            };
        }
    }

    public class SymbolTable
    {
        public class SymbolTableRecord
        {
            public SymbolTableRecord(int index, string type, Kind kind)
            {
                Index = index;
                Type = type;
                Kind = kind;
            }

            public int Index { get; set; }

            // built in types (int, char, boolean) or custom type (Point, Dog)
            public string Type { get; set; }

            public Kind Kind { get; set; }
        }

        private Dictionary<string, SymbolTableRecord> classTable = new Dictionary<string, SymbolTableRecord>();
        private Dictionary<string, SymbolTableRecord> subroutineTable = new Dictionary<string, SymbolTableRecord>();

        private static int staticCounter = 0, fieldCounter = 0, argCounter = 0, varCounter = 0;

        // var, argument, static, field, class, or subroutine.
        public enum Kind
        {
            STATIC,
            FIELD,
            ARG,
            VAR,

            // TODO: Not used
            CLASS,
            SUBROUTINE,
            //NONE
        }

        public SymbolTable() { }

        // Starts a new subroutine scope
        public void StartSubroutine()
        {
            varCounter = 0; argCounter = 0; fieldCounter = 0;
            subroutineTable.Clear();
        }

        /* Defines a new identifier of a given
         * @param name, @param type, and @param kind and assigns it
         * a running index */
        public void Define(string name, string type, Kind kind)
        {
            switch (kind)
            {
                case Kind.STATIC:
                    if (classTable.ContainsKey(name))
                    {
                        throw new ArgumentException($"identifier {name} already defined.");
                    }
                    classTable.Add(name, new SymbolTableRecord(staticCounter++, type, kind));
                    break;
                case Kind.FIELD:
                    if (classTable.ContainsKey(name))
                    {
                        throw new ArgumentException($"identifier {name} already defined.");
                    }
                    classTable.Add(name, new SymbolTableRecord(fieldCounter++, type, kind));
                    break;
                case Kind.ARG:
                    if (subroutineTable.ContainsKey(name))
                    {
                        throw new ArgumentException($"identifier {name} already defined.");
                    }
                    subroutineTable.Add(name, new SymbolTableRecord(argCounter++, type, kind));
                    break;
                case Kind.VAR:
                    if (subroutineTable.ContainsKey(name))
                    {
                        throw new ArgumentException($"identifier {name} already defined.");
                    }
                    subroutineTable.Add(name, new SymbolTableRecord(varCounter++, type, kind));
                    break;
                default:
                    throw new ArgumentException($"Invalid kind {kind} supplied.");
            }
        }

        /* Returns the number of variables of the given kind already
         * defined in the given scope */
        public int VarCount(Kind k)
        {
            if (k == Kind.STATIC || k == Kind.FIELD)
                return classTable.Where((record) => record.Value.Kind == k).Count();
            if (k == Kind.VAR || k == Kind.ARG)
                return subroutineTable.Where((record) => record.Value.Kind == k).Count();

            throw new ArgumentException("Invalid kind arg supplied.");
        }

        /* Returns the kind of the named identifier in the current scope.
         * If the identifer is unknown in the current scope, it returns none */
        public Kind KindOf(string name)
        {
            var record = FindRecordOrThrow(name);
            return record.Kind;
        }

        // Returns the type of the named identifier in the current scope.
        public string TypeOf(string name)
        {
            var record = FindRecordOrThrow(name);
            return record.Type;
        }

        // Returns the index assigned to the named identifier
        public int IndexOf(string name)
        {
            var record = FindRecordOrThrow(name);
            return record.Index;
        }

        // Helper method
        private SymbolTableRecord FindRecordOrThrow(string key)
        {
            SymbolTableRecord record;

            bool isIdentifierDefined = subroutineTable.TryGetValue(key, out record);

            if (!isIdentifierDefined)
            {
                isIdentifierDefined = classTable.TryGetValue(key, out record);

                if (!isIdentifierDefined)
                {
                    throw new Exception($"identifier {key} is not defined in either the subroutine/class level symbol tables.");
                }
            }

            return record!;
        }

        public bool IsIdentifierDefined(string name)
        {
            if (subroutineTable.ContainsKey(name))
            {
                return true;
            }

            return classTable.ContainsKey(name);
        }
    }
}
