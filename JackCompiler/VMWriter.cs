using System.Text;

namespace JackCompiler
{
    public class VMWriter
    {
        private StreamWriter writer;

        public enum Segment
        {
            CONSTANT,
            ARGUMENT,
            LOCAL,
            STATIC,
            THIS,
            THAT,
            POINTER,
            TEMP
        }

        // TODO: Document enum
        public enum Command
        {
            ADD,
            SUB,
            NEG, // Negate topmost value in stack
            EQ,
            GT,
            LT,
            AND,
            OR,
            NOT
        }

        // TODO: return singleton

        public VMWriter(string outputFile)
        {
            this.writer = new StreamWriter(outputFile);
        }

        // Writes a VM push command
        public void WritePush(Segment s, int index)
        {
            writer.WriteLine($"push {s.ToString().ToLower()} {index}");
        }

        // Writes a VM pop command
        public void WritePop(Segment s, int index)
        {
            writer.WriteLine($"pop {s.ToString().ToLower()} {index}");
        }
        public void WriteArithmetic(Command c)
        {
            // Unary operations
            if (c == Command.NEG || c == Command.NOT) {

            }

            writer.WriteLine($"{c.ToString().ToLower()}");
        }

        public void WriteLabel(String label)
        {
            writer.WriteLine($"label {label}");
        }

        public void WriteGoto(String label)
        {
            writer.WriteLine($"goto {label}");
        }

        // Writes a VM if-goto command
        public void WriteIf(String label)
        {
            writer.WriteLine($"if-goto {label}");
        }

        /// <summary>
        /// VM function call
        /// </summary>
        /// <param name="num">The full name of the function</param>
        /// <param name="ptr">The number of arguments that have already been pushed onto the stack</param>
        public void WriteCall(String funcName, int nArgs)
        {
            writer.WriteLine($"call {funcName} {nArgs}");
        }

        /// <summary>
        /// VM function definition
        /// </summary>
        /// <param name="num">The full name of the function</param>
        /// <param name="ptr">The number of local variables in the subroutine</param>
        public void WriteFunction(String name, int nLocals)
        {
            writer.WriteLine($"function {name} {nLocals}");
        }

        public void WriteReturn()
        {
            writer.WriteLine("return");
        }


        // Closes the output file
        public void Close()
        {
            writer.Dispose();
        }
    }
}
