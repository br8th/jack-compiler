namespace JackCompiler
{
    internal class JackCompiler
    {
        static void Main(string[] args)
        {
            string filePath = args[0];
            bool isDirectory = File.GetAttributes(filePath).HasFlag(FileAttributes.Directory);

            string[] filesInDir = { filePath };

            if (isDirectory)
            {
                filesInDir = Directory.GetFiles(filePath, "*.jack");
            }

            foreach (string fileName in filesInDir)
            {
                JackTokenizer tokenizer = new JackTokenizer(fileName);
                CompilationEngine engine = new CompilationEngine(tokenizer, fileName.Replace(".jack", ".xml"));
                engine.CompileClass();
                engine.ShutDown();
            }
        }
    }
}
