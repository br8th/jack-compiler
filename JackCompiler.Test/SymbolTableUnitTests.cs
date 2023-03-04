namespace JackCompiler.Test
{
    [TestClass]
    public class SymbolTableUnitTests
    {
        // Assigns indexes based on kind
        // Scopes correctly (routine first, then class level)
        // Throws an exception if already defined
        // Checks if a variable is defined

        private static SymbolTable table;

        [TestInitialize]
        public void TestInitialize()
        {
            table = new SymbolTable();
        }

        [TestMethod]
        public void ReturnsCorrectVarCount()
        {
            table.Define("a", "int", SymbolTable.Kind.STATIC);
            table.Define("b", "int", SymbolTable.Kind.STATIC);

            table.Define("c", "int", SymbolTable.Kind.FIELD);

            table.Define("d", "int", SymbolTable.Kind.VAR);
            table.Define("e", "int", SymbolTable.Kind.VAR);

            table.Define("f", "int", SymbolTable.Kind.ARG);

            Assert.AreEqual(2, table.VarCount(SymbolTable.Kind.STATIC));
            Assert.AreEqual(1, table.VarCount(SymbolTable.Kind.FIELD));
            Assert.AreEqual(2, table.VarCount(SymbolTable.Kind.VAR));
            Assert.AreEqual(1, table.VarCount(SymbolTable.Kind.ARG));
        }

        [TestMethod]
        public void ChecksIfVariableDefined()
        {
            table.Define("a", "int", SymbolTable.Kind.STATIC);
            Assert.IsTrue(table.IsIdentifierDefined("a"));
            Assert.IsFalse(table.IsIdentifierDefined("b"));
        }
    }
}
