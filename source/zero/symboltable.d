module zero.symboltable;

import pegged.peg;

class Symbol
{
    string name;
    Position[] positions;
    SymbolTable table;
}

class SymbolTable
{
    private Symbol[string] symbols;
    private int level = 0;

    this(int nestingLv)
    {
        level = nestingLv;
    }

    Symbol enter(string name)
    {
        assert(lookup(name) is null);
        auto symbol = new Symbol();
        symbol.name = name;
        symbol.table = this;
        symbols[name] = symbol;
        return symbol;
    }

    Symbol lookup(string name)
    {
        auto symbol = name in symbols;
        if (symbol !is null)
            return *symbol;
        return null;
    }

    int nestingLevel() const @property @safe pure @nogc nothrow
    {
        return level;
    }
}

class SymbolTableStack
{
    private SymbolTable[] tables;

    this()
    {
        push(); // global table
    }

    SymbolTable push()
    {
        ++currentLevel;
        auto table = new SymbolTable(currentLevel);
        tables ~= table;
        return table;
    }

    SymbolTable pop()
    {
        --currentLevel;
        auto table = tables[$ - 1];
        tables = tables[0 .. $ - 1];
        return table;
    }

    SymbolTable localSymbolTable() @property
    {
        assert(currentLevel >= 0 && currentLevel < tables.length);
        return tables[currentLevel];
    }

    SymbolTable globalSymbolTable() @property
    {
        assert(currentLevel >= 0 && currentLevel < tables.length);
        return tables[0];
    }

    Symbol enterLocal(string name)
    {
        return localSymbolTable.enter(name);
    }

    Symbol enterGlobal(string name)
    {
        return globalSymbolTable.enter(name);
    }

    Symbol lookupLocal(string name)
    {
        return localSymbolTable.lookup(name);
    }

    Symbol lookup(string name)
    {
        Symbol symbol = null;
        for (auto i = currentLevel; i >= 0; i--)
        {
            symbol = tables[i].lookup(name);
            if (symbol !is null)
                break;
        }
        return symbol;
    }

    private int currentLevel = -1;
    int currentNestingLevel() const @property @safe pure @nogc nothrow
    {
        return currentLevel;
    }
}
