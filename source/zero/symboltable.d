module zero.symboltable;

import pegged.peg;

class Symbol
{
    string name;
    Position[] positions;
    SymbolTable table;
    int offset;
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
    int globalSP;
    int sp;
    int[] spStack;

    this()
    {
        push(); // global table
    }

    private SymbolTable push()
    {
        ++currentLevel;
        auto table = new SymbolTable(currentLevel);
        tables ~= table;
        return table;
    }

    private SymbolTable pop()
    {
        --currentLevel;
        auto table = tables[$ - 1];
        tables = tables[0 .. $ - 1];
        return table;
    }

    SymbolTable pushFunction()
    {
        auto table = push();
        sp = 0;
        return table;
    }

    SymbolTable popFunction()
    {
        return pop();
    }

    SymbolTable pushBlock()
    {
        spStack ~= sp;
        return push();
    }

    SymbolTable popBlock()
    {
        sp = spStack[$-1];
        spStack = spStack[0..$-1];
        return pop();
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
        auto symbol = localSymbolTable.enter(name);
        symbol.offset = sp++;
        return symbol;
    }

    Symbol enterGlobal(string name)
    {
        auto symbol = globalSymbolTable.enter(name);
        symbol.offset = globalSP++;
        return symbol;
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
