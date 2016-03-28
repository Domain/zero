module zero.allocator;

import std.container.slist;

enum EVarScope
{
    Static, Global, Local
}

class Variable
{
    string name;
    EVarScope varScope;
    int offset;
    int depth;

    this(string name, EVarScope varScope, int offset, int depth)
    {
        this.name = name;
        this.varScope = varScope;
        this.offset = offset;
        this.depth = depth;
    }
}

struct Allocator
{
public:
    static Allocator opCall()
    {
        Allocator alloc;
        alloc.bp ~= 0;
        return alloc;
    }

    void EnterFunction()
    {
        bp ~= bp[$-1] + sp;
        sp = 0;
        ++depth;
    }

    void LeaveFunction()
    {
        --depth;
        sp = bp[$-1];
        --bp.length;
        sp -= bp[$-1];
    }

    void EnterScope()
    {
        fp.insertFront(sp);
        ++depth;
    }

    void LeaveScope()
    {
        --depth;
        sp = fp.front();
        fp.removeFront();
    }

    Variable AllocateVariable(string name, EVarScope varScope)
    {
        Variable result = null;
        final switch (varScope)
        {
            case EVarScope.Global:
            case EVarScope.Static:
                result = FindVariable(name);
                if (result !is null && result.varScope == varScope)
                    break;
                result = new Variable(name, varScope, globalSP, 0);
                globalStack[globalSP++] = result;
                break;

            case EVarScope.Local:
                result = new Variable(name, varScope, sp, bp.length-1);
                auto abs = bp[$-1]+sp;
                sp++;
                localStack[abs] = result;
                break;
        }
        return result;
    }

    Variable FindVariable(string name)
    {
        for (auto i = bp[$-1]+sp-1; i>= 0; i--)
        {
            if (localStack[i].name == name)
            {
                return localStack[i];
            }
        }

        for (auto i = globalSP-1; i>= 0; i--)
        {
            if (globalStack[i].name == name)
            {
                return globalStack[i];
            }
        }

        return null;
    }

private:
    static immutable StackSize = 128;

    Variable[StackSize] localStack;
    Variable[StackSize] globalStack;

    int[] bp;
    SList!int fp;

    int sp = 0;
    int globalSP = 0;
    int depth = 0;
}