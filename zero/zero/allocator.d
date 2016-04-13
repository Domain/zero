module zero.allocator;

import std.container.slist;
import std.array;
import std.format;
import msgpack;

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

enum RegisterType
{
    Local = 0, Global, Const, Literal
}

struct Register
{
    int offset;
    RegisterType type;
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

    string AllocateConst(T)(T value)
    {
        auto map = FindConst(value);
        if (map >= 0)
        {
            return format("%d(%d)", map, RegisterType.Const);
        }
        auto offset = constStack.data.length;
        constStack.put(pack(value));
        constCount++;
        AddConst(value, offset);
        return format("%d(%d)", offset, RegisterType.Const);
    }

    int FindConst(long value)
    {
        auto map = value in longMap;
        if (map !is null)
        {
            return *map;
        }
        return -1;
    }

    int FindConst(double value)
    {
        auto map = value in doubleMap;
        if (map !is null)
        {
            return *map;
        }
        return -1;
    }

    int FindConst(string value)
    {
        auto map = value in stringMap;
        if (map !is null)
        {
            return *map;
        }
        return -1;
    }

    void AddConst(long value, int offset)
    {
        longMap[value] = offset;
    }

    void AddConst(double value, int offset)
    {
        doubleMap[value] = offset;
    }

    void AddConst(string value, int offset)
    {
        stringMap[value] = offset;
    }

    string AllocateRegister()
    {
        return format("%d(%d)", freeReg++, RegisterType.Local);
    }

    string LastRegister()
    {
        return format("%d(%d)", freeReg, RegisterType.Local);
    }

    const(ubyte[]) ConstData() @property
    {
        return constStack.data;
    }

    int ConstCount() @property
    {
        return constCount;
    }

private:
    static immutable StackSize = 128;

    Variable[StackSize] localStack;
    Variable[StackSize] globalStack;
    Appender!(ubyte[]) constStack;
    int[long] longMap;
    int[double] doubleMap;
    int[string] stringMap;

    int[] bp;
    SList!int fp;

    int sp = 0;
    int globalSP = 0;
    int depth = 0;
    int constCount = 0;
    int freeReg = 0;
}