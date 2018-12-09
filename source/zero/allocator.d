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
    size_t depth;

    this(string name, EVarScope varScope, int offset, size_t depth)
    {
        this.name = name;
        this.varScope = varScope;
        this.offset = offset;
        this.depth = depth;
    }

    override string toString()
    {
        if (varScope == EVarScope.Local)
        {
            return format("%d(%s)", offset, RegisterType.Local);
        }

        return format("%d(%s)", offset, RegisterType.Global);
    }

    @property Register toRegister() const
    {
        Register r;
        r.offset = offset;
        if (varScope == EVarScope.Local)
            r.type = RegisterType.Local;
        else
            r.type = RegisterType.Global;
        return r;
    }

    alias toRegister this;

    bool opEquals()(auto ref const Register rhs) const
    {
        return toRegister() == rhs;
    }
}

enum RegisterType
{
    Local = 0, Global, Const, Literal, Count
}

struct Register
{
    int offset = 0;
    RegisterType type = RegisterType.Count;

    static Register Invalid = {0, RegisterType.Count};

    string toString()
    {
        return format("%d(%s)", offset, type);
    }

    bool opEquals()(auto ref const Register rhs) const
    {
        return offset == rhs.offset && type == rhs.type;
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
                tempCount = 0;
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

    Register AllocateLiteral(T)(T value)
    {
        auto map = FindLiteral(value);
        if (map >= 0)
        {
            return Register(map, RegisterType.Const);
        }
        int offset = cast(int)constStack.data.length;
        constStack.put(pack(value));
        constCount++;
        AddLiteral(value, offset);
        return Register(offset, RegisterType.Const);
    }

    int FindLiteral(long value)
    {
        auto map = value in longMap;
        if (map !is null)
        {
            return *map;
        }
        return -1;
    }

    int FindLiteral(double value)
    {
        auto map = value in doubleMap;
        if (map !is null)
        {
            return *map;
        }
        return -1;
    }

    int FindLiteral(string value)
    {
        auto map = value in stringMap;
        if (map !is null)
        {
            return *map;
        }
        return -1;
    }

    void AddLiteral(long value, int offset)
    {
        longMap[value] = offset;
    }

    void AddLiteral(double value, int offset)
    {
        doubleMap[value] = offset;
    }

    void AddLiteral(string value, int offset)
    {
        stringMap[value] = offset;
    }

    Register AllocateRegister()
    {
        return Register(sp + tempCount++, RegisterType.Local);
    }

    const(ubyte[]) ConstData() @property
    {
        return constStack.data;
    }

    int ConstCount() @property
    {
        return constCount;
    }

    bool AllocateFunction(string name, int loc)
    {
        auto f = name in functionMap;
        if (f !is null)
            return false;
        functionMap[name] = loc;
        return true;
    }

    int FindFunction(string name)
    {
        auto f = name in functionMap;
        if (f !is null)
            return *f;
        return functionMap[name];
    }

private:
    static immutable StackSize = 128;

    Variable[StackSize] localStack;
    Variable[StackSize] globalStack;
    Appender!(ubyte[]) constStack;
    int[long] longMap;
    int[double] doubleMap;
    int[string] stringMap;
    int[string] functionMap;

    int[] bp;
    SList!int fp;

    int sp = 0;
    int globalSP = 0;
    int depth = 0;
    int constCount = 0;
    int tempCount = 0;
}
