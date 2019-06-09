module zero.data.serialization.deserializer;

import std.meta;
import std.traits;
import std.conv;
import std.format : format;
import std.uni : asCapitalized;
import std.string : toUpper;

import zero.data.serialization.archives.unarchive;
import zero.data.serialization.archives.stringUnarchive;
import zero.data.serialization.attributes;

private __gshared IUnarchive defaultUnarchive;

@property IUnarchive DefaultUnarchive()
{
    if (defaultUnarchive is null)
    {
        //defaultUnarchive = new StringUnarchive();
    }
    return defaultUnarchive;
}

@property void DefaultUnarchive(IUnarchive archive)
{
    defaultUnarchive = archive;
}

struct Deserializer
{
public:
    this(IUnarchive unarchive)
    {
        _unarchive = unarchive;
    }

    T deserialize(T)(immutable(void)[] data, string name = null)
    {
        _unarchive.data = data;
        return read!T(name);
    }

public:
    IUnarchive unarchive() @property { return _unarchive; }

private:
    T read(T)(string name)
    {
        static foreach (Type; SignType)
        {
            static if (is(T == Type))
            {
                mixin("return _unarchive.read%s(name);"
                      .format(Type.stringof.asCapitalized()));
            }
            else static if (is(T == enum) && is(OriginalType!T == Type))
            {
                mixin("return cast(T) _unarchive.readEnum%s(name, enumTypeName);"
                      .format(Type.stringof.asCapitalized()));
            }
        }

        static foreach (Type; UnsignType)
        {
            static if (is(T == Type))
            {
                mixin("return _unarchive.read%s(name);"
                      .format(toUpper(Type.stringof[0..2]) ~ Type.stringof[2..$]));            
            }
            else static if (is(T == enum) && is(OriginalType!T == Type))
            {
                mixin("return cast(T) _unarchive.readEnum%s(name, enumTypeName);"
                      .format(toUpper(Type.stringof[0..2]) ~ Type.stringof[2..$]));            
            }
        }

        static if (is(T == string))
        {
            return _unarchive.readString(name);
        }
        else static if (isStaticArray!T)
        {
            return readStaticArray!T(name);
        }
        else static if (isDynamicArray!T)
        {
            return readDynamicArray!T(name);
        }
        else static if (isAssociativeArray!T)
        {

        }
        else static if (is(T == class) || is(T == struct) || is(T == union))
        {

        }
    }

private:
    T readStaticArray(T)(string name)
    {
        T value;

        static if (is(T : U[n], U, size_t n))
        {
            _unarchive.readStaticArrayBegin(U.stringof, name, n);

            foreach (i, ref v; value)
            {
                v = read!U(i.to!string());
            }

            _unarchive.readStaticArrayEnd(U.stringof, name, n);
        }

        return value;
    }

    T readDynamicArray(T)(string name)
    {
        T value;

        _unarchive.readDynamicArrayBegin();

        return value;
    }

    void read(T)(in T data, string name) 
        if (is(T == struct) || is(T == class) || is(T == union))
        {
            static if (is(T == struct))
                _unarchive.readStructBegin(T.stringof, name);
            else static if (is(T == class))
                _unarchive.readClassBegin(T.stringof, name);
            else
                _unarchive.readUnionBegin(T.stringof, name);

            alias types = Fields!T;
            alias names = FieldNameTuple!T;
            foreach(i, type; types)
            {
                auto memberName = names[i];
                alias symbol = Alias!(__traits(getMember, data, names[i]));
                auto refer = &(__traits(getMember, data, names[i]));

                if (hasUDA!(symbol, NonSerialized))
                    continue;

                enum byName = getUDAs!(symbol, ByName);
                static if (byName.length > 0 && byName[0].name != "")
                {
                    memberName = byName[0].name;
                }

                deserialize(data.tupleof[i], memberName);
            }

            static if (is(T == struct))
                _unarchive.readStructEnd(T.stringof, name);
            else static if (is(T == class))
                _unarchive.readClassEnd(T.stringof, name);
            else
                _unarchive.readUnionEnd(T.stringof, name);
        }

private:
    IUnarchive _unarchive;
}

auto deserialize(T)(IUnarchive archive, immutable(void)[] data, string name = null)
{
    auto deserializer = Deserializer(archive);
    return deserializer.deserialize!T(data, name);
}

auto deserialize(T)(immutable(void)[] data, string name = null)
{
    auto result = serialize!T(DefaultUnarchive, data, name);
    defaultUnarchive.clear();
    return result;
}