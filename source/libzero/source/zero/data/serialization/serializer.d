module zero.data.serialization.serializer;

import std.meta;
import std.traits;
import std.conv;
import std.format : format;
import std.uni : asCapitalized;
import std.string : toUpper;

import zero.data.serialization.archives.archive;
import zero.data.serialization.archives.stringArchive;
import zero.data.serialization.attributes;

private __gshared IArchive defaultArchive;

@property IArchive DefaultArchive()
{
    if (defaultArchive is null)
    {
        defaultArchive = new StringArchive();
    }
    return defaultArchive;
}

@property void DefaultArchive(IArchive archive)
{
    defaultArchive = archive;
}

struct Serializer
{
public:
    this(IArchive archive)
    {
        _archive = archive;
    }

    void serialize(U)(in U data, string name = null)
    {
        write(data, name);
    }

public:
    IArchive archive() @property { return _archive; }

private:
    static foreach (Type; SignType)
    {
        void write(T)(in T data, string name) if (is(T == Type))
        {
            mixin("_archive.write%s(data, name);".format(Type.stringof.asCapitalized()));
        }

        void writeEnum(in Type data, string name, string enumTypeName, string enumValueName)
        {
            mixin("_archive.writeEnum%s(data, name, enumTypeName, enumValueName);"
                  .format(Type.stringof.asCapitalized()));
        }
    }

    static foreach (Type; UnsignType)
    {
        void write(T)(in T data, string name) if (is(T == Type))
        {
            mixin("_archive.write%s(data, name);"
                  .format(toUpper(Type.stringof[0..2]) ~ Type.stringof[2..$]));
        }

        void writeEnum(in Type data, string name, string enumTypeName, string enumValueName)
        {
            mixin("_archive.writeEnum%s(data, name, enumTypeName, enumValueName);"
                  .format(toUpper(Type.stringof[0..2]) ~ Type.stringof[2..$]));
        }
    }

    void write(in string data, string name)
    {
        _archive.writeString(data, name);
    }

    void write(E)(in E data, string name) if (is(E == enum))
    {
        alias type = OriginalType!E;

        auto enumName = data.to!string();
        auto enumValue = cast(type)data;

        auto byName = getUDAs!(data, ByName);
        static if (byName.length > 0 && byName[0].name != "")
        {
            enumName = byName[0].name;
        }

        writeEnum(enumValue, name, E.stringof, enumName);
    }

    void write(T)(in T data, string name) if (isStaticArray!T)
    {
        static if (is(T U : U[]))
        {
            _archive.writeStaticArrayBegin(U.stringof, name, data.length);

            foreach (i, element; data)
            {
                this.serialize(element, i.to!string());
            }

            _archive.writeStaticArrayEnd(U.stringof, name, data.length);
        }
    }

    void write(T)(in T data, string name) if (isDynamicArray!T)
    {
        static if (is(T U : U[]))
        {
            _archive.writeDynamicArrayBegin(U.stringof, name, data.length);

            foreach (i, element; data)
            {
                this.serialize(element, i.to!string());
            }

            _archive.writeDynamicArrayEnd(U.stringof, name, data.length);
        }
    }

    void write(K, V)(in V[K] data, string name)
    {
        _archive.writeMapBegin(K.stringof, V.stringof, name, data.length);

        size_t i = 0;
        foreach (key, value; data)
        {
            this.serialize(key, name ~ "_key_" ~ i.to!string());
            this.serialize(value, name ~ "_value_" ~ i.to!string());
            i++;
        }

        _archive.writeMapEnd(K.stringof, V.stringof, name, data.length);
    }

    void write(T)(in T data, string name) 
        if (is(T == struct) || is(T == class) || is(T == union))
    {
        static if (is(T == struct))
            _archive.writeStructBegin(T.stringof, name);
        else static if (is(T == class))
            _archive.writeClassBegin(T.stringof, name);
        else
            _archive.writeUnionBegin(T.stringof, name);

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

            serialize(data.tupleof[i], memberName);
        }

        static if (is(T == struct))
            _archive.writeStructEnd(T.stringof, name);
        else static if (is(T == class))
            _archive.writeClassEnd(T.stringof, name);
        else
            _archive.writeUnionEnd(T.stringof, name);
    }

private:
    IArchive _archive;
}

auto serialize(T)(IArchive archive, T data, string name = null)
{
    auto serializer = Serializer(archive);
    serializer.serialize(data, name);
    return archive.data;
}

auto serialize(T)(T data, string name = null)
{
    auto result = serialize(DefaultArchive, data, name);
    defaultArchive.clear();
    return result;
}