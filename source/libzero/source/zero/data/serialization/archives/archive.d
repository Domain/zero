module zero.data.serialization.archives.archive;

import std.meta;
import std.format;
import std.string : toUpper;
import std.uni : asCapitalized;

alias SignType = AliasSeq!(bool, byte, short, int, long, float, double, char);
alias UnsignType = AliasSeq!(ubyte, ushort, uint, ulong, wchar, dchar);
alias BasicType = AliasSeq!(SignType, UnsignType, string);

interface IArchive
{
    static foreach (Type; SignType)
    {
        mixin(`
        void write%1$s(%2$s data, string name);
        void writeEnum%1$s(%2$s data, string name, string enumTypeName, string enumValueName);
        `.format(Type.stringof.asCapitalized, Type.stringof));
    }

    static foreach (Type; UnsignType)
    {
        mixin(`
        void write%1$s(%2$s data, string name);
        void writeEnum%1$s(%2$s data, string name, string enumTypeName, string enumValueName);
        `.format(toUpper(Type.stringof[0..2]) ~ Type.stringof[2..$], Type.stringof));
    }

    void writeString(string data, string name);

    void writeStaticArrayBegin(string typeName, string varName, size_t length);
    void writeStaticArrayEnd(string typeName, string varName, size_t length);

    void writeDynamicArrayBegin(string typeName, string varName, size_t length);
    void writeDynamicArrayEnd(string typeName, string varName, size_t length);

    void writeMapBegin(string keyName, string valueName, string varName, size_t length);
    void writeMapEnd(string keyName, string valueName, string varName, size_t length);

    void writeStructBegin(string typeName, string varName);
    void writeStructEnd(string typeName, string varName);

    void writeClassBegin(string typeName, string varName);
    void writeClassEnd(string typeName, string varName);

    void writeUnionBegin(string typeName, string varName);
    void writeUnionEnd(string typeName, string varName);

    void clear();

    immutable(void)[] data() @property;
    void data(immutable(void)[] value) @property;
}