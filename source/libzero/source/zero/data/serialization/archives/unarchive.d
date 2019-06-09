module zero.data.serialization.archives.unarchive;

import std.meta;
import std.format;
import std.string : toUpper;
import std.uni : asCapitalized;

alias SignType = AliasSeq!(bool, byte, short, int, long, float, double, char);
alias UnsignType = AliasSeq!(ubyte, ushort, uint, ulong, wchar, dchar);
alias BasicType = AliasSeq!(SignType, UnsignType, string);

interface IUnarchive
{
    static foreach (Type; SignType)
    {
        mixin(`
        %2$s read%1$s(string name);
        %2$s readEnum%1$s(string name);
        `.format(Type.stringof.asCapitalized, Type.stringof));
    }

    static foreach (Type; UnsignType)
    {
        mixin(`
        %2$s read%1$s(string name);
        %2$s readEnum%1$s(string name);
        `.format(toUpper(Type.stringof[0..2]) ~ Type.stringof[2..$], Type.stringof));
    }

    string readString(string name);

    void readStaticArrayBegin(string typeName, string varName, size_t length);
    void readStaticArrayEnd(string typeName, string varName, size_t length);

    void readDynamicArrayBegin(string typeName, string varName, size_t length);
    void readDynamicArrayEnd(string typeName, string varName, size_t length);

    void readMapBegin(string keyName, string valueName, string varName, size_t length);
    void readMapEnd(string keyName, string valueName, string varName, size_t length);

    void readStructBegin(string typeName, string varName);
    void readStructEnd(string typeName, string varName);

    void readClassBegin(string typeName, string varName);
    void readClassEnd(string typeName, string varName);

    void readUnionBegin(string typeName, string varName);
    void readUnionEnd(string typeName, string varName);

    void clear();

    immutable(void)[] data() @property;
    void data(immutable(void)[] value) @property;
}