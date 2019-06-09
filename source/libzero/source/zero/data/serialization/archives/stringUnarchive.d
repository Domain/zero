module zero.data.serialization.archives.stringUnarchive;

import zero.data.serialization.archives.unarchive;
import std.format : format;
import std.string : toUpper;
import std.uni : asCapitalized;
import std.conv;
import std.array;

class StringUnarchive// : IUnarchive
{
//    auto buffer = appender!string;
//
//    static foreach (T; SignType)
//    {
//        mixin(`
//              %2$s read%1$s(string name)
//              {
//              readIndent();
//              buffer ~= name;
//              buffer ~= "=";
//              buffer ~= data.to!string();
//              buffer ~= ";\n";
//              }
//
//              %2$s readEnum%1$s(string name, string enumTypeName, string enumValueName)
//              {
//              readIndent();
//              buffer ~= name;
//              buffer ~= "=";
//              buffer ~= enumTypeName;
//              buffer ~= ".";
//              buffer ~= enumValueName;
//              buffer ~= ";\n";
//              }`
//              .format(T.stringof.asCapitalized, T.stringof)
//              );
//    }
//
//    static foreach (T; UnsignType)
//    {
//        mixin(`
//              void read%1$s(%2$s data, string name)
//              {
//              readIndent();
//              buffer ~= name;
//              buffer ~= "=";
//              buffer ~= data.to!string();
//              buffer ~= ";\n";
//              }
//
//              void readEnum%1$s(%2$s data, string name, string enumTypeName, string enumValueName)
//              {
//              readIndent();
//              buffer ~= name;
//              buffer ~= "=";
//              buffer ~= enumTypeName;
//              buffer ~= ".";
//              buffer ~= enumValueName;
//              buffer ~= ";\n";
//              }`.format(toUpper(T.stringof[0..2]) ~ T.stringof[2..$], T.stringof)
//              );
//    }
//
//    void readString(string data, string name)
//    {
//        readIndent();
//        buffer ~= name;
//        buffer ~= "=";
//        buffer ~= data;
//        buffer ~= ";\n";
//    }
//
//    void readStaticArrayBegin(string typeName, string varName, size_t length)
//    {
//        readIndent();
//        buffer ~= varName;
//        buffer ~= "=";
//        buffer ~= typeName;
//        buffer ~= "[";
//        buffer ~= to!string(length);
//        buffer ~= "] ";
//        buffer ~= "[\n";
//        indent++;
//    }
//
//    void readStaticArrayEnd(string typeName, string varName, size_t length)
//    {
//        indent--;
//        readIndent();
//        buffer ~= "]\n";
//    }
//
//    void readDynamicArrayBegin(string typeName, string varName, size_t length)
//    {
//        readIndent();
//        buffer ~= varName;
//        buffer ~= "=";
//        buffer ~= typeName;
//        buffer ~= "[] ";
//        buffer ~= "[\n";
//        indent++;
//    }
//
//    void readDynamicArrayEnd(string typeName, string varName, size_t length)
//    {
//        indent--;
//        readIndent();
//        buffer ~= "]\n";
//    }
//
//    void readMapBegin(string keyName, string valueName, string varName, size_t length)
//    {
//        readIndent();
//        buffer ~= varName;
//        buffer ~= "=";
//        buffer ~= valueName;
//        buffer ~= "[";
//        buffer ~= keyName;
//        buffer ~= "]{\n";
//        indent++;
//    }
//
//    void readMapEnd(string keyName, string valueName, string varName, size_t length)
//    {
//        indent--;
//        readIndent();
//        buffer ~= "}\n";
//    }
//
//    void readStructBegin(string typeName, string varName)
//    {
//        readIndent();
//        buffer ~= varName;
//        buffer ~= "=";
//        buffer ~= typeName;
//        buffer ~= "{\n";
//        indent++;
//    }
//
//    void readStructEnd(string typeName, string varName)
//    {
//        indent--;
//        readIndent();
//        buffer ~= "}\n";
//    }
//
//    void readClassBegin(string typeName, string varName)
//    {
//        readIndent();
//        buffer ~= varName;
//        buffer ~= "=";
//        buffer ~= typeName;
//        buffer ~= "{\n";
//        indent++;
//    }
//
//    void readClassEnd(string typeName, string varName)
//    {
//        indent--;
//        readIndent();
//        buffer ~= "}\n";
//    }
//
//    void readUnionBegin(string typeName, string varName)
//    {
//        readIndent();
//        buffer ~= varName;
//        buffer ~= "=";
//        buffer ~= typeName;
//        buffer ~= "{\n";
//        indent++;
//    }
//
//    void readUnionEnd(string typeName, string varName)
//    {
//        indent--;
//        readIndent();
//        buffer ~= "}\n";
//    }
//
//    void clear()
//    {
//        buffer = appender!string();
//    }
//
//    immutable(void)[] data() @property 
//    {
//        return buffer.data;
//    }
//
//    void data(immutable(void)[] value) @property 
//    {
//        buffer = appender(cast(string)value);
//    }
//
//private:
//    void readIndent()
//    {
//        buffer ~= format("%*s", indent * 4, "");
//    }
//
//private:
//    int indent = 0;
}