module zero.data.serialization.archives.stringArchive;

import zero.data.serialization.archives.archive;
import std.format : format;
import std.string : toUpper;
import std.uni : asCapitalized;
import std.conv;
import std.array;

class StringArchive : IArchive
{
    auto buffer = appender!string;

    static foreach (T; SignType)
    {
        mixin(`
              void write%1$s(%2$s data, string name)
              {
              writeIndent();
              buffer ~= name;
              buffer ~= "=";
              buffer ~= data.to!string();
              buffer ~= ";\n";
              }

              void writeEnum%1$s(%2$s data, string name, string enumTypeName, string enumValueName)
              {
              writeIndent();
              buffer ~= name;
              buffer ~= "=";
              buffer ~= enumTypeName;
              buffer ~= ".";
              buffer ~= enumValueName;
              buffer ~= ";\n";
              }`
              .format(T.stringof.asCapitalized, T.stringof)
              );
    }

    static foreach (T; UnsignType)
    {
        mixin(`
              void write%1$s(%2$s data, string name)
              {
              writeIndent();
              buffer ~= name;
              buffer ~= "=";
              buffer ~= data.to!string();
              buffer ~= ";\n";
              }

              void writeEnum%1$s(%2$s data, string name, string enumTypeName, string enumValueName)
              {
              writeIndent();
              buffer ~= name;
              buffer ~= "=";
              buffer ~= enumTypeName;
              buffer ~= ".";
              buffer ~= enumValueName;
              buffer ~= ";\n";
              }`.format(toUpper(T.stringof[0..2]) ~ T.stringof[2..$], T.stringof)
              );
    }

    void writeString(string data, string name)
    {
        writeIndent();
        buffer ~= name;
        buffer ~= "=";
        buffer ~= data;
        buffer ~= ";\n";
    }

    void writeStaticArrayBegin(string typeName, string varName, size_t length)
    {
        writeIndent();
        buffer ~= varName;
        buffer ~= "=";
        buffer ~= typeName;
        buffer ~= "[";
        buffer ~= to!string(length);
        buffer ~= "] ";
        buffer ~= "[\n";
        indent++;
    }

    void writeStaticArrayEnd(string typeName, string varName, size_t length)
    {
        indent--;
        writeIndent();
        buffer ~= "]\n";
    }

    void writeDynamicArrayBegin(string typeName, string varName, size_t length)
    {
        writeIndent();
        buffer ~= varName;
        buffer ~= "=";
        buffer ~= typeName;
        buffer ~= "[] ";
        buffer ~= "[\n";
        indent++;
    }

    void writeDynamicArrayEnd(string typeName, string varName, size_t length)
    {
        indent--;
        writeIndent();
        buffer ~= "]\n";
    }

    void writeMapBegin(string keyName, string valueName, string varName, size_t length)
    {
        writeIndent();
        buffer ~= varName;
        buffer ~= "=";
        buffer ~= valueName;
        buffer ~= "[";
        buffer ~= keyName;
        buffer ~= "]{\n";
        indent++;
    }

    void writeMapEnd(string keyName, string valueName, string varName, size_t length)
    {
        indent--;
        writeIndent();
        buffer ~= "}\n";
    }

    void writeStructBegin(string typeName, string varName)
    {
        writeIndent();
        buffer ~= varName;
        buffer ~= "=";
        buffer ~= typeName;
        buffer ~= "{\n";
        indent++;
    }

    void writeStructEnd(string typeName, string varName)
    {
        indent--;
        writeIndent();
        buffer ~= "}\n";
    }

    void writeClassBegin(string typeName, string varName)
    {
        writeIndent();
        buffer ~= varName;
        buffer ~= "=";
        buffer ~= typeName;
        buffer ~= "{\n";
        indent++;
    }

    void writeClassEnd(string typeName, string varName)
    {
        indent--;
        writeIndent();
        buffer ~= "}\n";
    }

    void writeUnionBegin(string typeName, string varName)
    {
        writeIndent();
        buffer ~= varName;
        buffer ~= "=";
        buffer ~= typeName;
        buffer ~= "{\n";
        indent++;
    }

    void writeUnionEnd(string typeName, string varName)
    {
        indent--;
        writeIndent();
        buffer ~= "}\n";
    }

    void clear()
    {
        buffer = appender!string();
    }

    immutable(void)[] data() @property 
    {
        return buffer.data;
    }

    void data(immutable(void)[] value) @property 
    {
        buffer = appender(cast(string)value);
    }

private:
    void writeIndent()
    {
        buffer ~= format("%*s", indent * 4, "");
    }

private:
    int indent = 0;
}