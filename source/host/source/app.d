import zero.logger;
import zero.plugins.hostingengine;
import zero.system.event;

import std.path : dirName, buildPath;
import std.file : thisExePath;
import std.stdio;

int main(string[] args)
{
    version(Windows)
    {
        import core.sys.windows.windows;
        if (SetConsoleCP(65001) == 0)
            throw new Exception("failure");
        if (SetConsoleOutputCP(65001) == 0)
            throw new Exception("failure");
    }

    HostingEngine host = HostingEngine(buildPath(thisExePath.dirName, "plugins"));
    host.run(args);

    trace("finish");

    import zero.data.serialization.serializer;
    import zero.data.serialization.archives.stringArchive;
    import zero.data.serialization.attributes;

    enum e { Monday, Sunday }
    struct S
    {
        int a = 1;
        string b = "abcdef";
        @ByName("ENUM") e today = e.Monday;
        int[] array = [123, 456];
        int[2] array2 = [1234, 4567];
        string[string] dict;
    }
    auto data = serialize(1234, "data1");
    auto data2 = serialize(1234UL, "data2");
    auto s = S();
    s.dict["abc"] = "123";
    auto data3 = serialize(s, "data3");

    tracef("Data: %s", cast(string)data);
    tracef("Data2: %s", cast(string)data2);
    tracef("Data3: %s", cast(string)data3);

    readln();

    return 0;
}