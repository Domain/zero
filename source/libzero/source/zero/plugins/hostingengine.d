module zero.plugins.hostingengine;

import zero.logger;
import zero.plugins.iplugin;
import zero.plugins.metadata;
import zero.plugins.descriptor;
import std.algorithm.sorting : sort;
import std.algorithm.iteration : each, filter;
import std.array : array;
import std.datetime.stopwatch;

struct HostingEngine
{
    private string path;
    private bool _troubleshootingMode = false;
    private bool running = false;
    private StopWatch watch;

    bool troubleshootingMode() @property
    {
        return _troubleshootingMode;
    }

    this(string path)
    {
        tracef("PlugIn path: %s", path);
        this.path = path;
    }

    void run(string[] args)
    {
        tracef("Running...");

        auto plugins = searchForPlugIns(path);

        plugins.each!((ref p) => p.load());
        plugins.each!((ref p) { 
                p.findMissingDependencies(plugins); 
                p.findCircularlyDependencies(plugins);
                p.markHaveDependencyThatIsCircularlyDependent(plugins); 
                p.markHaveDependencyThatIsMissingDependency(plugins); 
            });

        trace("Sorting plugins");
        plugins = plugins.sort!((a, b) => b.metaData.dependsOn(a.metaData)).array;
        tracef("Plugins sorted: %s", plugins);
        plugins.each!((ref p) => p.start(cast(void*)&this, &control));
        plugins.filter!(p => !p.isStarted).each!((ref p) => p.unload());
        
        auto startedPlugins = plugins.filter!(p => p.isStarted).array;

        running = true;

        watch = StopWatch(AutoStart.yes);
        while (running)
        {
            startedPlugins.each!((ref p) => p.update(watch.peek().total!"msecs"()));
        }

        import std.algorithm.mutation : reverse;

        startedPlugins.reverse();
        startedPlugins.each!((ref p) => p.stop());
        startedPlugins.each!((ref p) => p.unload());
    }
}

Descriptor[] searchForPlugIns(string path)
{
    import std.path : buildPath, extension;
    import std.stdio : File, KeepTerminator;
    import std.file : exists, dirEntries, SpanMode;
    import std.algorithm.iteration : map, uniq;
    import std.algorithm.searching : canFind;
    import std.ascii : newline;
    import zero.bootstrap.versioningbootstrap;

    version(Windows)
        auto ext = ".dll";
    else
        auto ext = ".so";

    auto allPlugins = dirEntries(path, SpanMode.shallow).filter!(f => f.isDir || f.name.extension == ext)
        .map!(f => f.toDescriptor()).array;

    auto plugins = allPlugins;

    auto allowFilename = buildPath(path, "plugins.allow");
    auto denyFilename = buildPath(path, "plugins.deny");

    if (allowFilename.exists)
    {
        auto f = File(allowFilename);
        auto allowed = f.byLineCopy!string(KeepTerminator.no, newline).array;
        plugins = allPlugins.filter!(a => allowed.canFind!((name, plugin) => plugin.name == name)(a)).array;
    }
    else if (denyFilename.exists)
    {
        auto f = File(denyFilename);
        auto denied = f.byLineCopy!string(KeepTerminator.no, newline).array;
        plugins = allPlugins.filter!(a => !denied.canFind!((name, plugin) => plugin.name == name)(a)).array;
    }

    tracef("Plugins found: %s", plugins);

    return plugins;
}

extern(C) int control(void* handle, int opcode, long param1, void* param2, void* paramv)
{
    tracef("handle %s, op %s, p1 %s, p2 %s", handle, opcode, param1, param2);
    auto hosting = cast(HostingEngine*)handle;
    if (param1 % 10 == 1 || opcode % 10 == 2)
        hosting.running = false;

    switch (opcode)
    {
        case 101:
            sharedLog = param2 == null ? null : *cast(Logger*)param2;
            break;

        case 102:
            auto logger = sharedLog;
            *cast(Logger**)paramv = &logger;
            break;

        default:
            break;
    }
    return 0;
}
