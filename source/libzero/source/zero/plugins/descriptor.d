module zero.plugins.descriptor;

import zero.version_;
import zero.plugins.iplugin;
import zero.plugins.metadata;
import zero.plugins.dependency;
import zero.logger;

import std.typecons : Flag;
import std.datetime : DateTime, Clock;
import std.string : fromStringz;
import std.file : DirEntry;
import core.runtime;

version (Windows) import core.sys.windows.windows;

struct Descriptor
{
    MetaData metaData;
    string name;
    string fullPath;
    string path;
    void* dllHandle;

    Dependency[] missingDependencies;
    Dependency[] circularlyDependencies;
    bool isMissingDependency() @property { return missingDependencies !is null && missingDependencies.length > 0; }
    bool isCircularlyDependent() @property { return circularlyDependencies !is null && circularlyDependencies.length > 0; }
    bool isDependentOnTypeThatIsCircularlyDependent = false;
    bool isDependentOnTypeThatIsMissingDependency = false;
    bool isStarted = false;
    bool isUninstalled = true;
    DateTime installDate;
    int runCount = 0;

    string toString()
    {
        return name;
    }
}

Descriptor toDescriptor(string fullPath)
{
    return toDescriptor(DirEntry(fullPath));
}

Descriptor toDescriptor(DirEntry de)
{
    import std.path : baseName, stripExtension, dirName;

    Descriptor desc;
    if (de.isFile)
    {
        desc.name = de.name.baseName.stripExtension;
        desc.fullPath = de.name;
        desc.path = de.name.dirName;
    }
    else
    {
        desc.name = de.name.baseName;
        desc.path = de.name;
    }
    
    return desc;
}

void findMissingDependencies(ref Descriptor descriptor, Descriptor[] descriptors)
{
    tracef("Finding dependencies for %s ...", descriptor.name);
    for (size_t i = 0; i < descriptor.metaData.numberOfDependences; i++)
    {
        auto dep = descriptor.metaData.dependencies[i];
        foreach (desc; descriptors)
        {
            bool found = false;
            auto relationship = dep.relateTo(desc.metaData);
            switch (relationship)
            {
                case Relationship.IndependentOf:
                    break;

                case Relationship.DependOn:
                    found = true;
                    break;

                default:
                    descriptor.missingDependencies ~= dep;
                    found = true;
                    break;
            }

            if (found)
                break;
        }
    }
}

import std.algorithm.comparison : equal;
import core.stdc.string : strcmp;

void findCircularlyDependencies(ref Descriptor descriptor, Descriptor[] descriptors)
{
    tracef("Finding dependencies for %s ...", descriptor.name);
    for (size_t i = 0; i < descriptor.metaData.numberOfDependences; i++)
    {
        auto dep = descriptor.metaData.dependencies[i];
        foreach (other; descriptors)
        {
            if (other.metaData.name.strcmp(dep.name) == 0)
            {
                if (other.metaData.dependsOn(descriptor.metaData))
                {
                    descriptor.circularlyDependencies ~= dep;
                }
            }
        }
    }
}

void markHaveDependencyThatIsCircularlyDependent(ref Descriptor descriptor, Descriptor[] descriptors)
{
    tracef("Finding dependencies for %s ...", descriptor.name);
    for (size_t i = 0; i < descriptor.metaData.numberOfDependences; i++)
    {
        auto dep = descriptor.metaData.dependencies[i];
        foreach (other; descriptors)
        {
            if (other.metaData.name.strcmp(dep.name) == 0 && other.isCircularlyDependent)
            {
                descriptor.isDependentOnTypeThatIsCircularlyDependent = true;
                return;
            }
        }
    }
}

void markHaveDependencyThatIsMissingDependency(ref Descriptor descriptor, Descriptor[] descriptors)
{
    tracef("Finding dependencies for %s ...", descriptor.name);
    for (size_t i = 0; i < descriptor.metaData.numberOfDependences; i++)
    {
        auto dep = descriptor.metaData.dependencies[i];
        foreach (other; descriptors)
        {
            if (other.metaData.name.strcmp(dep.name) == 0 && other.missingDependencies !is null && other.missingDependencies.length > 0)
            {
                descriptor.isDependentOnTypeThatIsMissingDependency = true;
                return;
            }
        }
    }
}

bool load(ref Descriptor plugin)
{
    DirEntry[] allVersions;

    import std.file : exists, isFile;
    import std.path : buildPath, setExtension;
    import zero.bootstrap.versioningbootstrap;

    if (plugin.fullPath !is null && plugin.fullPath != "" && 
        plugin.fullPath.exists && plugin.fullPath.isFile)
    {
        allVersions ~= DirEntry(plugin.path);
    }
    else
    {
        import std.algorithm : each;
        import std.algorithm : filter;

        allVersions = searchAllVersions(plugin.path);
        if (allVersions.length == 0)
        {
            allVersions ~= DirEntry(plugin.path);
        }
    }

    import std.process : Pid;
    import std.typecons : Yes;

    version(Windows)
        auto ext = ".dll";
    else
        auto ext = ".so";

    auto pluginName = plugin.name.setExtension(ext);

    DirEntry versionStarted; Pid startedPid; 
    startNewestVersion(pluginName, allVersions, Yes.IsDll, versionStarted, startedPid, plugin.dllHandle);

    plugin.fullPath = versionStarted.name;

    if (plugin.dllHandle is null)
    {
        tracef("loading %s failed", plugin.fullPath);
        return false;
    }

    auto handle = cast(HMODULE)plugin.dllHandle;
    auto fp = GetProcAddress(handle, "GetMetaData");
    if (fp is null)
    {
        trace("GetMetaData not found");
        return false;
    }

    plugin.metaData = (cast(getMetaDataFn)fp)();
    auto metaName = plugin.metaData.name.fromStringz();
    assert(plugin.name == metaName);
    if (plugin.name != metaName)
    {
        warningf("Plugin %s has another file name %s", metaName, plugin.name);
        plugin.name = metaName;
    }

    tracef("Plugin %s, ver %s, dep %s loaded", plugin.name, plugin.metaData.ver, 
           plugin.metaData.dependencies);

    return true;
}

bool unload(ref Descriptor plugin)
{
    auto result = Runtime.unloadLibrary(plugin.dllHandle);

    if (!result)
    {
        tracef("Unloading %s failed", plugin);
    }
    else
    {
        plugin.dllHandle = null;
    }

    return result;
}

bool start(ref Descriptor plugin, void* handle, controlFn controller)
{
    if (plugin.dllHandle is null || plugin.metaData.start is null)
        return false;

    if (plugin.isMissingDependency)
    {
        foreach (dep; plugin.missingDependencies)
        {
            infof("%s is missing dependency %s", plugin.name, dep);
        }
        return false;
    }

    if (plugin.isCircularlyDependent ||
        plugin.isDependentOnTypeThatIsCircularlyDependent ||
        plugin.isDependentOnTypeThatIsMissingDependency)
    {
        warningf("Cannot start plugin %s because of wrong dependencies", plugin.name);
        return false;
    }

    tracef("Starting %s ...", plugin.name);

    auto result = plugin.metaData.start(handle, controller);

    if (result)
    {
        plugin.isStarted = true;
        plugin.runCount++;
    }
    else
    {
        tracef("Starting %s failed", plugin.name);
    }

    return result;
}

bool stop(ref Descriptor plugin)
{
    tracef("Stopping %s ...", plugin.name);

    if (plugin.dllHandle is null || plugin.metaData.stop is null)
        return true;

    auto result = plugin.metaData.stop();

    if (!result)
    {
        tracef("Stopping %s failed", plugin.name);
    }

    return result;
}

void update(ref Descriptor plugin, long deltaMS)
{
    tracef("Updating %s ...", plugin.name);

    if (plugin.dllHandle is null || plugin.metaData.update is null)
        return;

    plugin.metaData.update(deltaMS);
}
