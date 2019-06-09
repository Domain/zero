module zero.plugins.dependency;

import zero.version_;

struct Dependency
{
    immutable(char)* name;
    bool includeMin;
    Version minVer;
    bool includeMax;
    Version maxVer;

    string toString()
    {
        import std.format : format;
        import std.string : fromStringz;

        return "[ %s %s %s, %s %s ]".format(name.fromStringz(), 
               includeMin ? "[" : "(", minVer, maxVer, includeMax ? "]" : ")");
    }
}

enum Relationship
{
    IndependentOf,
    DependOn,
    VersionTooLow,
    VersionTooHigh,
    Unknown
}

int versionIn(Version ver, Dependency dep)
{
    if (dep.includeMin && ver < dep.minVer)
    {
        return -1;
    }

    if (!dep.includeMin && ver <= dep.minVer)
    {
        return -1;
    }

    if (dep.maxVer.Empty)
    {
        return 0;
    }

    if (dep.includeMax && ver > dep.maxVer)
    {
        return 1;
    }

    if (!dep.includeMax && ver >= dep.maxVer)
    {
        return 1;
    }

    return 0;
}
