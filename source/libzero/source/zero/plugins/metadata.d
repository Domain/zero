module zero.plugins.metadata;

import zero.version_;
import zero.logger;
import zero.plugins.iplugin;
import zero.plugins.dependency;

import std.typecons : Flag;
import core.stdc.string : strcmp;

struct MetaData
{
    immutable(char)* name;
    Version ver;
    immutable(Dependency)* dependencies;
    size_t numberOfDependences;

    startFn     start;
    stopFn      stop;
    updateFn    update;
    upgradeFn   upgrade;
}

string bindFunction(T)(string name)
{
    import std.format : format;
    import std.string : toLower;

    auto lowerName = toLower(name);

    return `
        static if (__traits(compiles, T.%2$s))
        {
            if (__traits(getLinkage, T.%2$s) == "C" && hasStaticMember!(T, "%2$s"))
            {
                metadata.%2$s = &T.%2$s;
            }
        }
        else static if (__traits(compiles, T.%1$s))
        {
            if (__traits(getLinkage, T.%1$s) == "C" && hasStaticMember!(T, "%1$s"))
                metadata.%2$s = &T.%1$s;
        }
    `.format(name, lowerName);
}

MetaData getMetaData(T)()
{
    import std.traits : getUDAs, hasStaticMember;
    import std.string : toStringz;

    MetaData metadata;
    metadata.name = T.stringof.toStringz;
    auto ver = getUDAs!(T, Version);
    if (ver.length > 0)
    {
        metadata.ver = ver[0];
    }
    immutable(Dependency)[] dependencies;
    foreach (dep; getUDAs!(T, Dependency))
    {
        dependencies ~= dep;
    }
    metadata.dependencies = dependencies.ptr;
    metadata.numberOfDependences = dependencies.length;

    mixin(bindFunction!(T)("Start"));
    mixin(bindFunction!(T)("Stop"));
    mixin(bindFunction!(T)("Update"));
    mixin(bindFunction!(T)("Upgrade"));

    return metadata;
}

mixin template GetMetaData(T)
{
    pragma(mangle, __traits(identifier, GetMetaData))
    export extern(C) MetaData GetMetaData()
    {
        return getMetaData!T();
    }
}

Relationship relateTo(Dependency dependency, MetaData metadata)
{
    import std.string : icmp;

    if (strcmp(dependency.name, metadata.name) == 0)
    {
        switch (metadata.ver.versionIn(dependency))
        {
            case -1:
                return Relationship.VersionTooLow;

            case 1:
                return Relationship.VersionTooHigh;

            case 0:
                return Relationship.DependOn;

            default:
                break;
        }
        return Relationship.Unknown;
    }

    return Relationship.IndependentOf;
}

Relationship relateTo(MetaData first, MetaData second)
{
    import std.string : fromStringz;
    for (size_t i = 0; i < first.numberOfDependences; i++)
    {
        auto dep = first.dependencies[i];
        auto relationship = dep.relateTo(second);
        tracef("%s : %s --> %s", dep.name.fromStringz, second.name.fromStringz, relationship);
        if (relationship != Relationship.IndependentOf)
        {
            return relationship;
        }
    }

    return Relationship.IndependentOf;   
}

bool dependsOn(MetaData first, MetaData second)
{
    if (first.relateTo(second) == Relationship.DependOn)
        return true;

    return false;
}
