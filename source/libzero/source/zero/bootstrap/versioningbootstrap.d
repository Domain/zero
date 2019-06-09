module zero.bootstrap.versioningbootstrap;

import zero.bootstrap.ibootstrap;
import zero.version_;
import zero.logger;

import std.getopt;
import std.file;
import std.path;
import std.algorithm.iteration : each, filter;
import std.algorithm.sorting : sort;
import std.array : array;
import std.process;
import std.typecons;

class VersioningBootstrap : IBootstrap
{
    private
    {
        string program;
        bool removeOld = false;
        int pid = 0;
        bool wait = false;
        bool verbose = false;
        bool isDll = false;
    }

    int Run(string[] args)
    {
        program = thisExePath.baseName;

        auto opt = getopt(
                          args,
                          std.getopt.config.passThrough,
                          "program|p",  &program,
                          "removeold|r",&removeOld,
                          "pid|i",      &pid,
                          "wait|w",     &wait,
                          "verbose|v",  &verbose,
                          "dll|d",      &isDll);

        if (opt.helpWanted)
        {
            defaultGetoptPrinter("Some information about the program.",
                                 opt.options);
            return 0;
        }

        Version dummy;

        auto allVers = searchAllVersions(thisExePath.dirName());

        if (wait && pid != 0)
        {
            infof("Waiting for process %s to exit...", pid);
            //wait(pid);
        }

        DirEntry versionStarted;
        Pid startedPid;
        void* handle;
        if (!startNewestVersion(program, args, allVers, isDll ? Yes.IsDll : No.IsDll, versionStarted, startedPid, handle))
        {
            error("No suitable executable was found or able to be started.");
            return 1;
        }

        scope(exit) std.process.wait(startedPid);

        if (removeOld)
        {
            deleteOlderVersions(allVers, versionStarted);
        }

        return 0;
    }
}

DirEntry[] searchAllVersions(string path)
{
    Version dummy;

    auto allVers = dirEntries(path, SpanMode.shallow)
        .filter!(f => f.isDir && Version.TryParse(f.name.baseName, dummy)).array
        .sort!((a, b) => (Version(a.name.baseName) > Version(b.name.baseName))).array;

    return allVers;
}

bool startNewestVersion(string program, DirEntry[] sortedVersionedDir, Flag!"IsDll" isDll, 
                        out DirEntry versionStarted, out Pid startedPid, out void* dllHandle)
{
    string[] args;
    args ~= program;
    return startNewestVersion(program, args, sortedVersionedDir, isDll, versionStarted, startedPid, dllHandle);
}

bool startNewestVersion(string program, string[] args, DirEntry[] sortedVersionedDir, Flag!"IsDll" isDll, 
                        out DirEntry versionStarted, out Pid startedPid, out void* dllHandle)
{
    import core.runtime;
    foreach (versionToStart; sortedVersionedDir)
    {
        auto path = buildPath(versionToStart.name, program);
        if (path.exists)
        {
            try
            {
                args[0] = path;
                tracef("Starting %s ...", args);
                if (isDll)
                {
                    dllHandle = Runtime.loadLibrary(path);
                }
                else
                {
                    startedPid = spawnProcess(args, null, Config.none, versionToStart.name);
                }
                versionStarted = versionToStart;
                return true;
            }
            catch (Exception e)
            {
                infof("Fail to start %s : %s", path, e.msg);
            }
        }
    }

    return false;
}

bool deleteOlderVersions(DirEntry[] sortedVersionedDir, in DirEntry versionStarted)
{
    import std.algorithm.iteration : filter, each;
    sortedVersionedDir[1..$].filter!(v => v != versionStarted).each!(v => v.name.rmdirRecurse());

    return true;
}