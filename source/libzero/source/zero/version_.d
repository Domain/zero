module zero.version_;

import zero.algorithm.hash : hashCombine;
import zero.logger;

struct Version
{
public:
    this(string ver)
    {
        TryParse(ver, this);
    }

    this(int major, int minor)
    {
        this(major, minor, 0);
    }

    this(int major, int minor, int build)
    {
        this(major, minor, build, 0);
    }

    this(int major, int minor, int build, int revision)
    {
        this.major = major;
        this.minor = minor;
        this.build = build;
        this.revision = revision;
    }

    static bool TryParse(string str, out Version ver)
    {
        import std.string : split;
        import std.conv : to;
        import std.algorithm.iteration : map;
        import std.array : array;
        import std.range : padRight;

        try
        {
            ver.versionArray = str.split(".").map!(n => to!int(n)).padRight(0, 4).array;
        }
        catch (Exception e)
        {
            return false;
        }

        return true;
    }

    unittest
    {
        Version v = Version("1.2.3.4");
        assert(v == Version(1, 2, 3, 4));

        Version v1 = Version("1.2");
        assert(v1 == Version(1, 2, 0, 0));

        auto rev = 123 << 16 | 456;
        auto v2 = Version(1, 2, 3, rev);
        assert(v2.Major == 1);
        assert(v2.Minor == 2);
        assert(v2.Build == 3);
        assert(v2.Revision == rev);
        assert(v2.MajorRevision == (rev >> 16));
        assert(v2.MajorRevision == 123);
        assert(v2.MinorRevision == (rev & 0xFFFF));
        assert(v2.MinorRevision == 456);

        Version v3;
        assert(!Version.TryParse("0.1.a", v3));
        assert(Version.TryParse("0", v3));
        assert(Version.TryParse("123.456", v3));
        assert(Version.TryParse("123.456.789", v3));
        assert(Version.TryParse("123.456.789.111", v3));

        assert(v3 > v2);
        assert(v3 > v1);
        assert(v > v1);
        assert(v2 > v);
        assert(v < v3);

        Version v4 = Version(123, 456, 789, 111);
        assert(v3 == v4);
    }

    int Major()     const @property @safe pure nothrow { return major; }
    int Minor()     const @property @safe pure nothrow { return minor; }
    int Build()     const @property @safe pure nothrow { return build; }
    int Revision()  const @property @safe pure nothrow { return revision; }

    short MajorRevision() const @property @safe pure nothrow { return majorRev; }
    short MinorRevision() const @property @safe pure nothrow { return minorRev; }

    bool Empty() const @property @safe pure nothrow 
    { 
        import std.algorithm.searching : all;
        return versionArray[].all!"a == 0";
    }

    bool opEquals()(auto ref const typeof(this) rhs) const @safe pure nothrow
    {
        return major == rhs.major && minor == rhs.minor && build == rhs.build && revision == rhs.revision;
    }

    int opCmp()(auto ref const typeof(this) rhs) const @safe pure nothrow
    {
        int delta = major - rhs.major;
        if (delta == 0)
        {
            delta = minor - rhs.minor;
            if (delta == 0)
            {
                delta = build - rhs.build;
                if (delta == 0)
                {
                    delta = revision - rhs.revision;
                }
            }
        }

        return delta;
    }

    size_t toHash() const @safe pure nothrow
    {
        return major.hashCombine(minor).hashCombine(build).hashCombine(revision);
    }

    string toString()
    {
        import std.format : format;
        return format!"%s.%s.%s.%s"(major, minor, build, revision);
    }

private:
    union
    {
        struct
        {
            int major;
            int minor;
            int build;
            union
            {
                int revision;
                struct
                {
                    short minorRev;
                    short majorRev;
                }
            }
        }

        int[4] versionArray;
    }
}