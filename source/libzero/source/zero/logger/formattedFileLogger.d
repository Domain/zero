module zero.logger.formattedFileLogger;

import zero.logger.formattedLogger;
import std.stdio;
import std.experimental.logger : LogLevel;

class FormattedFileLogger : FormattedLogger!(std.stdio.File.LockingTextWriter)
{
    private File file;

    this(File file, string spec, const LogLevel lv = LogLevel.all) @safe
    {
        this.file = file;
        super(file.lockingTextWriter(), spec, lv);
    }

    this(string filename, string spec, const LogLevel lv = LogLevel.all) @safe
    {
        file.open(filename, "a");
        this(file, spec, lv);
    }

    this(File file, string spec, string[char] extender, const LogLevel lv = LogLevel.all) @safe
    {
        this.file = file;
        super(file.lockingTextWriter(), spec, extender, lv);
    }

    this(string filename, string spec, string[char] extender, const LogLevel lv = LogLevel.all) @safe
    {
        file.open(filename, "a");
        this(file, spec, extender, lv);
    }
}
