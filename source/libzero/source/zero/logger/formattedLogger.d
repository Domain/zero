module zero.logger.formattedLogger;

import std.experimental.logger;
import std.format : FormatSpec, formatValue;
import std.datetime;
import std.range : put;

/**
 格式化日志，可以指定日志的输出格式。
 格式说明：
    f: 文件名，就是__FILE__；
    l: 行号，就是__LINE__；
    n: 函数名，就是__FUNCTION__；
    p: 函数完整名，就是__PRETTY_FUNCTION__；
    o: 模块名，就是__MODULE__；
    g: 日志级别，trace(DBG), info(INF), warning(WRN), error(ERR), critical(CRI), fatal(FTL)；
    d: 日期，例如：2016-11-11；
    t: 时间，例如：12:11:05；
    m: 毫秒，例如：123；
    s: 日志内容
 以上格式均可使用标准格式控制
 */
class FormattedLogger(OutputRange) : Logger
{
    protected struct LogFormatter
    {
        /// log info
        LogEntry entry;
        bool used;

        void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt)
        {
            used = true;

            switch (fmt.spec)
            {
                case 'f':
                {
                    fmt.spec = 's';
                    formatValue(sink, entry.file, fmt);
                    break;
                }

                case 'l':
                {
                    fmt.spec = 'd';
                    formatValue(sink, entry.line, fmt);
                    break;
                }

                case 'n':
                {
                    fmt.spec = 's';
                    formatValue(sink, entry.funcName, fmt);
                    break;
                }

                case 'p':
                {
                    fmt.spec = 's';
                    formatValue(sink, entry.prettyFuncName, fmt);
                    break;
                }

                case 'o':
                {
                    fmt.spec = 's';
                    formatValue(sink, entry.moduleName, fmt);
                    break;
                }

                case 'g':
                {
                    string level = "all";
                    final switch (entry.logLevel)
                    {
                        case LogLevel.trace:
                            level = "DBG";
                            break;

                        case LogLevel.info:
                            level = "INF";
                            break;

                        case LogLevel.warning:
                            level = "WRN";
                            break;

                        case LogLevel.error:
                            level = "ERR";
                            break;

                        case LogLevel.critical:
                            level = "CRI";
                            break;

                        case LogLevel.fatal:
                            level = "FTL";
                            break;

                        case LogLevel.all:
                        case LogLevel.off:
                            level = "UKN";
                            break;
                    }
                    fmt.spec = 's';
                    formatValue(sink, level, fmt);
                    break;
                }

                case 'd':
                {
                    fmt.spec = 's';
                    formatValue(sink, entry.timestamp.toISOExtString()[0..10], fmt);
                    break;
                }

                case 't':
                {
                    fmt.spec = 's';
                    formatValue(sink, entry.timestamp.toISOExtString()[11..19], fmt);
                    break;
                }

                case 'm':
                {
                    fmt.spec = 'd';
                    formatValue(sink, entry.timestamp.fracSecs.split!"msecs"().msecs, fmt);
                    break;
                }

                case 's':
                {
                    fmt.spec = 's';
                    formatValue(sink, entry.msg, fmt);
                    break;
                }

                default:
                {
                    used = false;
                    break;
                }
            }
        }
    }

    private OutputRange buffer;
    private string spec;
    private FormatSpec!char formatSpec;
    private LogFormatter formatter;
    private string[char] extender;

    this(OutputRange buffer, string spec, const LogLevel lv = LogLevel.all) @safe
    {
        super(lv);
        this.buffer = buffer;
        this.spec = spec;
        formatSpec = FormatSpec!char(spec);
    }

    this(OutputRange buffer, string spec, string[char] extender, const LogLevel lv = LogLevel.all) @safe
    {
        super(lv);
        this.buffer = buffer;
        this.spec = spec;
        formatSpec = FormatSpec!char(spec);
        this.extender = extender;
    }

    void addExtender(in char spec, string value)
    {
        extender[spec] = value;
    }

    override protected void writeLogMsg(ref LogEntry entry) @trusted
    {
        formatter.entry = entry;
        formatSpec.trailing = spec;
        while (formatSpec.writeUpToNextSpec(buffer))
        {
            formatValue(buffer, formatter, formatSpec);
            if (!formatter.used)
            {
                auto ext = formatSpec.spec in extender;
                if (ext !is null)
                {
                    formatSpec.spec = 's';
                    formatValue(buffer, *ext, formatSpec);
                }
            }
        }
        put(buffer, "\n");
    }
}

unittest
{
    import std.array : appender;
    import std.format : formattedWrite;

    auto server = "Test";
    auto buffer = appender!string();
    auto logger = new FormattedLogger!(typeof(buffer))(buffer, "[%g %d %t.%m %v] %s [%f(%l):%n]");
    logger.addExtender('v', server);
    auto msg = "test logger";
    auto now = Clock.currTime();
    logger.info(msg);
    auto line = __LINE__ - 1;
    auto expected = appender!string();
    formattedWrite(expected, "[INF %s %s.%d %s] %s [%s(%d):%s]\n", now.toISOExtString()[0..10], now.toISOExtString[11..19], 
        now.fracSecs.split!"msecs"().msecs, server, msg, __FILE__, line, __FUNCTION__);
    assert(buffer.data == expected.data, buffer.data ~ " != " ~ expected.data);
}