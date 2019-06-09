module zero.system.string;

import std.array:appender;

/// Like CommandlineToArgvW
string[] parseCmd(string cmd)
{
    string[] args;
    auto builder = appender!string();
    bool inQuote = false;
    int backslashesCount = 0;

    for (auto i = 0; i < cmd.length; i++)
    {
        switch (cmd[i])
        {
            case '"':
                if (backslashesCount > 0)
                {
                    for (auto j = 0; j < backslashesCount / 2; j++)
                    {
                        builder.put('\\');
                    }

                    if (backslashesCount % 2 == 0)
                    {
                        inQuote = true;
                    }
                    else
                    {
                        builder.put('"');
                    }

                    backslashesCount = 0;
                }
                else if (inQuote)
                {
                    inQuote = false;
                }
                else
                {
                    inQuote = true;
                }
                break;

            case '\\':
                backslashesCount++;
                break;

            case ' ':
            case '\t':
                if (inQuote)
                {
                    builder.put(cmd[i]);
                }
                else
                {
                    args ~= builder.data;
                    builder = appender!string();
                }
                break;

            default:
                for (auto j = 0; j < backslashesCount; j++)
                {
                    builder.put('\\');
                }
                backslashesCount = 0;
                builder.put(cmd[i]);
                break;
        }
    }
    args ~= builder.data;
    return args;
}
