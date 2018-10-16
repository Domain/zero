import std.stdio;
import std.getopt;
import zero.grammar;
import zero.parser;
import zero.analyzer;
import zero.generator;

string help(string program, GetoptResult result)
{
    import std.array : appender;

    static string usage = null;

    if (usage !is null)
        return usage;

    auto buffer = appender!string;
    buffer ~= "Usage: ";
    buffer ~= program;
    buffer ~= " [options] [files...]\n";
    buffer ~= "options: ";

    defaultGetoptFormatter(buffer, "", result.options);
    usage = buffer.data;

    return usage;
}

void parse(string code)
{
    auto pt = Zero(code);
    if (!pt.successful)
	    writeln(pt.failMsg);
    else
	    writeln(pt.matches);
    writeln(pt);
    writeln();
    auto syntaxTree = buildSyntaxTree(pt.dup);
    writeln(syntaxTree);
    // auto simplified = simplifyParseTree(pt.dup);
    // writeln(simplified);
    writeln();
    writeln(generate(syntaxTree));
    writeln();
}

int main(string[] args)
{
    auto verbose = false;
    auto buildParser = false;
    string output = "a.out";

    auto options = getopt(
        args,
        std.getopt.config.bundling,
        std.getopt.config.passThrough,
        "parser|p", "build the language parser",  &buildParser,
        "verbose|v","verbose log", &verbose,
        "output|o", "output file name", &output
    );

    if (options.helpWanted)
    {
        writeln(help(args[0], options));
        return 0;
    }

    if (buildParser)
    {
        if (output == "a.out")
            output = null;
        build("zero.parser", output);
        return 0;
    }

    if (args.length > 1)
    {
        import std.file : readText;

        auto inputs = args[1..$];
        foreach (f; inputs)
            parse(f.readText);

        return 0;
    }

    import std.array : appender;
    auto code = appender!string();

    auto done = false;

    while (!done)
    {
        auto line = readln();
        if (line is null)
        {
            done = true;
        }
        else if (line == "\n")
        {
            parse(code.data);
            code = appender!string();
        }
        else
        {
            code ~= line;
        }
    }

    return 0;
}

