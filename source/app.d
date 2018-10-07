import std.stdio;
import std.getopt;
import zero.grammar;
import zero.parser;
import zero.analyzer;
import zero.generator;

int main(string[] args)
{
    auto verbose = false;
    auto buildParser = false;
    string output = "a.out";

    auto help = getopt(
        args,
        std.getopt.config.bundling,
        std.getopt.config.passThrough,
        "parser|p", "build the language parser",  &buildParser,
        "verbose|v","verbose log", &verbose,
        "output|o", "output file name", &output
    );

    if (help.helpWanted)
    {
        defaultGetoptPrinter("Some information about the program.",
            help.options);
        return 0;
    }

    if (buildParser)
    {
        build();
        return 0;
    }

    if (args.length <= 1)
    {
        defaultGetoptPrinter("Some information about the program.",
            help.options);
        return 0;
    }

    auto inputs = args[1..$];
    foreach (f; inputs)
        writefln("input %s", f);

    string line;
	while ((line = readln()) !is null)
	{
		auto parsedTree = Zero(line);
		if (!parsedTree.successful)
			writeln(parsedTree.failMsg);
		else
			writeln(parsedTree.matches);
        writeln(parsedTree);
        writeln();
        auto simplified = simplifyParseTree(parsedTree.dup);
        writeln(simplified);
        writeln();
        writeln(generate(simplified));
        writeln();
	}

    return 0;
}

