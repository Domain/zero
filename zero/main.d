import std.stdio;
import zero.parser;
import std.file;
import zero.generator;
import std.conv;
import zero.analyzer;

struct AST
{
	ParseTree parseTree;
	int i;
	alias parseTree this;
}

long calc(ParseTree node)
{
	//if (node.children.length == 0)
	//    return 0L;

	long result = 0;
	switch (node.name)
	{
		case "Zero.AddExpr":
		case "Zero.MulExpr":
			result = calc(node.children[0]);
			//writeln("pop");
			//foreach (child; node.children)
			if (node.children.length > 2)
			{
				writeln("push");
				//result = calc(node.children[0]);
				final switch (node.children[1].matches[0])
				{
					case "+":
						result += calc(node.children[2]);
						writeln("add");
						break;

					case "-":
						result -= calc(node.children[2]);
						writeln("sub");
						break;

					case "*":
						result *= calc(node.children[2]);
						writeln("mul");
						break;

					case "/":
						result /= calc(node.children[2]);
						writeln("div");
						break;

					case "%":
						result %= calc(node.children[2]);
						writeln("mod");
						break;
				}
			}
			break;

		case "Zero.Decimal":
			result = to!long(node.matches[0]);
			writeln("ldc ", result);
			break;

		case "Zero.ExpressionStatement":
			result = calc(node.children[0]);
			writeln("Result: ", result);
			break;

		default:
			foreach (child; node.children)
				result = calc(child);
			break;
	}
	return result;
}

int main(string[] argv)
{
	//alias GenericZero!(AST).Zero Parser;
	//auto f = readText("test.zero");
	string line;
	while ((line = readln()) !is null)
	{
		auto parsedTree = Zero(line);
        auto simplified = parsedTree.dup;
        simplified = simplifyParseTree(simplified);
		if (!parsedTree.successful)
			writeln(parsedTree.failMsg);
		else
			writeln(parsedTree.matches);
        //writeln(parsedTree);
        //writeln();
		writeln(simplified);
		writeln(generate(simplified));
        writeln();
	}
    return 0;
}
