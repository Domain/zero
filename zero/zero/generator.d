module zero.generator;

import pegged.peg;
import std.array : Appender, appender;
import std.format : formattedWrite;
import std.stdio : writeln;
import std.conv;

struct Generator
{
	static immutable int registerCount = 8;
	static immutable int pc = registerCount - 1;
	static immutable int mp = registerCount - 2;
	static immutable int gp = registerCount - 3;
	static immutable int ac = 0;
	static immutable int ac1 = 1;

	Appender!string buffer;
	int emitLoc = 0;
	int highEmitLoc = 0;
	int tmpOffset = 0;

	this(ParseTree root, Appender!string buf)
	{
		buffer = buf;
		generateExpression(root);
		emitRO("HALT", 0, 0, 0, "");
	}

	void generateExpression(ParseTree node)
	{
		switch (node.name)
		{
			case "Zero.IfStatement":
				generateExpression(node.children[0]);
				auto saveLoc1 = emitSkip(1);
				generateExpression(node.children[1]);
				auto saveLoc2 = emitSkip(1);
				auto currentLoc = emitSkip(0);
				emitBackup(saveLoc1);
				emitRM_Abs("JEQ", ac, currentLoc, "If: jmp to else");
				emitRestore();
				if (node.children.length > 2)
				{
					generateExpression(node.children[2]);
					currentLoc = emitSkip(0);
					emitBackup(saveLoc2);
					emitRM_Abs("LDA", pc, currentLoc, "jmp to end");
					emitRestore();
				}
				break;

			case "Zero.AddExpr":
			case "Zero.MulExpr":
			case "Zero.CompareExpr":
				generateExpression(node.children[0]);
				if (node.children.length > 2)
				{
					emitRM("ST", ac, tmpOffset--, mp, "op: push left");
					generateExpression(node.children[2]);
					emitRM("LD", ac1, ++tmpOffset, mp, "op: load left");

					final switch (node.children[1].matches[0])
					{
						case "+":
							emitRO("ADD", ac, ac1, ac, "op +");
							break;

						case "-":
							emitRO("SUB", ac, ac1, ac, "op -");
							break;

						case "~":
							emitRO("CAT", ac, ac1, ac, "op ~");
							break;

						case "*":
							emitRO("MUL", ac, ac1, ac, "op *");
							break;

						case "/":
							emitRO("DIV", ac, ac1, ac, "op /");
							break;

						case "%":
							emitRO("MOD", ac, ac1, ac, "op %");
							break;

						case "<":
							emitRO("SUB",ac,ac1,ac,"op <") ;
							emitRM("JLT",ac,2,pc,"br if true") ;
							emitRM("LDC",ac,0,ac,"false case") ;
							emitRM("LDA",pc,1,pc,"unconditional jmp") ;
							emitRM("LDC",ac,1,ac,"true case") ;
							break;

						case "=" :
							emitRO("SUB",ac,ac1,ac,"op ==") ;
							emitRM("JEQ",ac,2,pc,"br if true");
							emitRM("LDC",ac,0,ac,"false case") ;
							emitRM("LDA",pc,1,pc,"unconditional jmp") ;
							emitRM("LDC",ac,1,ac,"true case") ;
							break;
					}
				}
				break;

			case "Zero.OrExpr":
				generateExpression(node.children[0]);
				if (node.children.length > 1)
				{
					emitRM("ST", ac, tmpOffset--, mp, "op: push left");
					generateExpression(node.children[1]);
					emitRM("LD", ac1, ++tmpOffset, mp, "op: load left");
					emitRO("OR", ac, ac1, ac, "op or");
				}
				break;

			case "Zero.AndExpr":
				generateExpression(node.children[0]);
				if (node.children.length > 1)
				{
					emitRM("ST", ac, tmpOffset--, mp, "op: push left");
					generateExpression(node.children[1]);
					emitRM("LD", ac1, ++tmpOffset, mp, "op: load left");
					emitRO("AND", ac, ac1, ac, "op and");
				}
				break;

			case "Zero.NotExpr":
				generateExpression(node.children[0]);
				emitRO("NOT", ac, ac, 0, "op not");
				break;

			case "Zero.VarIdentifier":
				emitRM("LD", ac, node.matches[0], 0, "load var");
				break;

			case "Zero.Decimal":
				auto result = to!long(node.matches[0]);
				emitRM("LDC", ac, result, 0, "load const");
				break;

			case "Zero.Hexadecimal":
				auto result = to!long(node.matches[0], 16);
				emitRM("LDC", ac, result, 0, "load const");
				break;

			case "Zero.Binary":
				auto result = to!long(node.matches[0], 2);
				emitRM("LDC", ac, result, 0, "load const");
				break;

			case "Zero.ExpressionStatement":
				generateExpression(node.children[0]);
				break;

			case "Zero.CallExpr":
				emitRO("CALL", ac, ac1, ac, "call");
				break;

			default:
				foreach (child; node.children)
					generateExpression(child);
				break;
		}
	}

	void updateHighEmitLoc()
	{
		if (highEmitLoc < emitLoc)
			highEmitLoc = emitLoc;
	}

	void saveTemp()
	{
	}

	void loadTemp()
	{
	}

	void emitComment(string comment)
	{
		formattedWrite(buffer, "// %s\n", comment);
	}

	void emitRO(string op, int r, int s, int t, string comment)
	{
		formattedWrite(buffer, "%3d: %5s %d, %d, %d %-10s\n", emitLoc++, op, r, s, t, "//" ~ comment);
		updateHighEmitLoc();
	}

	void emitRM(T)(string op, int r, T d, int s, string comment)
	{
		formattedWrite(buffer, "%3d: %5s %d, %s(%d) %-10s\n", emitLoc++, op, r, d, s, "//" ~ comment);
		updateHighEmitLoc();
	}

	int emitSkip(int howMany)
	{
		int i = emitLoc;
		emitLoc += howMany;
		updateHighEmitLoc();
		return i;
	}

	void emitBackup(int loc)
	{
		if (loc > highEmitLoc)
			emitComment("BUG!!!");
		emitLoc = loc;
	}

	void emitRestore()
	{
		emitLoc = highEmitLoc;
	}

	void emitRM_Abs(string op, int r, int a, string comment)
	{
		formattedWrite(buffer, "%3d: %5s %d, %d(%d) // %s\n", emitLoc, op, r, a-(emitLoc+1), pc, comment);
		++emitLoc;
		updateHighEmitLoc();
	}
}

void traverse(ParseTree node, void delegate(ParseTree) prevProc, void delegate(ParseTree) postProc)
{
	prevProc(node);
	foreach (child; node.children)
	{
		traverse(child, prevProc, postProc);
	}
	postProc(node);
}

string generate(ParseTree node)
{
	auto buffer = appender!string();
	Generator generator = Generator(node, buffer);
	return buffer.data;
}
