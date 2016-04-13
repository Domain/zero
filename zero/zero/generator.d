module zero.generator;

import pegged.peg;
import std.array : join;
import std.format : format;
import std.stdio : writeln;
import std.conv;
import std.traits : isSomeString;
import std.container.slist;
import zero.allocator;
import std.bitmanip;
import msgpack;

struct Generator
{
public:
    this(ParseTree root)
	{
        allocator = Allocator();
		generate(root);
		emit(InstructionSet.HALT);
        generateData();
	}

    @property string source()
    {
        return buffers.join("\n");
    }

private:
	static immutable int registerCount = 8;
	static immutable int pc = registerCount - 1;
	static immutable int mp = registerCount - 2;
	static immutable int gp = registerCount - 3;
	static immutable int ac = 0;
	static immutable int ac1 = 1;
	static immutable string[] reg = ["AX", "BX", "CX", "DX", "SP", "GP", "MP", "PC"];

    enum InstructionSet
    {
        HALT = 0, ADD, SUB, MUL, DIV, 
        MOD, POW, NEG, CAT, IN,
        AND, OR, NOT, MOV, CALL, 
        RET, JMP, JEQ, JNE, LT, 
        LE, EQ, NE, GT, GE,
        LOAD, DATA
    }

    struct R0
    {
        mixin(bitfields!(
                         InstructionSet, "op", 6,
                         uint, "ax", 26));
    }

    struct R1
    {
        mixin(bitfields!(
                         InstructionSet, "op", 6,
                         uint, "a",  7,
                         RegisterType, "ar", 1,
                         uint, "bx", 18));
    }

    struct R3
    {
        mixin(bitfields!(
                         InstructionSet, "op", 6,
                         uint, "a",  7,
                         RegisterType, "ar", 1,
                         uint, "b",  7,
                         RegisterType, "br", 2,
                         uint, "c",  7,
                         RegisterType, "cr", 2));
    }

    union Instruction
    {
        uint instruction;
        mixin(bitfields!(
                         InstructionSet, "opCode", 6,
                         uint, "", 26));
        R0 r0;
        R1 r1;
        R3 r3;
    }


    
	int emitLoc = 0;
	int highEmitLoc = 0;
	int tmpOffset = 0;

    string[] buffers;

    Allocator allocator;

    void formatCode(Char, A...)(in Char[] fmt, A args)
    {
        if (emitLoc + 1 > buffers.length)
        {
            buffers.length = emitLoc + 1;
        }

        buffers[emitLoc] ~= format(fmt, args);
    }

    void generateData()
    {
        auto data = allocator.ConstData;
        emit(InstructionSet.DATA, allocator.ConstCount);
        auto unpacker = StreamingUnpacker(data);
        foreach (value; unpacker)
        {
            switch (value.type)
            {
                case Value.Type.signed:
                case Value.Type.unsigned:
                    emit("", value.as!long);
                    break;

                case Value.Type.raw:
                    emit("", value.as!string);
                    break;

                default:
                    break;
            }
        }
    }

	void generate(ParseTree node)
	{
		switch (node.name)
		{
            case "Zero.BlockStatement":
                generateBlock(node);
                break;

            case "Zero.VarStatement":
                generateVar(node);
                break;

			case "Zero.IfStatement":
                generateIf(node);
				break;

            case "Zero.RepeatStatement":
                generateRepeat(node);
                break;

            case "Zero.WhileStatement":
                generateWhile(node);
                break;

            case "Zero.ForStatement":
                generateFor(node);
                break;

            case "Zero.ForeachStatement":
                generateForeach(node);
                break;

            case "Zero.BreakStatement":
                generateBreak(node);
                break;

            case "Zero.ContinueStatement":
                generateContinue(node);
                break;

            case "Zero.ReturnStatement":
                generateReturn(node);
                break;

            case "Zero.FunctionStatement":
                generateFunction(node);
                break;

            case "Zero.AssignExpr":
                if (node.children.length > 1)
                {
                    generate(node.children[1]);
                    auto var = allocator.FindVariable(node.matches[0]);
                    if (var !is null)
                        emitRM("ST", ac, var.offset, mp, "assign %s", node.matches[0]);
                }
                else
                {
                    generate(node.children[0]);
                }
                break;

            case "Zero.TernaryExpr":
                generateTernary(node);
                break;

			case "Zero.AddExpr":
			case "Zero.MulExpr":
			case "Zero.CompareExpr":
			case "Zero.OrExpr":
			case "Zero.AndExpr":
            case "Zero.PowExpr":
				generateBinary(node);
				break;

			case "Zero.NotExpr":
				generate(node.children[0]);
				emitRO("NOT", ac, ac, 0, "op not");
				break;

            case "Zero.SignExpr":
				generate(node.children[0]);
                if (node.matches[0] == "-")
				    emitRO("NEG", ac, ac, 0, "op -");
                break;

			case "Zero.VarIdentifier":
                auto var = allocator.FindVariable(node.matches[0]);
                if (var !is null)
				    emitRM("LDV", ac, var.offset, mp, "load var %s", node.matches[0]);
				break;

			case "Zero.Decimal":
				auto result = to!long(node.matches[0]);
                emitRM("LDI", ac, result, 0, "load decimal %s", node.matches[0]);
                //emitCode("%5s, %s, %d // load decimal %s", "LDI", reg[ac], result, node.matches[0]);
				break;

			case "Zero.Hexadecimal":
				auto result = to!long(node.matches[0], 16);
                emitRM("LDI", ac, result, 0, "load hexadecimal %s", node.matches[0]);
                //emitCode("%5s, %s, %d // load hexadecimal %s", "LDI", reg[ac], result, node.matches[0]);
				break;

			case "Zero.Binary":
				auto result = to!long(node.matches[0], 2);
                emitRM("LDI", ac, result, 0, "load binary %s", node.matches[0]);
                //emitCode("%5s, %s, %d // load binary %s", "LDI", reg[ac], result, node.matches[0]);
				break;

			case "Zero.RealLiteral":
				auto result = to!double(node.matches[0]);
                emitRM("LDR", ac, result, 0, "load double %s", node.matches[0]);
                //emitCode("%5s, %s, %d // load double %s", "LDR", reg[ac], result, node.matches[0]);
				break;

            case "Zero.String":
                emitRM("LDS", ac, node.matches[0], 0, "load string %s", node.matches[0]);
                break;

            case "Zero.ExpressionStatement":
                generateExpr(node.children[0]);
                break;

			case "Zero.CallExpr":
                generateCall(node);
				break;

			default:
				foreach (child; node.children)
					generate(child);
				break;
		}
	}

    string generateExpr(ParseTree node)
    {
        string result = "";

        switch (node.name)
		{
            case "Zero.AssignExpr":
                if (node.children.length > 1)
                {
                    generate(node.children[1]);
                    auto var = allocator.FindVariable(node.matches[0]);
                    if (var !is null)
                        emitRM("ST", ac, var.offset, mp, "assign %s", node.matches[0]);
                }
                else
                {
                    generate(node.children[0]);
                }
                break;

            case "Zero.TernaryExpr":
                generateTernary(node);
                break;

			case "Zero.AddExpr":
                auto lhs = generateExpr(node.children[0]);
                auto rhs = generateExpr(node.children[2]);
                result = allocator.AllocateRegister();
                auto op = InstructionSet.ADD;
                if (node.children[1].matches[0] == "-")
                    op = InstructionSet.SUB;
                else if (node.children[1].matches[0] == "~")
                    op = InstructionSet.CAT;
                emit(op, result, lhs, rhs);
                break;

			case "Zero.MulExpr":
                auto lhs = generateExpr(node.children[0]);
                auto rhs = generateExpr(node.children[2]);
                result = allocator.AllocateRegister();
                auto op = InstructionSet.MUL;
                if (node.children[1].matches[0] == "/")
                    op = InstructionSet.DIV;
                else if (node.children[1].matches[0] == "%")
                    op = InstructionSet.MOD;
                emit(op, result, lhs, rhs);
                break;

			case "Zero.CompareExpr":
                auto lhs = generateExpr(node.children[0]);
                auto rhs = generateExpr(node.children[2]);
                result = allocator.AllocateRegister();
                auto op = InstructionSet.LE;
                auto isNotIn = false;
                switch (node.children[1].matches[0])
                {
                    case "<=":
                        op = InstructionSet.LE;
                        break;

                    case "<":
                        op = InstructionSet.LT;
                        break;

                    case "=":
                        op = InstructionSet.EQ;
                        break;

                    case ">=":
                        op = InstructionSet.GE;
                        break;

                    case ">":
                        op = InstructionSet.GT;
                        break;

                    case "!=":
                        op = InstructionSet.NE;
                        break;

                    case "in":
                        op = InstructionSet.IN;
                        break;

                    case "!in":
                        op = InstructionSet.IN;
                        isNotIn = true;
                        break;

                    default:
                        break;
                }
                emit(op, result, lhs, rhs);
                if (isNotIn)
                    emit(InstructionSet.NOT, result, result);
                break;

			case "Zero.OrExpr":
                auto lhs = generateExpr(node.children[0]);
                auto rhs = generateExpr(node.children[2]);
                result = allocator.AllocateRegister();
                emit(InstructionSet.OR, result, lhs, rhs);
                break;

			case "Zero.AndExpr":
                auto lhs = generateExpr(node.children[0]);
                auto rhs = generateExpr(node.children[2]);
                result = allocator.AllocateRegister();
                emit(InstructionSet.AND, result, lhs, rhs);
                break;

            case "Zero.PowExpr":
                auto lhs = generateExpr(node.children[0]);
                auto rhs = generateExpr(node.children[2]);
                result = allocator.AllocateRegister();
                emit(InstructionSet.POW, result, lhs, rhs);
                break;

			case "Zero.NotExpr":
				auto lhs = generateExpr(node.children[0]);
				result = allocator.AllocateRegister();
                emit(InstructionSet.NOT, result, lhs);
				break;

            case "Zero.SignExpr":
				generate(node.children[0]);
                if (node.matches[0] == "-")
				    emitRO("NEG", ac, ac, 0, "op -");
                break;

			case "Zero.VarIdentifier":
                auto var = allocator.FindVariable(node.matches[0]);
                if (var !is null)
				    emitRM("LDV", ac, var.offset, mp, "load var %s", node.matches[0]);
				break;

			case "Zero.Decimal":
				auto value = to!long(node.matches[0]);
                auto c = allocator.AllocateConst(value);
                result = allocator.AllocateRegister();
                emit(InstructionSet.MOV, result, c);
				break;

			case "Zero.Hexadecimal":
				auto value = to!long(node.matches[0], 16);
                auto c = allocator.AllocateConst(value);
                result = allocator.AllocateRegister();
                emit(InstructionSet.MOV, result, c);
				break;

			case "Zero.Binary":
				auto value = to!long(node.matches[0], 2);
                auto c = allocator.AllocateConst(value);
                result = allocator.AllocateRegister();
                emit(InstructionSet.MOV, result, c);
				break;

			case "Zero.RealLiteral":
				auto value = to!double(node.matches[0]);
                auto c = allocator.AllocateConst(value);
                result = allocator.AllocateRegister();
                emit(InstructionSet.MOV, result, c);
				break;

            case "Zero.RawString":
            case "Zero.QuotedString":
                auto c = allocator.AllocateConst(node.matches[0][1..$-1]);
                result = allocator.AllocateRegister();
                emit(InstructionSet.MOV, result, c);
                break;

                //case "Zero.ExpressionStatement":
                //    generateExpression(node.children[0]);
                //    break;

			case "Zero.CallExpr":
                generateCall(node);
				break;

			default:
				foreach (child; node.children)
					generate(child);
				break;
		}
        return result;
    }

    void generateBlock(ParseTree node)
    {
        foreach (child; node.children)
        {
            generate(child);
        }
    }

    void generateVar(ParseTree node)
    {
        EVarScope varScope = EVarScope.Local;

        if (node.matches[0] == "global")
        {
            varScope = EVarScope.Global;
        }

        foreach (decl; node.children)
        {
            auto var = allocator.AllocateVariable(decl.matches[0], varScope);
            if (decl.name == "Zero.VarExpression")
            {
                generate(decl.children[1]);
                emitRM("ST", ac, var.offset, mp, "assign %s", var.name);
            }
            //if (decl.children.length > 1)
            //{
            //}
        }
    }

    void generateIf(ParseTree node)
    {
        emitComment("----> if");
        // condition
        auto cond = generateExpr(node.children[0]);

        // then location, reserve loc for jump to else
        auto jmp2Else = emitSkip(1);
        // then
        generate(node.children[1]);

        // else location, reserve loc for jump over else
        auto jmp2OverElse = emitSkip(1);
        auto elseLoc = emitSkip(0);

        emitBackup(jmp2Else);
        //emitRM_Abs("JEQ", ac, elseLoc, "If: jmp to else");
        emitAbs(InstructionSet.JNE, cond, elseLoc);
        emitRestore();

        // else
        if (node.children.length > 2)
        {
            generate(node.children[2]);
        }

        auto overElseLoc = emitSkip(0);
        emitBackup(jmp2OverElse);
        //emitRM_Abs("LDA", pc, overElseLoc, "jmp to end");
        emitAbs(InstructionSet.JMP, overElseLoc);
        emitRestore();
        emitComment("<---- if");
    }

    int beginLoc = 0;
    int endLoc = 0;

    struct BackPatchHelper
    {
        static SList!(BackPatchHelper*) current;

        int[] backpatchLoc;
        string comment;
        Generator* generator;

        @disable this();

        this(string comment, Generator* generator)
        {
            this.comment = comment;
            this.generator = generator;
            current.insertFront(&this);
        }

        ~this()
        {
            backpatch();
            current.removeFront();
        }

        void backpatch()
        {
            foreach (loc; backpatchLoc)
            {
                generator.emitBackup(loc);
                //generator.emitRM_Abs("LDA", generator.pc, generator.endLoc, comment);
                generator.emitAbs(InstructionSet.JMP, generator.endLoc);
            }
            generator.emitRestore();
        }

        static void AddLocation(int loc)
        {
            assert(current.front != null);
            current.front.backpatchLoc ~= loc;
        }
    }

    void generateRepeat(ParseTree node)
    {
        BackPatchHelper helper = BackPatchHelper("break", &this);

        emitComment("----> repeat");
        beginLoc = emitSkip(0);
        generate(node.children[0]);
        auto cond = generateExpr(node.children[1]);
        endLoc = emitSkip(0);
        emitAbs(InstructionSet.JNE, cond, beginLoc);
        //emitRM_Abs("JEQ", ac, beginLoc, "repeat: jmp back to body");
        emitComment("<---- repeat");
    }

    void generateWhile(ParseTree node)
    {
        BackPatchHelper helper = BackPatchHelper("break", &this);

        emitComment("----> while");
        beginLoc = emitSkip(0);
        auto cond = generateExpr(node.children[0]);
        auto jmp2EndLoc = emitSkip(1);
        generate(node.children[1]);
        //emitRM_Abs("LDA", pc, beginLoc,"unconditional jmp");
        emitAbs(InstructionSet.JMP, beginLoc);
        endLoc = emitSkip(0);
        emitBackup(jmp2EndLoc);
        //emitRM_Abs("JEQ", ac, endLoc, "repeat: jmp back to body");
        emitAbs(InstructionSet.JNE, cond, endLoc);
        emitRestore();
        emitComment("<---- while");
    }

    void generateFor(ParseTree node)
    {
        BackPatchHelper helper = BackPatchHelper("break", &this);

        auto cond = node.children[0];
        auto stmt = node.children[1];

        auto init = cond.children[0];
        auto stop = cond.children[1];

        emitComment("----> for");
        generate(init);
        beginLoc = emitSkip(0);
        generate(stop);
        auto jmp2EndLoc = emitSkip(1);
        generate(stmt);
        if (cond.children.length > 2)
            generate(cond.children[2]);
        endLoc = emitSkip(0);
        emitBackup(jmp2EndLoc);
        emitRM_Abs("JEQ", ac, endLoc, "for: jmp to end");
        emitRestore();
        emitComment("<---- for");
    }

    void generateForeach(ParseTree node)
    {
        BackPatchHelper helper = BackPatchHelper("break", &this);

    }

    void generateBreak(ParseTree node)
    {
        BackPatchHelper.AddLocation(emitSkip(1));
    }

    void generateContinue(ParseTree node)
    {
        //emitRM_Abs("LDA", pc, beginLoc, "continue");
        emitAbs(InstructionSet.JMP, beginLoc);
    }

    void generateReturn(ParseTree node)
    {
        if (node.children.length > 1)
        {
            generate(node.children[0]);
        }
        BackPatchHelper.AddLocation(emitSkip(1));
    }

    void generateFunction(ParseTree node)
    {
        BackPatchHelper helper = BackPatchHelper("return", &this);

        emitComment("----> function");
        emitCode("%5s %s", "PROC", node.matches[0]);
        appendComment("function %s", node.matches[0]);
        endLoc = emitSkip(0);
        emitCode("%5s", "RET");
        emitComment("<---- function");
    }

    void generateCall(ParseTree node)
    {
        //generate(node.children[0]);
        //if (node.children.length > 1)
        //{
        //    emitRM("ST", ac, tmpOffset--, mp, "op: push left");
        //    foreach (child; node.children[1].children)
        //    {
        //        generate(child);
        //    }
        //    emitRM("LD", ac1, ++tmpOffset, mp, "op: load left");
        //}
        //emitRM("ST", fp, tmpOffset--, mp, "save fp");
        //emitRO("LDI", fp, sp, 0);
        //emitRO("CALL", ac, ac1, ac, "call");
    }

    void generateTernary(ParseTree node)
    {
        auto cond = generateExpr(node.children[0]);
        if (node.children.length > 2)
        {
            auto jmp2SecondLoc = emitSkip(1);
            generate(node.children[1]);
            auto jmp2EncLoc = emitSkip(1);
            auto secondLoc = emitSkip(0);
            generate(node.children[2]);
            auto endLoc = emitSkip(0);
            emitBackup(jmp2SecondLoc);
            emitRM_Abs("JEQ", ac, secondLoc, "jmp to second");
            emitBackup(jmp2EncLoc);
            emitRM_Abs("LDA", pc, endLoc, "jmp to end");
            emitRestore();
        }
    }

    void generateBinary(ParseTree node)
    {
        generate(node.children[0]);
        if (node.children.length > 2)
        {
            emitRM("ST", ac, tmpOffset--, mp, "op: push left");
            generate(node.children[2]);
            emitRM("LD", ac1, ++tmpOffset, mp, "op: load left");

            final switch (node.children[1].matches[0])
            {
                case "+":
                    emitRO("ADD", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "-":
                    emitRO("SUB", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "~":
                    emitRO("CAT", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "*":
                    emitRO("MUL", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "/":
                    emitRO("DIV", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "%":
                    emitRO("MOD", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "^":
                    emitRO("POW", ac, ac1, ac, "op %s", node.children[1].matches[0]); 
                    break;

                case "or":
                    emitRO("OR", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "and":
                    emitRO("AND", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "<":
                    /*emitRO("SUB",ac,ac1,ac, "op %s", node.children[1].matches[0]);
                    emitRM("JLT",ac,2,pc,"br if true");
                    emitRM("LDI",ac,0,ac,"false case");
                    emitRM("LDA",pc,1,pc,"unconditional jmp");
                    emitRM("LDI",ac,1,ac,"true case");*/
                    emitRO("LT", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "=" :
                    /*emitRO("SUB",ac,ac1,ac, "op %s", node.children[1].matches[0]);
                    emitRM("JEQ",ac,2,pc,"br if true");
                    emitRM("LDI",ac,0,ac,"false case");
                    emitRM("LDA",pc,1,pc,"unconditional jmp");
                    emitRM("LDI",ac,1,ac,"true case");*/
                    emitRO("EQ", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "<=" :
                    /*emitRO("SUB",ac,ac1,ac, "op %s", node.children[1].matches[0]);
                    emitRM("JLE",ac,2,pc,"br if true");
                    emitRM("LDI",ac,0,ac,"false case");
                    emitRM("LDA",pc,1,pc,"unconditional jmp");
                    emitRM("LDI",ac,1,ac,"true case");*/
                    emitRO("LE", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case ">" :
                    /*emitRO("SUB",ac,ac1,ac, "op %s", node.children[1].matches[0]);
                    emitRM("JGT",ac,2,pc,"br if true");
                    emitRM("LDI",ac,0,ac,"false case");
                    emitRM("LDA",pc,1,pc,"unconditional jmp");
                    emitRM("LDI",ac,1,ac,"true case");*/
                    emitRO("GT", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case ">=" :
                    /*emitRO("SUB",ac,ac1,ac, "op %s", node.children[1].matches[0]);
                    emitRM("JGE",ac,2,pc,"br if true");
                    emitRM("LDI",ac,0,ac,"false case");
                    emitRM("LDA",pc,1,pc,"unconditional jmp");
                    emitRM("LDI",ac,1,ac,"true case");*/
                    emitRO("GE", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "!=" :
                    /*emitRO("SUB",ac,ac1,ac, "op %s", node.children[1].matches[0]);
                    emitRM("JNE",ac,2,pc,"br if true");
                    emitRM("LDI",ac,0,ac,"false case");
                    emitRM("LDA",pc,1,pc,"unconditional jmp");
                    emitRM("LDI",ac,1,ac,"true case");*/
                    emitRO("NE", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "in":
                    emitRO("IN", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    break;

                case "!in":
                    emitRO("IN", ac, ac1, ac, "op %s", node.children[1].matches[0]);
                    emitRO("NOT", ac, ac, ac);
                    break;
            }
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
 
	void emitComment(Char, A...)(in Char[] fmt, A args)
	{
        if (fmt != "")
        {
            formatCode("        // ");
            formatCode(fmt, args);
        }
        formatCode("\n");
	}

    void appendComment(Char, A...)(in Char[] fmt, A args)
	{
        if (fmt != "")
        {
            --emitLoc;
            formatCode(" // ");
            formatCode(fmt, args);
            ++emitLoc;
        }
        //formatCode("\n");
	}

    void emit(O, R, S, T)(O op, R r, S s, T t)
    {
        emitCode("%5s %s, %s, %s", op, r, s, t);
    }

    void emit(O, R, S)(O op, R r, S s)
    {
        emitCode("%5s %s, %s", op, r, s);
    }

    void emit(O, R)(O op, R r)
    {
        emitCode("%5s %s", op, r);
    }

    void emit(O)(O op)
    {
        emitCode("%5s", op);
    }

    void emitCode(Char, A...)(in Char[] fmt, A args)
	{
        formatCode("%3d: ", emitLoc);
        formatCode(fmt, args);
        emitLoc++;
        updateHighEmitLoc();
	}

    void emitRO(string op)
	{
		emitCode("%5s");
	}

    void emitRO(string op, int r, int s, int t)
	{
		emitCode("%5s %s, %s, %s", op, r, s, t);
        //updateHighEmitLoc();
	}

	void emitRO(Char, A...)(string op, int r, int s, int t, in Char[] fmt, A args)
	{
		emitRO(op, r, s, t);
        appendComment(fmt, args);
	}

    void emitRM(T)(string op, int r, T d, int s) if (!isSomeString!T)
	{
		emitCode("%5s %s, %s(%s)", op, r, d, s);
        //updateHighEmitLoc();
	}

    void emitRM(T)(string op, int r, T d, int s) if (isSomeString!T)
	{
		emitCode("%5s %s, \"%s\"(%s)", op, r, d, s);
        //updateHighEmitLoc();
	}

	void emitRM(T, Char, A...)(string op, int r, T d, int s, in Char[] fmt, A args)
	{
		emitRM(op, r, d, s);
        appendComment(fmt, args);
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

    void emitAbs(O, R)(O op, R r, int loc)
    {
        auto addr = format("%d(%d)", loc - (emitLoc + 1), RegisterType.Literal);
        emit(op, r, addr);
    }

    void emitAbs(O)(O op, int loc)
    {
        auto addr = format("%d(%d)", loc - (emitLoc + 1), RegisterType.Literal);
        emit(op, addr);
    }

	void emitRM_Abs(Char, A...)(string op, int r, int a, in Char[] fmt, A args)
	{
		emitCode("%5s %d, %d(%d)", op, r, a-(emitLoc+1), pc);
        appendComment(fmt, args);
        //updateHighEmitLoc();
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
	Generator generator = Generator(node);
	return generator.source;
}
