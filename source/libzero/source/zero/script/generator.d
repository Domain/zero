module zero.script.generator;

import pegged.peg;
import std.array : join;
import std.format : format;
import std.stdio : writeln, writefln;
import std.conv;
import std.traits : isSomeString;
import std.container.slist;
import zero.script.allocator;
import zero.script.analyzer;
import std.bitmanip;
import msgpack;

struct Generator
{
public:
    this(SyntaxTree root)
	{
        allocator = Allocator();
        auto code = emitSkip(1);
		generate(root);
        auto end = emitSkip(0);
		emit(InstructionSet.HALT);
        emitBackup(code);
        emit(InstructionSet.CODE, end);
        emitRestore();
        generateData();

        Instruction64 ins;
        ins.code = InstructionSet.MOV;
        ins.operand = 112233;
        writefln("size = %s", ins.sizeof);
        writefln("operand = %b", ins.operand);
        writefln("value = %b", ins.value);
        writefln("instruction = %s", ins.instruction);
        writefln("op = %s", ins.op);

        writefln("code = %s", Instruction64.code.offsetof);
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

    enum InstructionSet : ubyte
    {
        /*  5 */    HALT = 0, ADD, SUB, MUL, DIV, 
        /* 10 */    MOD, POW, NEG, CAT, IN,
        /* 15 */    AND, OR, NOT, MOV, CALL, 
        /* 20 */    PROC, RET, JMP, JEQ, JNE, 
        /* 25 */    LT, LE, EQ, NE, GT, 
        /* 30 */    GE, DATA, GET, TAB, ARRY,
        /* 35 */    CODE, MOVA, MOVT
    }

    union Instruction64
    {
        ulong instruction;
        struct
        {
            union
            {
                uint operand;
                struct
                {
                    ushort cx;
                    ushort bx;
                }
            }
            ushort ax;
            mixin(bitfields!(ubyte, "eax", 2,
                             ubyte, "ebx", 3,
                             ubyte, "ecx", 3));
            InstructionSet code;
        }
        mixin(bitfields!(ulong, "value", 56,
                         ulong, "op", 8));
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
        R0 r0;
        R1 r1;
        R3 r3;
    }

    enum LiteralType
    {
        Integer, Other
    }

    union Literal
    {
        byte number;
        mixin(bitfields!(
                         byte, "value", 6,
                         LiteralType, "type", 1,
                         byte, "", 1
                         ));
    }

    immutable byte maxLiteral = cast(byte)0b0001_1111;
    immutable byte minLiteral = cast(byte)0b1001_1111;
    immutable Literal nullLiteral  = { 0b0100_0000 };
    immutable Literal trueLiteral  = { 0b0100_0001 };
    immutable Literal falseLiteral = { 0b0100_0010 };
    immutable Literal real1Literal = { 0b0100_0011 };
    immutable Literal thisLiteral  = { 0b0100_0100 };
    immutable Literal emptyLiteral = { 0b0100_0101 };
    
	int emitLoc = 0;
	int highEmitLoc = 0;
	int tmpOffset = 0;

    string[] buffers;
    ulong[] codes;

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
                    emit("", "\"" ~ value.as!string ~ "\"");
                    break;

                case Value.Type.floating:
                    emit("", value.as!double);
                    break;

                default:
                    break;
            }
        }
    }

	void generate(SyntaxTree node)
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

            case "Zero.ExpressionStatement":
                generateExpr(node.children[0]);
                break;

			default:
				foreach (child; node.children)
					generate(child);
				break;
		}
	}

    Register generateExpr(SyntaxTree node, Register reg = Register.Invalid)
    {
        Register result;

        switch (node.name)
		{
            case "Zero.AssignExpr":
                result = assign(node, reg);
                break;

            case "Zero.TernaryExpr":
                result = generateTernary(node, reg);
                break;

			case "Zero.AddExpr":
			case "Zero.MulExpr":
			case "Zero.CompareExpr":
			case "Zero.OrExpr":
			case "Zero.AndExpr":
            case "Zero.PowExpr":
                result = generateBinary(node, reg);
                break;

			case "Zero.NotExpr":
				result = generateExpr(node.children[0], reg);
                emit(InstructionSet.NOT, result, result);
				break;

            case "Zero.SignExpr":
				result = generateExpr(node.children[1], reg);
                if (node.matches[0] == "-")
				    emit(InstructionSet.NEG, result, result);
                break;

			case "Zero.VarIdentifier":
			case "Zero.Symbol":
                auto var = allocator.FindVariable(node.matches[0]);
                if (var !is null)
                    result = var.toRegister();
				break;

			case "Zero.Decimal":
				auto value = to!long(node.matches[0]);
                result = literal(value, reg);
				break;

			case "Zero.Hexadecimal":
				auto value = to!long(node.matches[0], 16);
                result = literal(value, reg);
				break;

            case "Zero.Octal":
				auto value = to!long(node.matches[0], 8);
                result = literal(value, reg);
				break;

			case "Zero.Binary":
				auto value = to!long(node.matches[0], 2);
                result = literal(value, reg);
				break;

			case "Zero.RealLiteral":
				auto value = to!double(node.matches[0]);
                result = literal(value, reg);
				break;

            case "Zero.RawString":
            case "Zero.QuotedString":
                auto value = node.matches[0][1..$-1];
                result = literal(value, reg);
                break;

			case "Zero.CallExpr":
                result = generateCall(node);
				break;

            case "Zero.True":
                result = literalRegister(trueLiteral);
                break;

            case "Zero.False":
                result = literalRegister(falseLiteral);
                break;

            case "Zero.Null":
                result = literalRegister(nullLiteral);
                break;

            case "Zero.This":
                result = literalRegister(thisLiteral);
                break;

            case "Zero.MemberCall":
                result = generateMember(node, reg);
                break;

            case "Zero.TableIndex":
            case "Zero.ArrayIndex":
                result = index(node, reg);
                break;

            case "Zero.ArrayExpr":
                result = arrayExpr(node, reg);
                break;

            case "Zero.TableExpr":
                result = tableExpr(node, reg);
                break;

			default:
				foreach (child; node.children)
					generate(child);
				break;
		}

        return result;
    }

    Register useOrNewReg(Register reg)
    {
        auto result = reg == Register.Invalid ? allocator.AllocateRegister() : reg;
        return result;
    }

    Register arrayExpr(SyntaxTree node, Register reg)
    {
        auto result = useOrNewReg(reg);
        emit(InstructionSet.ARRY, result, node.children.length);
        auto index = allocator.AllocateRegister();
        foreach (i, child; node.children)
        {
            emit(InstructionSet.GET, index, result, literal(i));
            emit(InstructionSet.MOV, index, generateExpr(child));
        }
        return result;
    }

    Register tableExpr(SyntaxTree node, Register reg)
    {
        auto result = useOrNewReg(reg);
        emit(InstructionSet.TAB, result, node.children.length);
        auto index = allocator.AllocateRegister();
        int value = 0;
        foreach (child; node.children)
        {
            index = generateExpr(child.children[0], index);
            emit(InstructionSet.GET, index, result, index);
            if (child.children.length > 1)
                emit(InstructionSet.MOV, index, generateExpr(child.children[1]));
            else
                emit(InstructionSet.MOV, index, literal(value));
        }
        return result;
    }

    Register assign(SyntaxTree node, Register reg)
    {
        auto result = generateExpr(node.children[0]);
        auto value = generateExpr(node.children[1], result);
        if (value != result)
            emit(InstructionSet.MOV, result, value);
        return result;
    }

    Register index(SyntaxTree node, Register reg)
    {
        auto o = generateExpr(node.children[0], reg);
        auto i = generateExpr(node.children[1]);
        auto result = useOrNewReg(reg);
        emit(InstructionSet.GET, result, o, i);
        return result;
    }

    Register generateMember(SyntaxTree node, Register reg)
    {
        auto o = generateExpr(node.children[0], reg);
        auto f = allocator.AllocateLiteral(node.children[1].matches[0]);
        auto result = useOrNewReg(reg);
        emit(InstructionSet.GET, result, o, f);
        return result;
    }

    Register literal(T)(T value, Register reg)
    {
        //auto c = allocator.AllocateLiteral(value);
        //auto result = reg == Register.Invalid ? allocator.AllocateRegister() : reg;
        //emit(InstructionSet.MOV, result, c);
        return allocator.AllocateLiteral(value);
    }

    Register literal(long value, Register reg = Register.Invalid)
    {
        if (value <= maxLiteral && value >= minLiteral)
        {
            Literal l;
            l.type = LiteralType.Integer;
            l.value = cast(byte)value;
            return Register(l.number, RegisterType.Literal);
        }

        //auto c = allocator.AllocateLiteral(value);
        //auto result = reg == Register.Invalid ? allocator.AllocateRegister() : reg;
        //emit(InstructionSet.MOV, result, c);
        return allocator.AllocateLiteral(value);
    }

    Register literalRegister(Literal literal)
    {
        return Register(literal.number, RegisterType.Literal);
    }

    void generateBlock(SyntaxTree node)
    {
        allocator.EnterScope();
        foreach (child; node.children)
        {
            generate(child);
        }
        allocator.LeaveScope();
    }

    void generateVar(SyntaxTree node)
    {
        EVarScope varScope = EVarScope.Local;

        if (node.matches[0] == "global")
        {
            varScope = EVarScope.Global;
        }

        foreach (decl; node.children)
        {
            auto var = allocator.AllocateVariable(decl.matches[0], varScope);
            if (decl.name == "Zero.VarDeclaration")
            {
                auto value = generateExpr(decl.children[1], var.toRegister());
                if (var != value)
                //if (value.type == RegisterType.Literal ||
                //    value.type == RegisterType.Const)
                    emit(InstructionSet.MOV, var, value);
            }
        }
    }

    void generateIf(SyntaxTree node)
    {
        emitComment("----> if");
        // condition
        Register cond;

        if (node.children[0].node.name == "Zero.Condition")
        {
            generate(node.children[0].children[0]);
            cond = generateExpr(node.children[0].children[1]);
        }
        else
        {
            cond = generateExpr(node.children[0]);
        }

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

    void generateRepeat(SyntaxTree node)
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

    void generateWhile(SyntaxTree node)
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

    void generateFor(SyntaxTree node)
    {
        BackPatchHelper helper = BackPatchHelper("break", &this);

        auto cond = node.children[0];
        auto stmt = node.children[1];

        auto init = cond.children[0];
        auto stop = cond.children[1];

        emitComment("----> for");
        generate(init);
        beginLoc = emitSkip(0);
        auto stopCond = generateExpr(stop);
        auto jmp2EndLoc = emitSkip(1);
        generate(stmt);
        if (cond.children.length > 2)
            generateExpr(cond.children[2]);
        emitAbs(InstructionSet.JMP, beginLoc);
        endLoc = emitSkip(0);
        emitBackup(jmp2EndLoc);
        emitAbs(InstructionSet.JNE, stopCond, endLoc);
        //emitRM_Abs("JEQ", ac, endLoc, "for: jmp to end");
        emitRestore();
        emitComment("<---- for");
    }

    void generateForeach(SyntaxTree node)
    {
        BackPatchHelper helper = BackPatchHelper("break", &this);

    }

    void generateBreak(SyntaxTree node)
    {
        BackPatchHelper.AddLocation(emitSkip(1));
    }

    void generateContinue(SyntaxTree node)
    {
        //emitRM_Abs("LDA", pc, beginLoc, "continue");
        emitAbs(InstructionSet.JMP, beginLoc);
    }

    void generateReturn(SyntaxTree node)
    {
        if (node.children.length > 0)
        {
            auto result = generateExpr(node.children[0]);
            emit(InstructionSet.RET, 1, result);
        }
        else
        {
            emit(InstructionSet.RET, 0, 0);
        }
        //BackPatchHelper.AddLocation(emitSkip(1));
    }

    void generateFunction(SyntaxTree node)
    {
        BackPatchHelper helper = BackPatchHelper("return", &this);

        allocator.AllocateFunction(node.matches[0], emitLoc);

        emitComment("----> function");
        auto func = emitSkip(1);

        allocator.EnterFunction();
        //emitCode("%5s %s", "PROC", node.matches[0]);
        //appendComment("function %s", node.matches[0]);

        generateVar(node.children[0]);

        auto stmt = node.children[1];
        if (node.children.length == 3)
            stmt = node.children[2];

        generate(stmt);
        emit(InstructionSet.RET, 0, 0);
        endLoc = emitSkip(0);
        //emitCode("%5s", "RET");
        emitComment("<---- function");
        emitBackup(func);
        emit(InstructionSet.PROC, endLoc - func);
        appendComment("function %s", node.matches[0]);
        emitRestore();

        allocator.LeaveFunction();
    }

    Register generateCall(SyntaxTree node)
    {
        auto result = allocator.AllocateRegister();
        auto f = generateExpr(node.children[0], result);
        foreach (child; node.children[1].children)
        {
            auto p = allocator.AllocateRegister();
            auto pp = generateExpr(child, p);
            if (pp != p)
            {
                emit(InstructionSet.MOV, p, pp);
            }
        }
        emit(InstructionSet.CALL, f, node.children[1].children.length);
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
        return result;
    }

    Register generateTernary(SyntaxTree node, Register reg)
    {
        auto result = reg == Register.Invalid ? allocator.AllocateRegister() : reg;
        auto cond = generateExpr(node.children[0]);
        auto jmp2SecondLoc = emitSkip(1);
        auto first = generateExpr(node.children[1]);
        emit(InstructionSet.MOV, result, first);
        auto jmp2EncLoc = emitSkip(1);
        auto secondLoc = emitSkip(0);
        auto second = generateExpr(node.children[2]);
        emit(InstructionSet.MOV, result, second);
        auto endLoc = emitSkip(0);
        emitBackup(jmp2SecondLoc);
        //emitRM_Abs("JEQ", ac, secondLoc, "jmp to second");
        emitAbs(InstructionSet.JNE, cond, secondLoc);
        emitBackup(jmp2EncLoc);
        //emitRM_Abs("LDA", pc, endLoc, "jmp to end");
        emitAbs(InstructionSet.JMP, endLoc);
        emitRestore();
        return result;
    }

    Register generateBinary(SyntaxTree node, Register reg)
    {
        auto lhs = generateExpr(node.children[0], reg);
        auto rhs = generateExpr(node.children[2]);
        auto result = reg == Register.Invalid ? allocator.AllocateRegister() : reg;

        auto op = InstructionSet.ADD;
        auto isNotIn = false;
        final switch (node.children[1].matches[0])
        {
            case "+":
                op = InstructionSet.ADD;
                break;

            case "-":
                op = InstructionSet.SUB;
                break;

            case "~":
                op = InstructionSet.CAT;
                break;

            case "*":
                op = InstructionSet.MUL;
                break;

            case "/":
                op = InstructionSet.DIV;
                break;

            case "%":
                op = InstructionSet.MOD;
                break;

            case "^":
                op = InstructionSet.POW;
                break;

            case "or":
                op = InstructionSet.OR;
                break;

            case "and":
                op = InstructionSet.AND;
                break;

            case "<":
                op = InstructionSet.LT;
                break;

            case "=" :
                op = InstructionSet.EQ;
                break;

            case "<=" :
                op = InstructionSet.LE;
                break;

            case ">" :
                op = InstructionSet.GT;
                break;

            case ">=" :
                op = InstructionSet.GE;
                break;

            case "!=" :
                op = InstructionSet.NE;
                break;

            case "in":
                op = InstructionSet.IN;
                break;

            case "!in":
                op = InstructionSet.IN;
                isNotIn = true;
                break;
        }

        emit(op, result, lhs, rhs);
        if (isNotIn)
            emit(InstructionSet.NOT, result, result);

        return result;
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
            formatCode("        ; ");
            formatCode(fmt, args);
        }
        formatCode("\n");
	}

    void appendComment(Char, A...)(in Char[] fmt, A args)
	{
        if (fmt != "")
        {
            --emitLoc;
            formatCode(" ; ");
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

    //void emitRO(string op)
    //{
    //    emitCode("%5s");
    //}
    //
    //void emitRO(string op, int r, int s, int t)
    //{
    //    emitCode("%5s %s, %s, %s", op, r, s, t);
    //    //updateHighEmitLoc();
    //}
    //
    //void emitRO(Char, A...)(string op, int r, int s, int t, in Char[] fmt, A args)
    //{
    //    emitRO(op, r, s, t);
    //    appendComment(fmt, args);
    //}
    //
    //void emitRM(T)(string op, int r, T d, int s) if (!isSomeString!T)
    //{
    //    emitCode("%5s %s, %s(%s)", op, r, d, s);
    //    //updateHighEmitLoc();
    //}
    //
    //void emitRM(T)(string op, int r, T d, int s) if (isSomeString!T)
    //{
    //    emitCode("%5s %s, \"%s\"(%s)", op, r, d, s);
    //    //updateHighEmitLoc();
    //}
    //
    //void emitRM(T, Char, A...)(string op, int r, T d, int s, in Char[] fmt, A args)
    //{
    //    emitRM(op, r, d, s);
    //    appendComment(fmt, args);
    //}

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
        auto addr = format("%d(%s)", loc - (emitLoc + 1), RegisterType.Literal);
        emit(op, r, addr);
        appendComment("%d", loc);
    }

    void emitAbs(O)(O op, int loc)
    {
        auto addr = format("%d(%s)", loc - (emitLoc + 1), RegisterType.Literal);
        emit(op, addr);
        appendComment("%d", loc);
    }

    //void emitRM_Abs(Char, A...)(string op, int r, int a, in Char[] fmt, A args)
    //{
    //    emitCode("%5s %d, %d(%d)", op, r, a-(emitLoc+1), pc);
    //    appendComment(fmt, args);
    //    //updateHighEmitLoc();
    //}
}

void traverse(SyntaxTree node, void delegate(SyntaxTree) prevProc, void delegate(SyntaxTree) postProc)
{
	prevProc(node);
	foreach (child; node.children)
	{
		traverse(child, prevProc, postProc);
	}
	postProc(node);
}

string generate(SyntaxTree node)
{
	Generator generator = Generator(node);
	return generator.source;
}
