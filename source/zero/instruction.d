module zero.instruction;

enum InstructionSet : ubyte
{
    /*  5 */ HALT = 0, ADD, SUB, MUL, DIV,
    /* 10 */ MOD, POW, NEG, CAT, IN,
    /* 15 */ AND, OR, NOT, MOV, CALL,
    /* 20 */ PROC, RET, JMP, JEQ, JNE,
    /* 25 */ LT, LE, EQ, NE, GT,
    /* 30 */ GE, DATA, GET, TAB, ARRY,
    /* 35 */ CODE, MOVA, MOVT
}

struct RA
{
    mixin(bitfields!(
        ushort, "", 16, 
        ushort, "", 16, 
        ushort, "value", 16,
        ubyte, "type", 2, 
        ubyte, "", 3, 
        ubyte, "", 3, 
        ubyte, "", 8)
    );
}

struct RB
{
    mixin(bitfields!(
        ushort, "", 16, 
        ushort, "value", 16, 
        ushort, "", 16,
        ubyte, "", 2, 
        ubyte, "type", 3, 
        ubyte, "", 3, 
        ubyte, "", 8)
    );
}

struct RC
{
    mixin(bitfields!(
        short, "value", 16, 
        ushort, "", 16, 
        ushort, "", 16,
        ubyte, "", 2, 
        ubyte, "", 3, 
        ubyte, "type", 3, 
        ubyte, "", 8)
    );
}

struct RD
{
    mixin(bitfields!(
        uint, "value", 32, 
        ushort, "", 16, 
        ubyte, "", 2, 
        ubyte, "type", 6, 
        ubyte, "", 8)
    );
}

struct RE
{
    mixin(bitfields!(
        ulong, "value", 56, 
        ubyte, "", 8)
    );
}

union Instruction
{
    ulong code;

    RA ax;
    RB bx;
    RC cx;
    RD dx;
    RE ex;

    mixin(bitfields!(
        ulong, "", 56, 
        ulong, "op", 8)
    );
}
