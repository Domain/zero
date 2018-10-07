/++
This module was automatically generated from the following grammar:


	Zero:
		### Statement ###
		Program < Statement* :eoi
		Statement <- NonEmptyStatement / EmptyStatement
		EmptyStatement < ';'
		NonEmptyStatement < BlockStatement / 
			VarStatement / 
			ConstStatement /
			FunctionStatement / 
			IfStatement / 
			WhileStatement / 
			RepeatStatement / 
			ForStatement / 
			ForeachStatement /  
			ReturnStatement / 
			BreakStatement / 
			ContinueStatement / 
			ExpressionStatement 
		BlockStatement < '{' Statement* '}'
		IfStatement < :'if' Expression :'then' Statement ( :'else' Statement )?
		WhileStatement < :'while' Expression :'do' Statement
		RepeatStatement < :'repeat' Statement :'until' Expression :';'
		ForStatement < :'for' :'(' ForExpression :')' :'do' Statement
        ForExpression < Initialize Expression :';' Increment?
		Initialize < Statement
		Increment < Expression
		ForeachStatement < :'foreach' :'(' :'var' VarIdentifier :';' ForeachRange :')' :'do' Statement
        ForeachRange < Expression ('..' Expression)?
		ReturnStatement < 'return' Expression? :';'
		BreakStatement < 'break' :';'
		ContinueStatement < 'continue' :';'
		ExpressionStatement < Expression :';'
		VarStatement < 'global'? :'var' VarDeclarationList :';'
		VarDeclarationList < VarExpression (:',' VarExpression)*
		VarExpression < VarDeclaration (:':=' Expression)?
		VarDeclaration <- identifier
		ConstStatement < 'global'? :'const' ConstDeclarationList :';'
		ConstDeclarationList < ConstDeclaration (:',' ConstDeclaration)*
		ConstDeclaration < Identifier :':=' Expression
		FunctionStatement < :'function' identifier Parameters FunctionAttributes Statement
		Parameters < '(' ParameterList? ')'
		ParameterList < Parameter (:',' Parameter)*
		Parameter < 'var' VarExpression
		FunctionAttributes < FunctionAttribute*
        FunctionAttribute <- 'override'
		### Expression ###
		Expression < AssignExpr 
		AssignExpr < TernaryExpr ( :':=' TernaryExpr )? 
		TernaryExpr < OrExpr ( :'?' TernaryExpr :':' TernaryExpr )?
		OrExpr < (OrExpr 'or')? AndExpr
		AndExpr < (AndExpr 'and')? CompareExpr
		CompareExpr < AddExpr (CompareOp AddExpr)?
		CompareOp <- '<=' / '<' / '=' / '!=' / '>=' / '>' / 'in' / '!in'
		AddExpr < (AddExpr AddOp)? MulExpr
		AddOp < '+' / '-' / '~'
		MulExpr < (MulExpr MulOp)? UnaryExpr
		MulOp < '*' / '/' / '%'
		UnaryExpr < NotExpr / PowExpr / SignExpr
		NotExpr < :'not' UnaryExpr
		SignExpr < Sign UnaryExpr
		PowExpr < PostExpr ('^' UnaryExpr)?
		PostExpr < TableIndex / ArrayIndex / CallExpr / MemberCall / PrimaryExpr
		MemberCall <- PostExpr :'.' Identifier
		TableIndex < PostExpr :'{' Expression :'}'
		ArrayIndex < PostExpr :'[' Expression :']'
		CallExpr < PostExpr Argument
        Argument < '(' ArgumentList? ')'
		ArgumentList < Expression (:',' Expression)*
		PrimaryExpr < :'(' Expression :')' / 
			ArrayExpr / 
			TableExpr / 
			ConstExpr /
			RealLiteral / 
			IntegerLiteral / 
			String / 
			VarIdentifier / 
			This / 
			Null / 
			True / 
			False / 
			Lambda
		ArrayExpr < '[' ArrayElement? :','? ']'
		ArrayElement < Expression (:',' Expression)*
		TableExpr < '{' TableElementList? :','? '}'
		TableElementList < TableElement (:',' TableElement)*
		TableElement < TernaryExpr (:':=' Expression)?
		ConstExpr < Identifier
		VarIdentifier <- Identifier
		
		IntegerLiteral <- Binary / Hexadecimal / Octal / Decimal
		Decimal <~ :('0'[dD])? Integer / Sign? Integer
		Binary <~ :('0'[bB]) [01]([01] / :'\'')*
		Hexadecimal <~ :('0'[xX]) HexDigit (HexDigit / '\'')*
		HexDigit < [0-9a-fA-F]
		Octal <~ :('0'[cC]) OctalDigit (OctalDigit / '\'')*
		OctalDigit < [0-7]
		Sign <- '+' / '-'
		Integer <~ digit (digit / :'\'')*
		RealLiteral <~ Sign? Integer '.' Integer ([eE] Sign? Integer)? / Sign? Integer [eE] Sign? Integer
		String < RawString / QuotedString
		RawString <~ backquote (!backquote .)* backquote
		QuotedString <~ doublequote (DQChar)* doublequote
		DQChar <- EscapeSequence / !doublequote .
		EscapeSequence <- backslash (doublequote / backslash / [nrt])
		This < '$'
		Null < 'null'
		True < 'true'
		False < 'false'
		Lambda < CapturedList Parameters BlockStatement
		CapturedList < '<' CapturedVar? '>'
		CapturedVar < (RenamedVar / VarIdentifier) (:',' CapturedVar)*
		RenamedVar < :'var' VarIdentifier :':=' Expression
		Identifier <~ !Keyword [a-zA-Z_$][a-zA-Z0-9_$]*
		Keyword < (
                'if' / 
                'else' / 
                'while' / 
                'repeat' / 
                'until' / 
                'for' / 
                'foreach' / 
                'break' / 
			    'continue' / 
                'switch' / 
                'case' / 
                'var' / 
                'global' / 
                'return' / 
                'function' /
			    'override' / 
                'null' / 
                'and' / 
                'or' / 
                'not' / 
                'true' / 
                'false' / 
                'in' /
                'do') ![a-zA-Z0-9_]
		Spacing <: (blank / Comment)*
		Comment <: BlockComment / LineComment
		BlockComment <~ :'/*' (!'*/' .)* :'*/'
		LineComment <~ :'//' (!endOfLine .)* :endOfLine
		

+/
module zero.parser;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

/** Left-recursive cycles:
OrExpr
PostExpr <- TableIndex
PostExpr <- ArrayIndex
CallExpr <- PostExpr
PostExpr <- MemberCall
AndExpr
AddExpr
MulExpr
*/

/** Rules that stop left-recursive cycles, followed by rules for which
 *  memoization is blocked during recursion:
OrExpr: OrExpr, PostExpr, TableIndex, ArrayIndex, CallExpr, MemberCall, AndExpr, AddExpr, MulExpr
PostExpr: OrExpr, PostExpr, TableIndex, ArrayIndex, CallExpr, MemberCall, AndExpr, AddExpr, MulExpr
CallExpr: OrExpr, PostExpr, TableIndex, ArrayIndex, CallExpr, MemberCall, AndExpr, AddExpr, MulExpr
AndExpr: OrExpr, PostExpr, TableIndex, ArrayIndex, CallExpr, MemberCall, AndExpr, AddExpr, MulExpr
AddExpr: OrExpr, PostExpr, TableIndex, ArrayIndex, CallExpr, MemberCall, AndExpr, AddExpr, MulExpr
MulExpr: OrExpr, PostExpr, TableIndex, ArrayIndex, CallExpr, MemberCall, AndExpr, AddExpr, MulExpr
*/

struct GenericZero(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct Zero
    {
    enum name = "Zero";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    import std.algorithm: canFind, countUntil, remove;
    static size_t[] blockMemoAtPos;
    static this()
    {
        rules["Program"] = toDelegate(&Program);
        rules["Statement"] = toDelegate(&Statement);
        rules["EmptyStatement"] = toDelegate(&EmptyStatement);
        rules["NonEmptyStatement"] = toDelegate(&NonEmptyStatement);
        rules["BlockStatement"] = toDelegate(&BlockStatement);
        rules["IfStatement"] = toDelegate(&IfStatement);
        rules["WhileStatement"] = toDelegate(&WhileStatement);
        rules["RepeatStatement"] = toDelegate(&RepeatStatement);
        rules["ForStatement"] = toDelegate(&ForStatement);
        rules["ForExpression"] = toDelegate(&ForExpression);
        rules["Initialize"] = toDelegate(&Initialize);
        rules["Increment"] = toDelegate(&Increment);
        rules["ForeachStatement"] = toDelegate(&ForeachStatement);
        rules["ForeachRange"] = toDelegate(&ForeachRange);
        rules["ReturnStatement"] = toDelegate(&ReturnStatement);
        rules["BreakStatement"] = toDelegate(&BreakStatement);
        rules["ContinueStatement"] = toDelegate(&ContinueStatement);
        rules["ExpressionStatement"] = toDelegate(&ExpressionStatement);
        rules["VarStatement"] = toDelegate(&VarStatement);
        rules["VarDeclarationList"] = toDelegate(&VarDeclarationList);
        rules["VarExpression"] = toDelegate(&VarExpression);
        rules["VarDeclaration"] = toDelegate(&VarDeclaration);
        rules["ConstStatement"] = toDelegate(&ConstStatement);
        rules["ConstDeclarationList"] = toDelegate(&ConstDeclarationList);
        rules["ConstDeclaration"] = toDelegate(&ConstDeclaration);
        rules["FunctionStatement"] = toDelegate(&FunctionStatement);
        rules["Parameters"] = toDelegate(&Parameters);
        rules["ParameterList"] = toDelegate(&ParameterList);
        rules["Parameter"] = toDelegate(&Parameter);
        rules["FunctionAttributes"] = toDelegate(&FunctionAttributes);
        rules["FunctionAttribute"] = toDelegate(&FunctionAttribute);
        rules["Expression"] = toDelegate(&Expression);
        rules["AssignExpr"] = toDelegate(&AssignExpr);
        rules["TernaryExpr"] = toDelegate(&TernaryExpr);
        rules["OrExpr"] = toDelegate(&OrExpr);
        rules["AndExpr"] = toDelegate(&AndExpr);
        rules["CompareExpr"] = toDelegate(&CompareExpr);
        rules["CompareOp"] = toDelegate(&CompareOp);
        rules["AddExpr"] = toDelegate(&AddExpr);
        rules["AddOp"] = toDelegate(&AddOp);
        rules["MulExpr"] = toDelegate(&MulExpr);
        rules["MulOp"] = toDelegate(&MulOp);
        rules["UnaryExpr"] = toDelegate(&UnaryExpr);
        rules["NotExpr"] = toDelegate(&NotExpr);
        rules["SignExpr"] = toDelegate(&SignExpr);
        rules["PowExpr"] = toDelegate(&PowExpr);
        rules["PostExpr"] = toDelegate(&PostExpr);
        rules["MemberCall"] = toDelegate(&MemberCall);
        rules["TableIndex"] = toDelegate(&TableIndex);
        rules["ArrayIndex"] = toDelegate(&ArrayIndex);
        rules["CallExpr"] = toDelegate(&CallExpr);
        rules["Argument"] = toDelegate(&Argument);
        rules["ArgumentList"] = toDelegate(&ArgumentList);
        rules["PrimaryExpr"] = toDelegate(&PrimaryExpr);
        rules["ArrayExpr"] = toDelegate(&ArrayExpr);
        rules["ArrayElement"] = toDelegate(&ArrayElement);
        rules["TableExpr"] = toDelegate(&TableExpr);
        rules["TableElementList"] = toDelegate(&TableElementList);
        rules["TableElement"] = toDelegate(&TableElement);
        rules["ConstExpr"] = toDelegate(&ConstExpr);
        rules["VarIdentifier"] = toDelegate(&VarIdentifier);
        rules["IntegerLiteral"] = toDelegate(&IntegerLiteral);
        rules["Decimal"] = toDelegate(&Decimal);
        rules["Binary"] = toDelegate(&Binary);
        rules["Hexadecimal"] = toDelegate(&Hexadecimal);
        rules["HexDigit"] = toDelegate(&HexDigit);
        rules["Octal"] = toDelegate(&Octal);
        rules["OctalDigit"] = toDelegate(&OctalDigit);
        rules["Sign"] = toDelegate(&Sign);
        rules["Integer"] = toDelegate(&Integer);
        rules["RealLiteral"] = toDelegate(&RealLiteral);
        rules["String"] = toDelegate(&String);
        rules["RawString"] = toDelegate(&RawString);
        rules["QuotedString"] = toDelegate(&QuotedString);
        rules["DQChar"] = toDelegate(&DQChar);
        rules["EscapeSequence"] = toDelegate(&EscapeSequence);
        rules["This"] = toDelegate(&This);
        rules["Null"] = toDelegate(&Null);
        rules["True"] = toDelegate(&True);
        rules["False"] = toDelegate(&False);
        rules["Lambda"] = toDelegate(&Lambda);
        rules["CapturedList"] = toDelegate(&CapturedList);
        rules["CapturedVar"] = toDelegate(&CapturedVar);
        rules["RenamedVar"] = toDelegate(&RenamedVar);
        rules["Identifier"] = toDelegate(&Identifier);
        rules["Keyword"] = toDelegate(&Keyword);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(name,rule; dg.rules)
        {
            if (name != "Spacing")
                rules[name] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
		import std.algorithm : startsWith;
        return s.startsWith("Zero.");
    }
    mixin decimateTree;

    static TParseTree Program(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, eoi, Spacing))), "Zero.Program")(p);
        }
        else
        {
            if (auto m = tuple(`Program`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, eoi, Spacing))), "Zero.Program"), "Program")(p);
                memo[tuple(`Program`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Program(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, eoi, Spacing))), "Zero.Program")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, eoi, Spacing))), "Zero.Program"), "Program")(TParseTree("", false,[], s));
        }
    }
    static string Program(GetName g)
    {
        return "Zero.Program";
    }

    static TParseTree Statement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(NonEmptyStatement, EmptyStatement), "Zero.Statement")(p);
        }
        else
        {
            if (auto m = tuple(`Statement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(NonEmptyStatement, EmptyStatement), "Zero.Statement"), "Statement")(p);
                memo[tuple(`Statement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Statement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(NonEmptyStatement, EmptyStatement), "Zero.Statement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(NonEmptyStatement, EmptyStatement), "Zero.Statement"), "Statement")(TParseTree("", false,[], s));
        }
    }
    static string Statement(GetName g)
    {
        return "Zero.Statement";
    }

    static TParseTree EmptyStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), "Zero.EmptyStatement")(p);
        }
        else
        {
            if (auto m = tuple(`EmptyStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), "Zero.EmptyStatement"), "EmptyStatement")(p);
                memo[tuple(`EmptyStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EmptyStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), "Zero.EmptyStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), "Zero.EmptyStatement"), "EmptyStatement")(TParseTree("", false,[], s));
        }
    }
    static string EmptyStatement(GetName g)
    {
        return "Zero.EmptyStatement";
    }

    static TParseTree NonEmptyStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing), pegged.peg.wrapAround!(Spacing, VarStatement, Spacing), pegged.peg.wrapAround!(Spacing, ConstStatement, Spacing), pegged.peg.wrapAround!(Spacing, FunctionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, RepeatStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing)), "Zero.NonEmptyStatement")(p);
        }
        else
        {
            if (auto m = tuple(`NonEmptyStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing), pegged.peg.wrapAround!(Spacing, VarStatement, Spacing), pegged.peg.wrapAround!(Spacing, ConstStatement, Spacing), pegged.peg.wrapAround!(Spacing, FunctionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, RepeatStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing)), "Zero.NonEmptyStatement"), "NonEmptyStatement")(p);
                memo[tuple(`NonEmptyStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonEmptyStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing), pegged.peg.wrapAround!(Spacing, VarStatement, Spacing), pegged.peg.wrapAround!(Spacing, ConstStatement, Spacing), pegged.peg.wrapAround!(Spacing, FunctionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, RepeatStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing)), "Zero.NonEmptyStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing), pegged.peg.wrapAround!(Spacing, VarStatement, Spacing), pegged.peg.wrapAround!(Spacing, ConstStatement, Spacing), pegged.peg.wrapAround!(Spacing, FunctionStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, RepeatStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing)), "Zero.NonEmptyStatement"), "NonEmptyStatement")(TParseTree("", false,[], s));
        }
    }
    static string NonEmptyStatement(GetName g)
    {
        return "Zero.NonEmptyStatement";
    }

    static TParseTree BlockStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Zero.BlockStatement")(p);
        }
        else
        {
            if (auto m = tuple(`BlockStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Zero.BlockStatement"), "BlockStatement")(p);
                memo[tuple(`BlockStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BlockStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Zero.BlockStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Zero.BlockStatement"), "BlockStatement")(TParseTree("", false,[], s));
        }
    }
    static string BlockStatement(GetName g)
    {
        return "Zero.BlockStatement";
    }

    static TParseTree IfStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("then"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "Zero.IfStatement")(p);
        }
        else
        {
            if (auto m = tuple(`IfStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("then"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "Zero.IfStatement"), "IfStatement")(p);
                memo[tuple(`IfStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IfStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("then"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "Zero.IfStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("then"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), Spacing))), "Zero.IfStatement"), "IfStatement")(TParseTree("", false,[], s));
        }
    }
    static string IfStatement(GetName g)
    {
        return "Zero.IfStatement";
    }

    static TParseTree WhileStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.WhileStatement")(p);
        }
        else
        {
            if (auto m = tuple(`WhileStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.WhileStatement"), "WhileStatement")(p);
                memo[tuple(`WhileStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree WhileStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.WhileStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.WhileStatement"), "WhileStatement")(TParseTree("", false,[], s));
        }
    }
    static string WhileStatement(GetName g)
    {
        return "Zero.WhileStatement";
    }

    static TParseTree RepeatStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("repeat"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("until"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.RepeatStatement")(p);
        }
        else
        {
            if (auto m = tuple(`RepeatStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("repeat"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("until"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.RepeatStatement"), "RepeatStatement")(p);
                memo[tuple(`RepeatStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RepeatStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("repeat"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("until"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.RepeatStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("repeat"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("until"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.RepeatStatement"), "RepeatStatement")(TParseTree("", false,[], s));
        }
    }
    static string RepeatStatement(GetName g)
    {
        return "Zero.RepeatStatement";
    }

    static TParseTree ForStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, ForExpression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.ForStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ForStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, ForExpression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.ForStatement"), "ForStatement")(p);
                memo[tuple(`ForStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, ForExpression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.ForStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, ForExpression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.ForStatement"), "ForStatement")(TParseTree("", false,[], s));
        }
    }
    static string ForStatement(GetName g)
    {
        return "Zero.ForStatement";
    }

    static TParseTree ForExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing))), "Zero.ForExpression")(p);
        }
        else
        {
            if (auto m = tuple(`ForExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing))), "Zero.ForExpression"), "ForExpression")(p);
                memo[tuple(`ForExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing))), "Zero.ForExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing))), "Zero.ForExpression"), "ForExpression")(TParseTree("", false,[], s));
        }
    }
    static string ForExpression(GetName g)
    {
        return "Zero.ForExpression";
    }

    static TParseTree Initialize(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Statement, Spacing), "Zero.Initialize")(p);
        }
        else
        {
            if (auto m = tuple(`Initialize`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Statement, Spacing), "Zero.Initialize"), "Initialize")(p);
                memo[tuple(`Initialize`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Initialize(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Statement, Spacing), "Zero.Initialize")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Statement, Spacing), "Zero.Initialize"), "Initialize")(TParseTree("", false,[], s));
        }
    }
    static string Initialize(GetName g)
    {
        return "Zero.Initialize";
    }

    static TParseTree Increment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "Zero.Increment")(p);
        }
        else
        {
            if (auto m = tuple(`Increment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "Zero.Increment"), "Increment")(p);
                memo[tuple(`Increment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Increment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "Zero.Increment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "Zero.Increment"), "Increment")(TParseTree("", false,[], s));
        }
    }
    static string Increment(GetName g)
    {
        return "Zero.Increment";
    }

    static TParseTree ForeachStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.wrapAround!(Spacing, ForeachRange, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.ForeachStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ForeachStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.wrapAround!(Spacing, ForeachRange, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.ForeachStatement"), "ForeachStatement")(p);
                memo[tuple(`ForeachStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForeachStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.wrapAround!(Spacing, ForeachRange, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.ForeachStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.wrapAround!(Spacing, ForeachRange, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.ForeachStatement"), "ForeachStatement")(TParseTree("", false,[], s));
        }
    }
    static string ForeachStatement(GetName g)
    {
        return "Zero.ForeachStatement";
    }

    static TParseTree ForeachRange(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ForeachRange")(p);
        }
        else
        {
            if (auto m = tuple(`ForeachRange`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ForeachRange"), "ForeachRange")(p);
                memo[tuple(`ForeachRange`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForeachRange(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ForeachRange")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ForeachRange"), "ForeachRange")(TParseTree("", false,[], s));
        }
    }
    static string ForeachRange(GetName g)
    {
        return "Zero.ForeachRange";
    }

    static TParseTree ReturnStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ReturnStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ReturnStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ReturnStatement"), "ReturnStatement")(p);
                memo[tuple(`ReturnStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReturnStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ReturnStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ReturnStatement"), "ReturnStatement")(TParseTree("", false,[], s));
        }
    }
    static string ReturnStatement(GetName g)
    {
        return "Zero.ReturnStatement";
    }

    static TParseTree BreakStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.BreakStatement")(p);
        }
        else
        {
            if (auto m = tuple(`BreakStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.BreakStatement"), "BreakStatement")(p);
                memo[tuple(`BreakStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BreakStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.BreakStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.BreakStatement"), "BreakStatement")(TParseTree("", false,[], s));
        }
    }
    static string BreakStatement(GetName g)
    {
        return "Zero.BreakStatement";
    }

    static TParseTree ContinueStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ContinueStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ContinueStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ContinueStatement"), "ContinueStatement")(p);
                memo[tuple(`ContinueStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ContinueStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ContinueStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ContinueStatement"), "ContinueStatement")(TParseTree("", false,[], s));
        }
    }
    static string ContinueStatement(GetName g)
    {
        return "Zero.ContinueStatement";
    }

    static TParseTree ExpressionStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ExpressionStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ExpressionStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ExpressionStatement"), "ExpressionStatement")(p);
                memo[tuple(`ExpressionStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExpressionStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ExpressionStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ExpressionStatement"), "ExpressionStatement")(TParseTree("", false,[], s));
        }
    }
    static string ExpressionStatement(GetName g)
    {
        return "Zero.ExpressionStatement";
    }

    static TParseTree VarStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarDeclarationList, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.VarStatement")(p);
        }
        else
        {
            if (auto m = tuple(`VarStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarDeclarationList, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.VarStatement"), "VarStatement")(p);
                memo[tuple(`VarStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarDeclarationList, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.VarStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarDeclarationList, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.VarStatement"), "VarStatement")(TParseTree("", false,[], s));
        }
    }
    static string VarStatement(GetName g)
    {
        return "Zero.VarStatement";
    }

    static TParseTree VarDeclarationList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VarExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, VarExpression, Spacing)), Spacing))), "Zero.VarDeclarationList")(p);
        }
        else
        {
            if (auto m = tuple(`VarDeclarationList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VarExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, VarExpression, Spacing)), Spacing))), "Zero.VarDeclarationList"), "VarDeclarationList")(p);
                memo[tuple(`VarDeclarationList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarDeclarationList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VarExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, VarExpression, Spacing)), Spacing))), "Zero.VarDeclarationList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VarExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, VarExpression, Spacing)), Spacing))), "Zero.VarDeclarationList"), "VarDeclarationList")(TParseTree("", false,[], s));
        }
    }
    static string VarDeclarationList(GetName g)
    {
        return "Zero.VarDeclarationList";
    }

    static TParseTree VarExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VarDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.VarExpression")(p);
        }
        else
        {
            if (auto m = tuple(`VarExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VarDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.VarExpression"), "VarExpression")(p);
                memo[tuple(`VarExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VarDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.VarExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VarDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.VarExpression"), "VarExpression")(TParseTree("", false,[], s));
        }
    }
    static string VarExpression(GetName g)
    {
        return "Zero.VarExpression";
    }

    static TParseTree VarDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(identifier, "Zero.VarDeclaration")(p);
        }
        else
        {
            if (auto m = tuple(`VarDeclaration`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(identifier, "Zero.VarDeclaration"), "VarDeclaration")(p);
                memo[tuple(`VarDeclaration`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarDeclaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(identifier, "Zero.VarDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(identifier, "Zero.VarDeclaration"), "VarDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string VarDeclaration(GetName g)
    {
        return "Zero.VarDeclaration";
    }

    static TParseTree ConstStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.wrapAround!(Spacing, ConstDeclarationList, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ConstStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ConstStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.wrapAround!(Spacing, ConstDeclarationList, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ConstStatement"), "ConstStatement")(p);
                memo[tuple(`ConstStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConstStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.wrapAround!(Spacing, ConstDeclarationList, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ConstStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.wrapAround!(Spacing, ConstDeclarationList, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "Zero.ConstStatement"), "ConstStatement")(TParseTree("", false,[], s));
        }
    }
    static string ConstStatement(GetName g)
    {
        return "Zero.ConstStatement";
    }

    static TParseTree ConstDeclarationList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConstDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, ConstDeclaration, Spacing)), Spacing))), "Zero.ConstDeclarationList")(p);
        }
        else
        {
            if (auto m = tuple(`ConstDeclarationList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConstDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, ConstDeclaration, Spacing)), Spacing))), "Zero.ConstDeclarationList"), "ConstDeclarationList")(p);
                memo[tuple(`ConstDeclarationList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConstDeclarationList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConstDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, ConstDeclaration, Spacing)), Spacing))), "Zero.ConstDeclarationList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConstDeclaration, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, ConstDeclaration, Spacing)), Spacing))), "Zero.ConstDeclarationList"), "ConstDeclarationList")(TParseTree("", false,[], s));
        }
    }
    static string ConstDeclarationList(GetName g)
    {
        return "Zero.ConstDeclarationList";
    }

    static TParseTree ConstDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "Zero.ConstDeclaration")(p);
        }
        else
        {
            if (auto m = tuple(`ConstDeclaration`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "Zero.ConstDeclaration"), "ConstDeclaration")(p);
                memo[tuple(`ConstDeclaration`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConstDeclaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "Zero.ConstDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "Zero.ConstDeclaration"), "ConstDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string ConstDeclaration(GetName g)
    {
        return "Zero.ConstDeclaration";
    }

    static TParseTree FunctionStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.FunctionStatement")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.FunctionStatement"), "FunctionStatement")(p);
                memo[tuple(`FunctionStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.FunctionStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing), pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "Zero.FunctionStatement"), "FunctionStatement")(TParseTree("", false,[], s));
        }
    }
    static string FunctionStatement(GetName g)
    {
        return "Zero.FunctionStatement";
    }

    static TParseTree Parameters(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Zero.Parameters")(p);
        }
        else
        {
            if (auto m = tuple(`Parameters`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Zero.Parameters"), "Parameters")(p);
                memo[tuple(`Parameters`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Parameters(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Zero.Parameters")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Zero.Parameters"), "Parameters")(TParseTree("", false,[], s));
        }
    }
    static string Parameters(GetName g)
    {
        return "Zero.Parameters";
    }

    static TParseTree ParameterList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing))), "Zero.ParameterList")(p);
        }
        else
        {
            if (auto m = tuple(`ParameterList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing))), "Zero.ParameterList"), "ParameterList")(p);
                memo[tuple(`ParameterList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParameterList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing))), "Zero.ParameterList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing))), "Zero.ParameterList"), "ParameterList")(TParseTree("", false,[], s));
        }
    }
    static string ParameterList(GetName g)
    {
        return "Zero.ParameterList";
    }

    static TParseTree Parameter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, VarExpression, Spacing)), "Zero.Parameter")(p);
        }
        else
        {
            if (auto m = tuple(`Parameter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, VarExpression, Spacing)), "Zero.Parameter"), "Parameter")(p);
                memo[tuple(`Parameter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Parameter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, VarExpression, Spacing)), "Zero.Parameter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, VarExpression, Spacing)), "Zero.Parameter"), "Parameter")(TParseTree("", false,[], s));
        }
    }
    static string Parameter(GetName g)
    {
        return "Zero.Parameter";
    }

    static TParseTree FunctionAttributes(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "Zero.FunctionAttributes")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionAttributes`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "Zero.FunctionAttributes"), "FunctionAttributes")(p);
                memo[tuple(`FunctionAttributes`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionAttributes(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "Zero.FunctionAttributes")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "Zero.FunctionAttributes"), "FunctionAttributes")(TParseTree("", false,[], s));
        }
    }
    static string FunctionAttributes(GetName g)
    {
        return "Zero.FunctionAttributes";
    }

    static TParseTree FunctionAttribute(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("override"), "Zero.FunctionAttribute")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionAttribute`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("override"), "Zero.FunctionAttribute"), "FunctionAttribute")(p);
                memo[tuple(`FunctionAttribute`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionAttribute(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("override"), "Zero.FunctionAttribute")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("override"), "Zero.FunctionAttribute"), "FunctionAttribute")(TParseTree("", false,[], s));
        }
    }
    static string FunctionAttribute(GetName g)
    {
        return "Zero.FunctionAttribute";
    }

    static TParseTree Expression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing), "Zero.Expression")(p);
        }
        else
        {
            if (auto m = tuple(`Expression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing), "Zero.Expression"), "Expression")(p);
                memo[tuple(`Expression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Expression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing), "Zero.Expression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, AssignExpr, Spacing), "Zero.Expression"), "Expression")(TParseTree("", false,[], s));
        }
    }
    static string Expression(GetName g)
    {
        return "Zero.Expression";
    }

    static TParseTree AssignExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing)), Spacing))), "Zero.AssignExpr")(p);
        }
        else
        {
            if (auto m = tuple(`AssignExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing)), Spacing))), "Zero.AssignExpr"), "AssignExpr")(p);
                memo[tuple(`AssignExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AssignExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing)), Spacing))), "Zero.AssignExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing)), Spacing))), "Zero.AssignExpr"), "AssignExpr")(TParseTree("", false,[], s));
        }
    }
    static string AssignExpr(GetName g)
    {
        return "Zero.AssignExpr";
    }

    static TParseTree TernaryExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing)), Spacing))), "Zero.TernaryExpr")(p);
        }
        else
        {
            if (auto m = tuple(`TernaryExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing)), Spacing))), "Zero.TernaryExpr"), "TernaryExpr")(p);
                memo[tuple(`TernaryExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TernaryExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing)), Spacing))), "Zero.TernaryExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing)), Spacing))), "Zero.TernaryExpr"), "TernaryExpr")(TParseTree("", false,[], s));
        }
    }
    static string TernaryExpr(GetName g)
    {
        return "Zero.TernaryExpr";
    }

    static TParseTree OrExpr(TParseTree p)
    {
        if(__ctfe)
        {
            assert(false, "OrExpr is left-recursive, which is not supported at compile-time. Consider using asModule().");
        }
        else
        {
            static TParseTree[size_t /*position*/] seed;
            if (auto s = p.end in seed)
                return *s;
            if (!blockMemoAtPos.canFind(p.end))
                if (auto m = tuple(`OrExpr`, p.end) in memo)
                    return *m;
            auto current = fail(p);
            seed[p.end] = current;
            blockMemoAtPos ~= p.end;
            while (true)
            {
                auto result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, AndExpr, Spacing)), "Zero.OrExpr"), "OrExpr")(p);
                if (result.end > current.end ||
                    (!current.successful && result.successful) /* null-match */)
                {
                    current = result;
                    seed[p.end] = current;
                } else {
                    seed.remove(p.end);
                    assert(blockMemoAtPos.canFind(p.end));
                    blockMemoAtPos = blockMemoAtPos.remove(countUntil(blockMemoAtPos, p.end));
                    memo[tuple(`OrExpr`, p.end)] = current;
                    return current;
                }
            }
        }
    }

    static TParseTree OrExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, AndExpr, Spacing)), "Zero.OrExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, AndExpr, Spacing)), "Zero.OrExpr"), "OrExpr")(TParseTree("", false,[], s));
        }
    }
    static string OrExpr(GetName g)
    {
        return "Zero.OrExpr";
    }

    static TParseTree AndExpr(TParseTree p)
    {
        if(__ctfe)
        {
            assert(false, "AndExpr is left-recursive, which is not supported at compile-time. Consider using asModule().");
        }
        else
        {
            static TParseTree[size_t /*position*/] seed;
            if (auto s = p.end in seed)
                return *s;
            if (!blockMemoAtPos.canFind(p.end))
                if (auto m = tuple(`AndExpr`, p.end) in memo)
                    return *m;
            auto current = fail(p);
            seed[p.end] = current;
            blockMemoAtPos ~= p.end;
            while (true)
            {
                auto result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("and"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, CompareExpr, Spacing)), "Zero.AndExpr"), "AndExpr")(p);
                if (result.end > current.end ||
                    (!current.successful && result.successful) /* null-match */)
                {
                    current = result;
                    seed[p.end] = current;
                } else {
                    seed.remove(p.end);
                    assert(blockMemoAtPos.canFind(p.end));
                    blockMemoAtPos = blockMemoAtPos.remove(countUntil(blockMemoAtPos, p.end));
                    memo[tuple(`AndExpr`, p.end)] = current;
                    return current;
                }
            }
        }
    }

    static TParseTree AndExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("and"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, CompareExpr, Spacing)), "Zero.AndExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("and"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, CompareExpr, Spacing)), "Zero.AndExpr"), "AndExpr")(TParseTree("", false,[], s));
        }
    }
    static string AndExpr(GetName g)
    {
        return "Zero.AndExpr";
    }

    static TParseTree CompareExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CompareOp, Spacing), pegged.peg.wrapAround!(Spacing, AddExpr, Spacing)), Spacing))), "Zero.CompareExpr")(p);
        }
        else
        {
            if (auto m = tuple(`CompareExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CompareOp, Spacing), pegged.peg.wrapAround!(Spacing, AddExpr, Spacing)), Spacing))), "Zero.CompareExpr"), "CompareExpr")(p);
                memo[tuple(`CompareExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CompareExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CompareOp, Spacing), pegged.peg.wrapAround!(Spacing, AddExpr, Spacing)), Spacing))), "Zero.CompareExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CompareOp, Spacing), pegged.peg.wrapAround!(Spacing, AddExpr, Spacing)), Spacing))), "Zero.CompareExpr"), "CompareExpr")(TParseTree("", false,[], s));
        }
    }
    static string CompareExpr(GetName g)
    {
        return "Zero.CompareExpr";
    }

    static TParseTree CompareOp(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("<=", "<", "=", "!=", ">=", ">", "in", "!in"), "Zero.CompareOp")(p);
        }
        else
        {
            if (auto m = tuple(`CompareOp`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("<=", "<", "=", "!=", ">=", ">", "in", "!in"), "Zero.CompareOp"), "CompareOp")(p);
                memo[tuple(`CompareOp`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CompareOp(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("<=", "<", "=", "!=", ">=", ">", "in", "!in"), "Zero.CompareOp")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("<=", "<", "=", "!=", ">=", ">", "in", "!in"), "Zero.CompareOp"), "CompareOp")(TParseTree("", false,[], s));
        }
    }
    static string CompareOp(GetName g)
    {
        return "Zero.CompareOp";
    }

    static TParseTree AddExpr(TParseTree p)
    {
        if(__ctfe)
        {
            assert(false, "AddExpr is left-recursive, which is not supported at compile-time. Consider using asModule().");
        }
        else
        {
            static TParseTree[size_t /*position*/] seed;
            if (auto s = p.end in seed)
                return *s;
            if (!blockMemoAtPos.canFind(p.end))
                if (auto m = tuple(`AddExpr`, p.end) in memo)
                    return *m;
            auto current = fail(p);
            seed[p.end] = current;
            blockMemoAtPos ~= p.end;
            while (true)
            {
                auto result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.wrapAround!(Spacing, AddOp, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, MulExpr, Spacing)), "Zero.AddExpr"), "AddExpr")(p);
                if (result.end > current.end ||
                    (!current.successful && result.successful) /* null-match */)
                {
                    current = result;
                    seed[p.end] = current;
                } else {
                    seed.remove(p.end);
                    assert(blockMemoAtPos.canFind(p.end));
                    blockMemoAtPos = blockMemoAtPos.remove(countUntil(blockMemoAtPos, p.end));
                    memo[tuple(`AddExpr`, p.end)] = current;
                    return current;
                }
            }
        }
    }

    static TParseTree AddExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.wrapAround!(Spacing, AddOp, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, MulExpr, Spacing)), "Zero.AddExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.wrapAround!(Spacing, AddOp, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, MulExpr, Spacing)), "Zero.AddExpr"), "AddExpr")(TParseTree("", false,[], s));
        }
    }
    static string AddExpr(GetName g)
    {
        return "Zero.AddExpr";
    }

    static TParseTree AddOp(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing)), "Zero.AddOp")(p);
        }
        else
        {
            if (auto m = tuple(`AddOp`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing)), "Zero.AddOp"), "AddOp")(p);
                memo[tuple(`AddOp`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AddOp(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing)), "Zero.AddOp")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing)), "Zero.AddOp"), "AddOp")(TParseTree("", false,[], s));
        }
    }
    static string AddOp(GetName g)
    {
        return "Zero.AddOp";
    }

    static TParseTree MulExpr(TParseTree p)
    {
        if(__ctfe)
        {
            assert(false, "MulExpr is left-recursive, which is not supported at compile-time. Consider using asModule().");
        }
        else
        {
            static TParseTree[size_t /*position*/] seed;
            if (auto s = p.end in seed)
                return *s;
            if (!blockMemoAtPos.canFind(p.end))
                if (auto m = tuple(`MulExpr`, p.end) in memo)
                    return *m;
            auto current = fail(p);
            seed[p.end] = current;
            blockMemoAtPos ~= p.end;
            while (true)
            {
                auto result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), pegged.peg.wrapAround!(Spacing, MulOp, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.MulExpr"), "MulExpr")(p);
                if (result.end > current.end ||
                    (!current.successful && result.successful) /* null-match */)
                {
                    current = result;
                    seed[p.end] = current;
                } else {
                    seed.remove(p.end);
                    assert(blockMemoAtPos.canFind(p.end));
                    blockMemoAtPos = blockMemoAtPos.remove(countUntil(blockMemoAtPos, p.end));
                    memo[tuple(`MulExpr`, p.end)] = current;
                    return current;
                }
            }
        }
    }

    static TParseTree MulExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), pegged.peg.wrapAround!(Spacing, MulOp, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.MulExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), pegged.peg.wrapAround!(Spacing, MulOp, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.MulExpr"), "MulExpr")(TParseTree("", false,[], s));
        }
    }
    static string MulExpr(GetName g)
    {
        return "Zero.MulExpr";
    }

    static TParseTree MulOp(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), "Zero.MulOp")(p);
        }
        else
        {
            if (auto m = tuple(`MulOp`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), "Zero.MulOp"), "MulOp")(p);
                memo[tuple(`MulOp`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MulOp(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), "Zero.MulOp")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), "Zero.MulOp"), "MulOp")(TParseTree("", false,[], s));
        }
    }
    static string MulOp(GetName g)
    {
        return "Zero.MulOp";
    }

    static TParseTree UnaryExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NotExpr, Spacing), pegged.peg.wrapAround!(Spacing, PowExpr, Spacing), pegged.peg.wrapAround!(Spacing, SignExpr, Spacing)), "Zero.UnaryExpr")(p);
        }
        else
        {
            if (auto m = tuple(`UnaryExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NotExpr, Spacing), pegged.peg.wrapAround!(Spacing, PowExpr, Spacing), pegged.peg.wrapAround!(Spacing, SignExpr, Spacing)), "Zero.UnaryExpr"), "UnaryExpr")(p);
                memo[tuple(`UnaryExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnaryExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NotExpr, Spacing), pegged.peg.wrapAround!(Spacing, PowExpr, Spacing), pegged.peg.wrapAround!(Spacing, SignExpr, Spacing)), "Zero.UnaryExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NotExpr, Spacing), pegged.peg.wrapAround!(Spacing, PowExpr, Spacing), pegged.peg.wrapAround!(Spacing, SignExpr, Spacing)), "Zero.UnaryExpr"), "UnaryExpr")(TParseTree("", false,[], s));
        }
    }
    static string UnaryExpr(GetName g)
    {
        return "Zero.UnaryExpr";
    }

    static TParseTree NotExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("not"), Spacing)), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.NotExpr")(p);
        }
        else
        {
            if (auto m = tuple(`NotExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("not"), Spacing)), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.NotExpr"), "NotExpr")(p);
                memo[tuple(`NotExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NotExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("not"), Spacing)), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.NotExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("not"), Spacing)), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.NotExpr"), "NotExpr")(TParseTree("", false,[], s));
        }
    }
    static string NotExpr(GetName g)
    {
        return "Zero.NotExpr";
    }

    static TParseTree SignExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sign, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.SignExpr")(p);
        }
        else
        {
            if (auto m = tuple(`SignExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sign, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.SignExpr"), "SignExpr")(p);
                memo[tuple(`SignExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SignExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sign, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.SignExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sign, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), "Zero.SignExpr"), "SignExpr")(TParseTree("", false,[], s));
        }
    }
    static string SignExpr(GetName g)
    {
        return "Zero.SignExpr";
    }

    static TParseTree PowExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), Spacing))), "Zero.PowExpr")(p);
        }
        else
        {
            if (auto m = tuple(`PowExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), Spacing))), "Zero.PowExpr"), "PowExpr")(p);
                memo[tuple(`PowExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PowExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), Spacing))), "Zero.PowExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing)), Spacing))), "Zero.PowExpr"), "PowExpr")(TParseTree("", false,[], s));
        }
    }
    static string PowExpr(GetName g)
    {
        return "Zero.PowExpr";
    }

    static TParseTree PostExpr(TParseTree p)
    {
        if(__ctfe)
        {
            assert(false, "PostExpr is left-recursive, which is not supported at compile-time. Consider using asModule().");
        }
        else
        {
            static TParseTree[size_t /*position*/] seed;
            if (auto s = p.end in seed)
                return *s;
            if (!blockMemoAtPos.canFind(p.end))
                if (auto m = tuple(`PostExpr`, p.end) in memo)
                    return *m;
            auto current = fail(p);
            seed[p.end] = current;
            blockMemoAtPos ~= p.end;
            while (true)
            {
                auto result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TableIndex, Spacing), pegged.peg.wrapAround!(Spacing, ArrayIndex, Spacing), pegged.peg.wrapAround!(Spacing, CallExpr, Spacing), pegged.peg.wrapAround!(Spacing, MemberCall, Spacing), pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing)), "Zero.PostExpr"), "PostExpr")(p);
                if (result.end > current.end ||
                    (!current.successful && result.successful) /* null-match */)
                {
                    current = result;
                    seed[p.end] = current;
                } else {
                    seed.remove(p.end);
                    assert(blockMemoAtPos.canFind(p.end));
                    blockMemoAtPos = blockMemoAtPos.remove(countUntil(blockMemoAtPos, p.end));
                    memo[tuple(`PostExpr`, p.end)] = current;
                    return current;
                }
            }
        }
    }

    static TParseTree PostExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TableIndex, Spacing), pegged.peg.wrapAround!(Spacing, ArrayIndex, Spacing), pegged.peg.wrapAround!(Spacing, CallExpr, Spacing), pegged.peg.wrapAround!(Spacing, MemberCall, Spacing), pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing)), "Zero.PostExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TableIndex, Spacing), pegged.peg.wrapAround!(Spacing, ArrayIndex, Spacing), pegged.peg.wrapAround!(Spacing, CallExpr, Spacing), pegged.peg.wrapAround!(Spacing, MemberCall, Spacing), pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing)), "Zero.PostExpr"), "PostExpr")(TParseTree("", false,[], s));
        }
    }
    static string PostExpr(GetName g)
    {
        return "Zero.PostExpr";
    }

    static TParseTree MemberCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(PostExpr, pegged.peg.discard!(pegged.peg.literal!(".")), Identifier), "Zero.MemberCall")(p);
        }
        else
        {
            if (blockMemoAtPos.canFind(p.end))
                return hooked!(pegged.peg.defined!(pegged.peg.and!(PostExpr, pegged.peg.discard!(pegged.peg.literal!(".")), Identifier), "Zero.MemberCall"), "MemberCall")(p);
            if (auto m = tuple(`MemberCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(PostExpr, pegged.peg.discard!(pegged.peg.literal!(".")), Identifier), "Zero.MemberCall"), "MemberCall")(p);
                memo[tuple(`MemberCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MemberCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(PostExpr, pegged.peg.discard!(pegged.peg.literal!(".")), Identifier), "Zero.MemberCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(PostExpr, pegged.peg.discard!(pegged.peg.literal!(".")), Identifier), "Zero.MemberCall"), "MemberCall")(TParseTree("", false,[], s));
        }
    }
    static string MemberCall(GetName g)
    {
        return "Zero.MemberCall";
    }

    static TParseTree TableIndex(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "Zero.TableIndex")(p);
        }
        else
        {
            if (blockMemoAtPos.canFind(p.end))
                return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "Zero.TableIndex"), "TableIndex")(p);
            if (auto m = tuple(`TableIndex`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "Zero.TableIndex"), "TableIndex")(p);
                memo[tuple(`TableIndex`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TableIndex(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "Zero.TableIndex")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "Zero.TableIndex"), "TableIndex")(TParseTree("", false,[], s));
        }
    }
    static string TableIndex(GetName g)
    {
        return "Zero.TableIndex";
    }

    static TParseTree ArrayIndex(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "Zero.ArrayIndex")(p);
        }
        else
        {
            if (blockMemoAtPos.canFind(p.end))
                return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "Zero.ArrayIndex"), "ArrayIndex")(p);
            if (auto m = tuple(`ArrayIndex`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "Zero.ArrayIndex"), "ArrayIndex")(p);
                memo[tuple(`ArrayIndex`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArrayIndex(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "Zero.ArrayIndex")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "Zero.ArrayIndex"), "ArrayIndex")(TParseTree("", false,[], s));
        }
    }
    static string ArrayIndex(GetName g)
    {
        return "Zero.ArrayIndex";
    }

    static TParseTree CallExpr(TParseTree p)
    {
        if(__ctfe)
        {
            assert(false, "CallExpr is left-recursive, which is not supported at compile-time. Consider using asModule().");
        }
        else
        {
            static TParseTree[size_t /*position*/] seed;
            if (auto s = p.end in seed)
                return *s;
            if (!blockMemoAtPos.canFind(p.end))
                if (auto m = tuple(`CallExpr`, p.end) in memo)
                    return *m;
            auto current = fail(p);
            seed[p.end] = current;
            blockMemoAtPos ~= p.end;
            while (true)
            {
                auto result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.wrapAround!(Spacing, Argument, Spacing)), "Zero.CallExpr"), "CallExpr")(p);
                if (result.end > current.end)
                {
                    current = result;
                    seed[p.end] = current;
                } else {
                    seed.remove(p.end);
                    assert(blockMemoAtPos.canFind(p.end));
                    blockMemoAtPos = blockMemoAtPos.remove(countUntil(blockMemoAtPos, p.end));
                    memo[tuple(`CallExpr`, p.end)] = current;
                    return current;
                }
            }
        }
    }

    static TParseTree CallExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.wrapAround!(Spacing, Argument, Spacing)), "Zero.CallExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostExpr, Spacing), pegged.peg.wrapAround!(Spacing, Argument, Spacing)), "Zero.CallExpr"), "CallExpr")(TParseTree("", false,[], s));
        }
    }
    static string CallExpr(GetName g)
    {
        return "Zero.CallExpr";
    }

    static TParseTree Argument(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Zero.Argument")(p);
        }
        else
        {
            if (auto m = tuple(`Argument`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Zero.Argument"), "Argument")(p);
                memo[tuple(`Argument`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Argument(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Zero.Argument")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Zero.Argument"), "Argument")(TParseTree("", false,[], s));
        }
    }
    static string Argument(GetName g)
    {
        return "Zero.Argument";
    }

    static TParseTree ArgumentList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ArgumentList")(p);
        }
        else
        {
            if (auto m = tuple(`ArgumentList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ArgumentList"), "ArgumentList")(p);
                memo[tuple(`ArgumentList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArgumentList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ArgumentList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ArgumentList"), "ArgumentList")(TParseTree("", false,[], s));
        }
    }
    static string ArgumentList(GetName g)
    {
        return "Zero.ArgumentList";
    }

    static TParseTree PrimaryExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), pegged.peg.wrapAround!(Spacing, ArrayExpr, Spacing), pegged.peg.wrapAround!(Spacing, TableExpr, Spacing), pegged.peg.wrapAround!(Spacing, ConstExpr, Spacing), pegged.peg.wrapAround!(Spacing, RealLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, This, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing), pegged.peg.wrapAround!(Spacing, True, Spacing), pegged.peg.wrapAround!(Spacing, False, Spacing), pegged.peg.wrapAround!(Spacing, Lambda, Spacing)), "Zero.PrimaryExpr")(p);
        }
        else
        {
            if (auto m = tuple(`PrimaryExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), pegged.peg.wrapAround!(Spacing, ArrayExpr, Spacing), pegged.peg.wrapAround!(Spacing, TableExpr, Spacing), pegged.peg.wrapAround!(Spacing, ConstExpr, Spacing), pegged.peg.wrapAround!(Spacing, RealLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, This, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing), pegged.peg.wrapAround!(Spacing, True, Spacing), pegged.peg.wrapAround!(Spacing, False, Spacing), pegged.peg.wrapAround!(Spacing, Lambda, Spacing)), "Zero.PrimaryExpr"), "PrimaryExpr")(p);
                memo[tuple(`PrimaryExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PrimaryExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), pegged.peg.wrapAround!(Spacing, ArrayExpr, Spacing), pegged.peg.wrapAround!(Spacing, TableExpr, Spacing), pegged.peg.wrapAround!(Spacing, ConstExpr, Spacing), pegged.peg.wrapAround!(Spacing, RealLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, This, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing), pegged.peg.wrapAround!(Spacing, True, Spacing), pegged.peg.wrapAround!(Spacing, False, Spacing), pegged.peg.wrapAround!(Spacing, Lambda, Spacing)), "Zero.PrimaryExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), pegged.peg.wrapAround!(Spacing, ArrayExpr, Spacing), pegged.peg.wrapAround!(Spacing, TableExpr, Spacing), pegged.peg.wrapAround!(Spacing, ConstExpr, Spacing), pegged.peg.wrapAround!(Spacing, RealLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, String, Spacing), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, This, Spacing), pegged.peg.wrapAround!(Spacing, Null, Spacing), pegged.peg.wrapAround!(Spacing, True, Spacing), pegged.peg.wrapAround!(Spacing, False, Spacing), pegged.peg.wrapAround!(Spacing, Lambda, Spacing)), "Zero.PrimaryExpr"), "PrimaryExpr")(TParseTree("", false,[], s));
        }
    }
    static string PrimaryExpr(GetName g)
    {
        return "Zero.PrimaryExpr";
    }

    static TParseTree ArrayExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayElement, Spacing)), pegged.peg.discard!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing))), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "Zero.ArrayExpr")(p);
        }
        else
        {
            if (auto m = tuple(`ArrayExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayElement, Spacing)), pegged.peg.discard!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing))), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "Zero.ArrayExpr"), "ArrayExpr")(p);
                memo[tuple(`ArrayExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArrayExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayElement, Spacing)), pegged.peg.discard!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing))), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "Zero.ArrayExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayElement, Spacing)), pegged.peg.discard!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing))), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "Zero.ArrayExpr"), "ArrayExpr")(TParseTree("", false,[], s));
        }
    }
    static string ArrayExpr(GetName g)
    {
        return "Zero.ArrayExpr";
    }

    static TParseTree ArrayElement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ArrayElement")(p);
        }
        else
        {
            if (auto m = tuple(`ArrayElement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ArrayElement"), "ArrayElement")(p);
                memo[tuple(`ArrayElement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArrayElement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ArrayElement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.ArrayElement"), "ArrayElement")(TParseTree("", false,[], s));
        }
    }
    static string ArrayElement(GetName g)
    {
        return "Zero.ArrayElement";
    }

    static TParseTree TableExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TableElementList, Spacing)), pegged.peg.discard!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing))), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Zero.TableExpr")(p);
        }
        else
        {
            if (auto m = tuple(`TableExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TableElementList, Spacing)), pegged.peg.discard!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing))), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Zero.TableExpr"), "TableExpr")(p);
                memo[tuple(`TableExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TableExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TableElementList, Spacing)), pegged.peg.discard!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing))), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Zero.TableExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TableElementList, Spacing)), pegged.peg.discard!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing))), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "Zero.TableExpr"), "TableExpr")(TParseTree("", false,[], s));
        }
    }
    static string TableExpr(GetName g)
    {
        return "Zero.TableExpr";
    }

    static TParseTree TableElementList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TableElement, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, TableElement, Spacing)), Spacing))), "Zero.TableElementList")(p);
        }
        else
        {
            if (auto m = tuple(`TableElementList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TableElement, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, TableElement, Spacing)), Spacing))), "Zero.TableElementList"), "TableElementList")(p);
                memo[tuple(`TableElementList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TableElementList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TableElement, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, TableElement, Spacing)), Spacing))), "Zero.TableElementList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TableElement, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, TableElement, Spacing)), Spacing))), "Zero.TableElementList"), "TableElementList")(TParseTree("", false,[], s));
        }
    }
    static string TableElementList(GetName g)
    {
        return "Zero.TableElementList";
    }

    static TParseTree TableElement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.TableElement")(p);
        }
        else
        {
            if (auto m = tuple(`TableElement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.TableElement"), "TableElement")(p);
                memo[tuple(`TableElement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TableElement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.TableElement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TernaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing))), "Zero.TableElement"), "TableElement")(TParseTree("", false,[], s));
        }
    }
    static string TableElement(GetName g)
    {
        return "Zero.TableElement";
    }

    static TParseTree ConstExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "Zero.ConstExpr")(p);
        }
        else
        {
            if (auto m = tuple(`ConstExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "Zero.ConstExpr"), "ConstExpr")(p);
                memo[tuple(`ConstExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConstExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "Zero.ConstExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "Zero.ConstExpr"), "ConstExpr")(TParseTree("", false,[], s));
        }
    }
    static string ConstExpr(GetName g)
    {
        return "Zero.ConstExpr";
    }

    static TParseTree VarIdentifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(Identifier, "Zero.VarIdentifier")(p);
        }
        else
        {
            if (auto m = tuple(`VarIdentifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(Identifier, "Zero.VarIdentifier"), "VarIdentifier")(p);
                memo[tuple(`VarIdentifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarIdentifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(Identifier, "Zero.VarIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(Identifier, "Zero.VarIdentifier"), "VarIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string VarIdentifier(GetName g)
    {
        return "Zero.VarIdentifier";
    }

    static TParseTree IntegerLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(Binary, Hexadecimal, Octal, Decimal), "Zero.IntegerLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`IntegerLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(Binary, Hexadecimal, Octal, Decimal), "Zero.IntegerLiteral"), "IntegerLiteral")(p);
                memo[tuple(`IntegerLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntegerLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(Binary, Hexadecimal, Octal, Decimal), "Zero.IntegerLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(Binary, Hexadecimal, Octal, Decimal), "Zero.IntegerLiteral"), "IntegerLiteral")(TParseTree("", false,[], s));
        }
    }
    static string IntegerLiteral(GetName g)
    {
        return "Zero.IntegerLiteral";
    }

    static TParseTree Decimal(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("d"), pegged.peg.literal!("D"))))), Integer), pegged.peg.and!(pegged.peg.option!(Sign), Integer))), "Zero.Decimal")(p);
        }
        else
        {
            if (auto m = tuple(`Decimal`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("d"), pegged.peg.literal!("D"))))), Integer), pegged.peg.and!(pegged.peg.option!(Sign), Integer))), "Zero.Decimal"), "Decimal")(p);
                memo[tuple(`Decimal`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Decimal(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("d"), pegged.peg.literal!("D"))))), Integer), pegged.peg.and!(pegged.peg.option!(Sign), Integer))), "Zero.Decimal")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("d"), pegged.peg.literal!("D"))))), Integer), pegged.peg.and!(pegged.peg.option!(Sign), Integer))), "Zero.Decimal"), "Decimal")(TParseTree("", false,[], s));
        }
    }
    static string Decimal(GetName g)
    {
        return "Zero.Decimal";
    }

    static TParseTree Binary(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("B")))), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.discard!(pegged.peg.literal!("\'")))))), "Zero.Binary")(p);
        }
        else
        {
            if (auto m = tuple(`Binary`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("B")))), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.discard!(pegged.peg.literal!("\'")))))), "Zero.Binary"), "Binary")(p);
                memo[tuple(`Binary`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Binary(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("B")))), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.discard!(pegged.peg.literal!("\'")))))), "Zero.Binary")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("b"), pegged.peg.literal!("B")))), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.discard!(pegged.peg.literal!("\'")))))), "Zero.Binary"), "Binary")(TParseTree("", false,[], s));
        }
    }
    static string Binary(GetName g)
    {
        return "Zero.Binary";
    }

    static TParseTree Hexadecimal(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("x"), pegged.peg.literal!("X")))), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(HexDigit, pegged.peg.literal!("\'"))))), "Zero.Hexadecimal")(p);
        }
        else
        {
            if (auto m = tuple(`Hexadecimal`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("x"), pegged.peg.literal!("X")))), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(HexDigit, pegged.peg.literal!("\'"))))), "Zero.Hexadecimal"), "Hexadecimal")(p);
                memo[tuple(`Hexadecimal`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Hexadecimal(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("x"), pegged.peg.literal!("X")))), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(HexDigit, pegged.peg.literal!("\'"))))), "Zero.Hexadecimal")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("x"), pegged.peg.literal!("X")))), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(HexDigit, pegged.peg.literal!("\'"))))), "Zero.Hexadecimal"), "Hexadecimal")(TParseTree("", false,[], s));
        }
    }
    static string Hexadecimal(GetName g)
    {
        return "Zero.Hexadecimal";
    }

    static TParseTree HexDigit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing), "Zero.HexDigit")(p);
        }
        else
        {
            if (auto m = tuple(`HexDigit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing), "Zero.HexDigit"), "HexDigit")(p);
                memo[tuple(`HexDigit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HexDigit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing), "Zero.HexDigit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing), "Zero.HexDigit"), "HexDigit")(TParseTree("", false,[], s));
        }
    }
    static string HexDigit(GetName g)
    {
        return "Zero.HexDigit";
    }

    static TParseTree Octal(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("c"), pegged.peg.literal!("C")))), OctalDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(OctalDigit, pegged.peg.literal!("\'"))))), "Zero.Octal")(p);
        }
        else
        {
            if (auto m = tuple(`Octal`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("c"), pegged.peg.literal!("C")))), OctalDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(OctalDigit, pegged.peg.literal!("\'"))))), "Zero.Octal"), "Octal")(p);
                memo[tuple(`Octal`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Octal(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("c"), pegged.peg.literal!("C")))), OctalDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(OctalDigit, pegged.peg.literal!("\'"))))), "Zero.Octal")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("0"), pegged.peg.or!(pegged.peg.literal!("c"), pegged.peg.literal!("C")))), OctalDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(OctalDigit, pegged.peg.literal!("\'"))))), "Zero.Octal"), "Octal")(TParseTree("", false,[], s));
        }
    }
    static string Octal(GetName g)
    {
        return "Zero.Octal";
    }

    static TParseTree OctalDigit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.charRange!('0', '7'), Spacing), "Zero.OctalDigit")(p);
        }
        else
        {
            if (auto m = tuple(`OctalDigit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.charRange!('0', '7'), Spacing), "Zero.OctalDigit"), "OctalDigit")(p);
                memo[tuple(`OctalDigit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OctalDigit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.charRange!('0', '7'), Spacing), "Zero.OctalDigit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.charRange!('0', '7'), Spacing), "Zero.OctalDigit"), "OctalDigit")(TParseTree("", false,[], s));
        }
    }
    static string OctalDigit(GetName g)
    {
        return "Zero.OctalDigit";
    }

    static TParseTree Sign(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("+", "-"), "Zero.Sign")(p);
        }
        else
        {
            if (auto m = tuple(`Sign`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("+", "-"), "Zero.Sign"), "Sign")(p);
                memo[tuple(`Sign`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Sign(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("+", "-"), "Zero.Sign")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("+", "-"), "Zero.Sign"), "Sign")(TParseTree("", false,[], s));
        }
    }
    static string Sign(GetName g)
    {
        return "Zero.Sign";
    }

    static TParseTree Integer(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.discard!(pegged.peg.literal!("\'")))))), "Zero.Integer")(p);
        }
        else
        {
            if (auto m = tuple(`Integer`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.discard!(pegged.peg.literal!("\'")))))), "Zero.Integer"), "Integer")(p);
                memo[tuple(`Integer`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Integer(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.discard!(pegged.peg.literal!("\'")))))), "Zero.Integer")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.discard!(pegged.peg.literal!("\'")))))), "Zero.Integer"), "Integer")(TParseTree("", false,[], s));
        }
    }
    static string Integer(GetName g)
    {
        return "Zero.Integer";
    }

    static TParseTree RealLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), Integer, pegged.peg.option!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("e"), pegged.peg.literal!("E")), pegged.peg.option!(Sign), Integer))), pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.or!(pegged.peg.literal!("e"), pegged.peg.literal!("E")), pegged.peg.option!(Sign), Integer))), "Zero.RealLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`RealLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), Integer, pegged.peg.option!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("e"), pegged.peg.literal!("E")), pegged.peg.option!(Sign), Integer))), pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.or!(pegged.peg.literal!("e"), pegged.peg.literal!("E")), pegged.peg.option!(Sign), Integer))), "Zero.RealLiteral"), "RealLiteral")(p);
                memo[tuple(`RealLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RealLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), Integer, pegged.peg.option!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("e"), pegged.peg.literal!("E")), pegged.peg.option!(Sign), Integer))), pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.or!(pegged.peg.literal!("e"), pegged.peg.literal!("E")), pegged.peg.option!(Sign), Integer))), "Zero.RealLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), Integer, pegged.peg.option!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("e"), pegged.peg.literal!("E")), pegged.peg.option!(Sign), Integer))), pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.or!(pegged.peg.literal!("e"), pegged.peg.literal!("E")), pegged.peg.option!(Sign), Integer))), "Zero.RealLiteral"), "RealLiteral")(TParseTree("", false,[], s));
        }
    }
    static string RealLiteral(GetName g)
    {
        return "Zero.RealLiteral";
    }

    static TParseTree String(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RawString, Spacing), pegged.peg.wrapAround!(Spacing, QuotedString, Spacing)), "Zero.String")(p);
        }
        else
        {
            if (auto m = tuple(`String`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RawString, Spacing), pegged.peg.wrapAround!(Spacing, QuotedString, Spacing)), "Zero.String"), "String")(p);
                memo[tuple(`String`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree String(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RawString, Spacing), pegged.peg.wrapAround!(Spacing, QuotedString, Spacing)), "Zero.String")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RawString, Spacing), pegged.peg.wrapAround!(Spacing, QuotedString, Spacing)), "Zero.String"), "String")(TParseTree("", false,[], s));
        }
    }
    static string String(GetName g)
    {
        return "Zero.String";
    }

    static TParseTree RawString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Zero.RawString")(p);
        }
        else
        {
            if (auto m = tuple(`RawString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Zero.RawString"), "RawString")(p);
                memo[tuple(`RawString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RawString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Zero.RawString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Zero.RawString"), "RawString")(TParseTree("", false,[], s));
        }
    }
    static string RawString(GetName g)
    {
        return "Zero.RawString";
    }

    static TParseTree QuotedString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote)), "Zero.QuotedString")(p);
        }
        else
        {
            if (auto m = tuple(`QuotedString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote)), "Zero.QuotedString"), "QuotedString")(p);
                memo[tuple(`QuotedString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree QuotedString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote)), "Zero.QuotedString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote)), "Zero.QuotedString"), "QuotedString")(TParseTree("", false,[], s));
        }
    }
    static string QuotedString(GetName g)
    {
        return "Zero.QuotedString";
    }

    static TParseTree DQChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "Zero.DQChar")(p);
        }
        else
        {
            if (auto m = tuple(`DQChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "Zero.DQChar"), "DQChar")(p);
                memo[tuple(`DQChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DQChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "Zero.DQChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "Zero.DQChar"), "DQChar")(TParseTree("", false,[], s));
        }
    }
    static string DQChar(GetName g)
    {
        return "Zero.DQChar";
    }

    static TParseTree EscapeSequence(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backslash, pegged.peg.or!(doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")))), "Zero.EscapeSequence")(p);
        }
        else
        {
            if (auto m = tuple(`EscapeSequence`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(backslash, pegged.peg.or!(doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")))), "Zero.EscapeSequence"), "EscapeSequence")(p);
                memo[tuple(`EscapeSequence`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EscapeSequence(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(backslash, pegged.peg.or!(doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")))), "Zero.EscapeSequence")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(backslash, pegged.peg.or!(doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")))), "Zero.EscapeSequence"), "EscapeSequence")(TParseTree("", false,[], s));
        }
    }
    static string EscapeSequence(GetName g)
    {
        return "Zero.EscapeSequence";
    }

    static TParseTree This(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), "Zero.This")(p);
        }
        else
        {
            if (auto m = tuple(`This`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), "Zero.This"), "This")(p);
                memo[tuple(`This`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree This(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), "Zero.This")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), "Zero.This"), "This")(TParseTree("", false,[], s));
        }
    }
    static string This(GetName g)
    {
        return "Zero.This";
    }

    static TParseTree Null(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), "Zero.Null")(p);
        }
        else
        {
            if (auto m = tuple(`Null`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), "Zero.Null"), "Null")(p);
                memo[tuple(`Null`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Null(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), "Zero.Null")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), "Zero.Null"), "Null")(TParseTree("", false,[], s));
        }
    }
    static string Null(GetName g)
    {
        return "Zero.Null";
    }

    static TParseTree True(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), "Zero.True")(p);
        }
        else
        {
            if (auto m = tuple(`True`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), "Zero.True"), "True")(p);
                memo[tuple(`True`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree True(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), "Zero.True")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), "Zero.True"), "True")(TParseTree("", false,[], s));
        }
    }
    static string True(GetName g)
    {
        return "Zero.True";
    }

    static TParseTree False(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), "Zero.False")(p);
        }
        else
        {
            if (auto m = tuple(`False`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), "Zero.False"), "False")(p);
                memo[tuple(`False`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree False(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), "Zero.False")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), "Zero.False"), "False")(TParseTree("", false,[], s));
        }
    }
    static string False(GetName g)
    {
        return "Zero.False";
    }

    static TParseTree Lambda(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CapturedList, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "Zero.Lambda")(p);
        }
        else
        {
            if (auto m = tuple(`Lambda`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CapturedList, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "Zero.Lambda"), "Lambda")(p);
                memo[tuple(`Lambda`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Lambda(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CapturedList, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "Zero.Lambda")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CapturedList, Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "Zero.Lambda"), "Lambda")(TParseTree("", false,[], s));
        }
    }
    static string Lambda(GetName g)
    {
        return "Zero.Lambda";
    }

    static TParseTree CapturedList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, CapturedVar, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "Zero.CapturedList")(p);
        }
        else
        {
            if (auto m = tuple(`CapturedList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, CapturedVar, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "Zero.CapturedList"), "CapturedList")(p);
                memo[tuple(`CapturedList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CapturedList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, CapturedVar, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "Zero.CapturedList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, CapturedVar, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "Zero.CapturedList"), "CapturedList")(TParseTree("", false,[], s));
        }
    }
    static string CapturedList(GetName g)
    {
        return "Zero.CapturedList";
    }

    static TParseTree CapturedVar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RenamedVar, Spacing), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, CapturedVar, Spacing)), Spacing))), "Zero.CapturedVar")(p);
        }
        else
        {
            if (auto m = tuple(`CapturedVar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RenamedVar, Spacing), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, CapturedVar, Spacing)), Spacing))), "Zero.CapturedVar"), "CapturedVar")(p);
                memo[tuple(`CapturedVar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CapturedVar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RenamedVar, Spacing), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, CapturedVar, Spacing)), Spacing))), "Zero.CapturedVar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, RenamedVar, Spacing), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, CapturedVar, Spacing)), Spacing))), "Zero.CapturedVar"), "CapturedVar")(TParseTree("", false,[], s));
        }
    }
    static string CapturedVar(GetName g)
    {
        return "Zero.CapturedVar";
    }

    static TParseTree RenamedVar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "Zero.RenamedVar")(p);
        }
        else
        {
            if (auto m = tuple(`RenamedVar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "Zero.RenamedVar"), "RenamedVar")(p);
                memo[tuple(`RenamedVar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RenamedVar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "Zero.RenamedVar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing)), pegged.peg.wrapAround!(Spacing, VarIdentifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":="), Spacing)), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), "Zero.RenamedVar"), "RenamedVar")(TParseTree("", false,[], s));
        }
    }
    static string RenamedVar(GetName g)
    {
        return "Zero.RenamedVar";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.literal!("$")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("$"))))), "Zero.Identifier")(p);
        }
        else
        {
            if (auto m = tuple(`Identifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.literal!("$")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("$"))))), "Zero.Identifier"), "Identifier")(p);
                memo[tuple(`Identifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.literal!("$")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("$"))))), "Zero.Identifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.literal!("$")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("$"))))), "Zero.Identifier"), "Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "Zero.Identifier";
    }

    static TParseTree Keyword(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("repeat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("until"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("and"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("not"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), Spacing), pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")), Spacing))), "Zero.Keyword")(p);
        }
        else
        {
            if (auto m = tuple(`Keyword`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("repeat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("until"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("and"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("not"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), Spacing), pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")), Spacing))), "Zero.Keyword"), "Keyword")(p);
                memo[tuple(`Keyword`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Keyword(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("repeat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("until"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("and"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("not"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), Spacing), pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")), Spacing))), "Zero.Keyword")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("repeat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("until"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("var"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("global"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("and"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("or"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("not"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), Spacing), pegged.peg.negLookahead!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")), Spacing))), "Zero.Keyword"), "Keyword")(TParseTree("", false,[], s));
        }
    }
    static string Keyword(GetName g)
    {
        return "Zero.Keyword";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Zero.Spacing")(p);
        }
        else
        {
            if (auto m = tuple(`Spacing`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Zero.Spacing"), "Spacing")(p);
                memo[tuple(`Spacing`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Zero.Spacing")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Zero.Spacing"), "Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "Zero.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(BlockComment, LineComment)), "Zero.Comment")(p);
        }
        else
        {
            if (auto m = tuple(`Comment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(BlockComment, LineComment)), "Zero.Comment"), "Comment")(p);
                memo[tuple(`Comment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(BlockComment, LineComment)), "Zero.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(BlockComment, LineComment)), "Zero.Comment"), "Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "Zero.Comment";
    }

    static TParseTree BlockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/*")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("*/")))), "Zero.BlockComment")(p);
        }
        else
        {
            if (auto m = tuple(`BlockComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/*")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("*/")))), "Zero.BlockComment"), "BlockComment")(p);
                memo[tuple(`BlockComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BlockComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/*")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("*/")))), "Zero.BlockComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/*")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("*/")))), "Zero.BlockComment"), "BlockComment")(TParseTree("", false,[], s));
        }
    }
    static string BlockComment(GetName g)
    {
        return "Zero.BlockComment";
    }

    static TParseTree LineComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "Zero.LineComment")(p);
        }
        else
        {
            if (auto m = tuple(`LineComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "Zero.LineComment"), "LineComment")(p);
                memo[tuple(`LineComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LineComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "Zero.LineComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "Zero.LineComment"), "LineComment")(TParseTree("", false,[], s));
        }
    }
    static string LineComment(GetName g)
    {
        return "Zero.LineComment";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Program(p));
        result.children = [result];
        result.name = "Zero";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return Zero(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return Zero(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "Zero";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericZero!(ParseTree).Zero Zero;

