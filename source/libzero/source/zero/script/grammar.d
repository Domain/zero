module zero.script.grammar;

import pegged.grammar;

void build(string moduleName = "zero.script.parser", string path = null)
{
	enum zeroGrammar = `
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
		IfStatement < :'if' Condition :'then' Statement ( :'else' Statement )?
		Condition < Expression / :'(' VarStatement Expression :')'
		WhileStatement < :'while' Expression :'do' Statement
		RepeatStatement < :'repeat' Statement :'until' Expression :';'
		ForStatement < :'for' :'(' ForExpression :')' :'do' Statement
        ForExpression < Initialize Expression :';' Increment?
		Initialize < Statement
		Increment < Expression
		ForeachStatement < :'foreach' :'(' :'var' Symbol :';' ForeachRange :')' :'do' Statement
        ForeachRange < Expression ('..' Expression)?
		ReturnStatement < 'return' Expression? :';'
		BreakStatement < 'break' :';'
		ContinueStatement < 'continue' :';'
		ExpressionStatement < Expression :';'
		VarStatement < 'global'? :'var' VarDeclarationList :';'
		VarDeclarationList < VarDeclaration (:',' VarDeclaration)*
		VarDeclaration < Symbol (:':=' Expression)?
		ConstStatement < 'global'? :'const' ConstDeclarationList :';'
		ConstDeclarationList < ConstDeclaration (:',' ConstDeclaration)*
		ConstDeclaration < Symbol :'=' Expression
		FunctionStatement < :'function' Symbol Parameters FunctionAttributes Statement
		Parameters < '(' ParameterList? ')'
		ParameterList < Parameter (:',' Parameter)*
		Parameter < 'var' VarDeclaration
		FunctionAttributes < FunctionAttribute*
        FunctionAttribute <- 'override' / 'server' / 'client'
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
		MemberCall <- PostExpr :'.' Symbol
		TableIndex < PostExpr :'{' Expression :'}'
		ArrayIndex < PostExpr :'[' Expression :']'
		CallExpr < PostExpr Argument
        Argument < '(' ArgumentList? ')'
		ArgumentList < Expression (:',' Expression)*
		PrimaryExpr < :'(' Expression :')' / 
			ArrayExpr / 
			TableExpr / 
			RealLiteral / 
			IntegerLiteral / 
			String / 
			This / 
			Null / 
			True / 
			False / 
			Symbol / 
			Lambda
		ArrayExpr < '[' ArrayElement? :','? ']'
		ArrayElement < Expression (:',' Expression)*
		TableExpr < '{' TableElementList? :','? '}'
		TableElementList < TableElement (:',' TableElement)*
		TableElement < TernaryExpr (:':=' Expression)?
		ConstExpr < Symbol
		
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
		CapturedVar < (RenamedVar / Symbol) (:',' CapturedVar)*
		RenamedVar < :'var' Symbol :':=' Expression
		Symbol <~ SymbolFirst SymbolChar*
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
                'do') !SymbolChar
        SymbolFirst <- [a-zA-Z_$\u00A8\u00AA\u00AD\u00AF\u00B2-\u00B5\u00B7-\u00BA\u00BC-\u00BE\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF\u0100-\u02FF\u0370-\u167F\u1681-\u180D\u180F-\u1DBF\u1E00-\u1FFF\u200B-\u200D\u202A-\u202E\u203F-\u2040\u2054\u2060-\u206F\u2070-\u20CF\u2100-\u218F\u2460-\u24FF\u2776-\u2793\u2C00-\u2DFF\u2E80-\u2FFF\u3004-\u3007\u3021-\u302F\u3031-\u303F\u3040-\uD7FF\uF900-\uFD3D\uFD40-\uFDCF\uFDF0-\uFE1F\uFE30-\uFE44\uFE47-\uFFFD\U00010000-\U0001FFFD\U00020000-\U0002FFFD\U00030000-\U0003FFFD\U00040000-\U0004FFFD\U00050000-\U0005FFFD\U00060000-\U0006FFFD\U00070000-\U0007FFFD\U00080000-\U0008FFFD\U00090000-\U0009FFFD\U000A0000-\U000AFFFD\U000B0000-\U000BFFFD\U000C0000-\U000CFFFD\U000D0000-\U000DFFFD\U000E0000-\U000EFFFD]
        SymbolChar <- SymbolFirst / [0-9\u0300-\u036F\u1DC0-\u1DFF\u20D0-\u20FF\uFE20-\uFE2F]
		Spacing <: (blank / Comment)*
		Comment <: BlockComment / LineComment
		BlockComment <~ :'/*' (!'*/' .)* :'*/'
		LineComment <~ :'//' (!endOfLine .)* :endOfLine
		`;

	import std.string : strip, replace;
	import std.path : dirSeparator;

	auto name = moduleName.strip;
	if (path is null)
		path = name.replace(".", dirSeparator);

	asModule!()(name, path, zeroGrammar);
}
