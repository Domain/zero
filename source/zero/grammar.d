module zero.grammar;

import pegged.grammar;

void build(string moduleName = "zero.parser", string path = null)
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
		IfStatement < :'if' Expression :'then' Statement ( :'else' Statement )?
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
			Symbol / 
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
		Symbol <~ !Keyword [a-zA-Z_$][a-zA-Z0-9_$]*
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
		`;

    import std.string : strip, replace;
    import std.path : dirSeparator;

    auto name = moduleName.strip;
    if (path is null)
        path = name.replace(".", dirSeparator);

	asModule!()(name, path, zeroGrammar);
}

