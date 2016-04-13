import pegged.grammar;



int main(string[] argv)
{
    enum semanticActions = 
`T simplify(T)(T node)
{
    /*if (node.children.length == 1)
    {
        return node.children[0];
    }*/
    return node;
}`;

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
		IfStatement < :'if' :'(' Expression :')' Statement ( :'else' Statement )?
		WhileStatement < :'while' :'(' Expression :')' Statement
		RepeatStatement < :'repeat' Statement :'until' :'(' Expression :')' :';'
		ForStatement < :'for' :'(' ForExpression :')' Statement
        ForExpression < Initialize Expression :';' Increment?
		Initialize < Statement
		Increment < Expression
		ForeachStatement < :'foreach' :'(' :'var' VarIdentifier :';' ForeachRange :')' Statement
        ForeachRange < Expression ('..' Expression)?
		ReturnStatement < 'return' Expression? :';'
		BreakStatement < 'break' :';'
		ContinueStatement < 'continue' :';'
		ExpressionStatement < Expression :';'
		VarStatement < 'global'? :'var' VarDeclarationList :';'
		VarDeclarationList < VarExpression (:',' VarExpression)*
		VarExpression < VarDeclaration (:':=' Expression)?
		VarDeclaration <- :'$' identifier
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
		AssignExpr <{simplify} TernaryExpr ( :':=' TernaryExpr )? 
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
		TableElementList < TableElement (:',' TableElementList)?
		TableElement < TernaryExpr (:':=' Expression)? (:',' TableElement)?
		ConstExpr < Identifier
		VarIdentifier <- :'$' identifier
		
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

		Identifier <~ !Keyword [a-zA-Z_][a-zA-Z0-9_]*
		Keyword < ('if' / 'else' / 'while' / 'repeat' / 'until' / 'for' / 'foreach' / 'break' / 
			'continue' / 'switch' / 'case' / 'var' / 'global' / 'return' / 'function' /
			'override' / 'null' / 'and' / 'or' / 'not' / 'true' / 'false' / 'in') ![a-zA-Z0-9_]
		Spacing <: (blank / Comment)*
		Comment <: BlockComment / LineComment
		BlockComment <~ :'/*' (!'*/' .)* :'*/'
		LineComment <~ :'//' (!endOfLine .)* :endOfLine
		`;

	asModule!()("zero.parser", "zero/parser", zeroGrammar, semanticActions);

    return 0;
}
