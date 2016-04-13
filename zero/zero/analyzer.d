module zero.analyzer;

import pegged.peg : ParseTree;

class SyntaxTree
{
    ParseTree node;
    SyntaxTree[] children;
}

SyntaxTree buildSyntaxTree(ParseTree root)
{
    auto tree = build(root);
    return tree;
}

ParseTree simplifyChildren(ParseTree p)
{
    foreach(ref child; p.children)
        child = simplifyParseTree(child);
    return p;
}

ParseTree simplifyAll(ParseTree p)
{
    simplifyChildren(p);

    if (p.children.length != 1)
        return p;
    else // linear tree
        return p.children[0];
}

ParseTree discardFirstChild(Char)(ParseTree p, in Char[] name)
{
    if (p.children.length == 1 && p.children[0].name == name)
        p.children = p.children[0].children;
    return p;
}

ParseTree simplifyParseTree(ParseTree p)
{
    switch (p.name)
    {
        case "Zero.Program":
        case "Zero.FunctionStatement":
        case "Zero.ReturnStatement":
        case "Zero.ArrayExpr":
        case "Zero.RawString":
        case "Zero.QuotedString":
        case "Zero.TableExpr":
        case "Zero.ExpressionStatement":
            return simplifyChildren(p);

        case "Zero.Parameters":
            return discardFirstChild(simplifyChildren(p), "Zero.ParameterList");

        case "Zero.Argument":
            return discardFirstChild(simplifyChildren(p), "Zero.ArgumentList");

        case "Zero.VarStatement":
            return discardFirstChild(simplifyChildren(p), "Zero.VarDeclarationList");

        case "Zero.ConstStatement":
            return discardFirstChild(simplifyChildren(p), "Zero.ConstDeclarationList");

        default:
            return simplifyAll(p);
    }

    return p;
}

private SyntaxTree build(ParseTree node)
{
    SyntaxTree result = null;
    switch (node.name)
    {
        case "Zero.Expression":
            result = build(node.children[0]);
            break;

        case "Zero.AssignExpr":
        case "Zero.TernaryExpr":
        case "Zero.OrExpr":
        case "Zero.AndExpr":
        case "Zero.CompareExpr":
        case "Zero.AddExpr":
        case "Zero.MulExpr":
        case "Zero.PowExpr":
            if (node.children.length == 1)
            {
                result = build(node.children[0]);
                break;
            }
            /*else
            {
                result = new SyntaxTree();
                result.node = node;
                foreach (child; node.children)
                {
                    result.children ~= build(child);
                }
            }
            break;*/

        default:
            result = new SyntaxTree();
            result.node = node;
            foreach (child; node.children)
            {
                result.children ~= build(child);
            }
            break;
    }
    return result;
}