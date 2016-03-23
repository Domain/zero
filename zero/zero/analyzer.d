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