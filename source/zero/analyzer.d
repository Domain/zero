module zero.analyzer;

import pegged.peg : ParseTree, position;
import zero.symboltable;

class SyntaxTree
{
    ParseTree node;
    Symbol symbol;
    SyntaxTree[] children;
}

SyntaxTree buildSyntaxTree(ParseTree root)
{
    auto tree = buildTree(root);
    auto stack = new SymbolTableStack();
    buildTables(tree, stack);
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
        case "Zero.RawString":
        case "Zero.QuotedString":
        case "Zero.ExpressionStatement":
        case "Zero.ConstExpr":
        case "Zero.TableElement":
            return simplifyChildren(p);

        case "Zero.Parameters":
            return discardFirstChild(simplifyChildren(p), "Zero.ParameterList");

        case "Zero.Argument":
            return discardFirstChild(simplifyChildren(p), "Zero.ArgumentList");

        case "Zero.VarStatement":
            return discardFirstChild(simplifyChildren(p), "Zero.VarDeclarationList");

        case "Zero.ConstStatement":
            return discardFirstChild(simplifyChildren(p), "Zero.ConstDeclarationList");

        case "Zero.ArrayExpr":
            return discardFirstChild(simplifyChildren(p), "Zero.ArrayElement");

        case "Zero.TableExpr":
            return discardFirstChild(simplifyChildren(p), "Zero.TableElementList");

        default:
            return simplifyAll(p);
    }
}

private SyntaxTree buildTree(ParseTree node)
{
    SyntaxTree result = null;
    switch (node.name)
    {
        case "Zero.Expression":
            result = buildTree(node.children[0]);
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
                result = buildTree(node.children[0]);
                break;
            }

            goto default;
            /*else
            {
                result = new SyntaxTree();
                result.node = node;
                foreach (child; node.children)
                {
                    result.children ~= buildTree(child);
                }
            }
            break;*/

        default:
            result = new SyntaxTree();
            result.node = node;
            foreach (child; node.children)
            {
                result.children ~= buildTree(child);
            }
            break;
    }
    return result;
}

private void buildTables(SyntaxTree node, SymbolTableStack stack)
{
    switch (node.node.name)
    {
        case "Zero.BlockStatement":
            stack.push();
            foreach (child; node.children)
            {
                buildTables(child, stack);
            }
            stack.pop();
            break;

        case "Zero.VarStatement":
            auto name = node.children[0].children[0].node.matches[0];

            if (node.node.matches[0] == "global")
            {
                node.symbol = stack.enterGlobal(name);
            }
            else
            {
                node.symbol = stack.enterLocal(name);
            }

            node.symbol.positions ~= position(node.node);
            break;

        default:
            break;
    }
}
