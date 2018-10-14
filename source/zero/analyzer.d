module zero.analyzer;

import std.stdio;
import std.conv : to;
import pegged.peg : ParseTree, position;
import zero.symboltable;

class SyntaxTree
{
    ParseTree node;
    Symbol symbol;
    SyntaxTree[] children;

    alias node this;

    this(ParseTree pt)
    {
        node = pt;
    }

    override string toString() const
    {
        return toString("");
    }

	string toString(string tabs = "") const
    {
        string result = node.name;
        string childrenString;
        foreach(i,child; children)
        {
            childrenString ~= tabs ~ " +-" ~ child.toString(tabs ~ ((i < children.length -1 ) ? " | " : "   "));
        }

        result ~= " " ~ to!string([node.begin, node.end]) ~ to!string(node.matches) ~ "\n";
        return result ~ childrenString;
    }
}

SyntaxTree buildSyntaxTree(ParseTree root)
{
    auto tree = buildTree(root);
    //auto stack = new SymbolTableStack();
    //buildTables(tree, stack);
    return tree;
}

SyntaxTree simplifyChildren(ParseTree p)
{
    auto node = new SyntaxTree(p);
    foreach(child; p.children)
        node.children ~= buildTree(child);
    return node;
}

SyntaxTree simplifyAll(ParseTree p)
{
    // auto node = simplifyChildren(p);
    if (p.children.length != 1)
        return simplifyChildren(p);
    else // linear tree
        return buildTree(p.children[0]);
}

SyntaxTree discardFirstChild(Char)(ParseTree p, in Char[] name)
{
    SyntaxTree node = simplifyChildren(p);
    
    if (node.children.length == 1 && node.children[0].name == name)
    {
        node.children = node.children[0].children;
    }
    return node;
}

SyntaxTree buildTree(ParseTree p)
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
            return discardFirstChild(p, "Zero.ParameterList");

        case "Zero.Argument":
            return discardFirstChild(p, "Zero.ArgumentList");

        case "Zero.VarStatement":
            return discardFirstChild(p, "Zero.VarDeclarationList");

        case "Zero.ConstStatement":
            return discardFirstChild(p, "Zero.ConstDeclarationList");

        case "Zero.ArrayExpr":
            return discardFirstChild(p, "Zero.ArrayElement");

        case "Zero.TableExpr":
            return discardFirstChild(p, "Zero.TableElementList");

        default:
            return simplifyAll(p);
    }
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
        case "Zero.ConstStatement":
            foreach (child; node.children)
            {
                auto name = child.matches[0];

                if (node.matches[0] == "global")
                {
                    child.symbol = stack.enterGlobal(name);
                }
                else
                {
                    child.symbol = stack.enterLocal(name);
                }
                child.symbol.positions ~= position(child.node);
            }
            break;

        case "Zero.Symbol":
            auto name = node.node.matches[0];
            auto symbol = stack.lookup(name);

            if (symbol is null)
            {
                writefln("Undefined symbol %s at line %s", name, position(node.node));
            }
            else
            {
                symbol.positions ~= position(node.node);
            }

            break;

        default:
            foreach (child; node.children)
            {
                buildTables(child, stack);
            }
            break;
    }
}
