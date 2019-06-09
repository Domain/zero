module zero.algorithm.hash;

size_t hashCombine(size_t seed, size_t value) @safe pure nothrow
{
    return seed ^ (value + 0x9e3779b9 + (seed << 6) + (seed >> 2));
}