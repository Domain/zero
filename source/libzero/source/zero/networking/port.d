module zero.network.port;

struct Port
{
    int port;
    alias port this;
}

struct WellKnownPort
{
    static immutable(Port) HTTP    = { 80 };
    static immutable(Port) FTP     = { 21 };
    static immutable(Port) SSH     = { 22 };
    static immutable(Port) TELNET  = { 23 };
}