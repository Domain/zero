module zero.network.address.address;

public import zero.network.port;

interface IAddress
{
@property:
    string host();
    void host(string);
    Port port();
    void port(Port);
}

IAddress resolveHost(string host)
{
    return null;
}