module zero.network.address.ipv4_address;

import zero.network.address.address;

class IPv4Address : IAddress
{
@property:
    string host() { return _host; }
    void host(string addr) { _host = addr; }
    Port port() { return _port; }
    void port(Port p) { _port = p; }

private:
    string _host;
    Port _port;
}