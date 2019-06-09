module zero.network.connection;

import zero.network.address;

class Connection
{
}

bool connect(IAddress address)
{
    return false;
}

bool listen()
{
    return false;
}

size_t send(Connection conn, ubyte[] data)
{
	return 0;
}

size_t recv(Connection conn)
{
	return 0;
}