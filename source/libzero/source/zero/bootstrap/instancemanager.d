module zero.bootstrap.instancemanager;

import core.sync.mutex;

class InstanceManager
{
    this()
    {

    }

    bool isOnlyInstance() @property
    {
        return true;
    }

    void run(string[] args)
    {

    }
}
