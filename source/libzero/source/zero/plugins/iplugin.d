module zero.plugins.iplugin;

import zero.plugins.metadata;

/// You don't have to implement this interface, just for convenient
interface IPlugIn
{
    bool Load();
    bool Start();
    bool Stop();
    void Unload();
    //void Update(long deltaMS);
    bool Upgrade();
}

extern(C) alias MetaData function() getMetaDataFn;

extern(C) alias int function(void* /*handle*/, int/*opcode*/, long/*in param*/, void*/*in param*/, void*/*out param*/) controlFn;
extern(C) alias bool function(void*, controlFn) startFn;
extern(C) alias bool function() stopFn;
extern(C) alias void function(long) updateFn;
extern(C) alias bool function(void*, const(MetaData)*) upgradeFn;

/* Need to implement following functions:
MetaData GetMetaData();
bool Start();
bool Stop();
*/