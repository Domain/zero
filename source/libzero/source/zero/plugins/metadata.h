#ifndef _METADATA_H_
#define _METADATA_H_

#include "zero/version.h"
#include "dependency.h"
#include "iplugin.h"
#include <type_traits>

struct MetaData
{
    const char* name = 0;
    Version ver;
    Dependency* dependencies = nullptr;
    size_t numberOfDependences = 0;

    startFn* start = nullptr;
    stopFn* stop = nullptr;
    updateFn* update = nullptr;
};

template<typename T, const char* fname, const char* candidate>
void bindFunction(MetaData& metadata)
{
    std::is_
}

template<typename T>
MetaData _GetMetaData(const char* name)
{
    MetaData metadata;
    metadata.name = name;
    return metadata;
}

#define getMetaData(T)  _GetMetaData<T>(#T)

#endif // !_METADATA_H_
