#include "diskhash.h"
HashTable* dht_open2(const char* f, unsigned int key_maxlen, unsigned int object_datalen, int flags, char** err) {
    HashTableOpts opts;
    opts.key_maxlen = key_maxlen;
    opts.object_datalen = object_datalen;
    return dht_open(f, opts, flags, err);
}
