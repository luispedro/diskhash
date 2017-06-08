#ifdef __cplusplus
extern "C" {
#endif

typedef struct HashTableOpts {
    size_t key_maxlen;
    size_t object_datalen;
} HashTableOpts;

typedef struct HashTableHeader {
    HashTableOpts opts_;
    int cursize_;
    int slots_used_;
} HashTableHeader;

typedef struct HashTable {
    int fd_;
    const char* fname_;
    void* data_;
    size_t datasize_;
} HashTable;

typedef struct HashTableEntry {
    const char* ht_key;
    void* ht_data;
} HashTableEntry;

HashTable* dht_open(const char* fpath, HashTableOpts opts, int flags);
HashTableEntry dht_lookup(const HashTable*, const char* key);
int dht_insert(HashTable*, const char* key, const void* data);

size_t dht_reserve(HashTable*, size_t capacity);
void dht_free(HashTable*);

void show_ht(const HashTable*);

#ifdef __cplusplus
} /* extern "C" */
#endif
