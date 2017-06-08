#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "diskhash.h"


typedef int64_t data_t;
int dht_ismember(HashTable* ht, const char* k) {
    HashTableEntry et = dht_lookup(ht, k);
    return et.ht_key != NULL;
}

void chomp(char* p) {
    for ( ; *p ; ++p) {
        if (*p == '\n') {
            *p = '\0';
            return;
        }
    }
}

data_t dht_lookup_data_or(HashTable* ht, const char* k, data_t def) {
    HashTableEntry et = dht_lookup(ht, k);
    if (!et.ht_data) return def;
    memcpy(&def, et.ht_data, sizeof(def));
    return def;
}
int main() {
    HashTableOpts opts;
    opts.key_maxlen = 15;
    opts.object_datalen = sizeof(data_t);
    HashTable* ht = dht_open("testing.dht", opts, O_RDWR|O_CREAT);
    if (!ht) {
        fprintf(stderr, "Failed opening hash table.\n");
        return 1;
    }

    char buffer[256];
    data_t i = 9;
    while (fgets(buffer, 255, stdin)) {
        chomp(buffer);
        printf("Looking for %s: %ld\n", buffer, dht_lookup_data_or(ht, buffer, -1));
        dht_insert(ht, buffer, &i);
        ++i;
    }
    show_ht(ht);
    dht_free(ht);
}
