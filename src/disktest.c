#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "diskhash.h"


typedef int64_t data_t;
int dht_ismember(HashTable* ht, const char* k) {
    return dht_lookup(ht, k) != NULL;
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
    void* data = dht_lookup(ht, k);
    if (!data) return def;
    memcpy(&def, data, sizeof(def));
    return def;
}
int main() {
    HashTableOpts opts;
    opts.key_maxlen = 15;
    opts.object_datalen = sizeof(data_t);
    char* err;
    HashTable* ht = dht_open("testing.dht", opts, O_RDWR|O_CREAT, &err);
    if (!ht) {
        fprintf(stderr, "Failed opening hash table: %s.\n", err);
        free(err);
        return 1;
    }

    char buffer[256];
    data_t i = 9;
    while (fgets(buffer, 255, stdin)) {
        chomp(buffer);
        printf("Looking for %s: %ld\n", buffer, dht_lookup_data_or(ht, buffer, -1));
        int v = dht_insert(ht, buffer, &i, &err);
        if (v < 1) {
            printf("dht_insert returned %d; %s.\n", v, err);
            free(err);
        }
        ++i;
    }
    show_ht(ht);
    dht_free(ht);
}
