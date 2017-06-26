#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <unistd.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include "diskhash.h"
#include "primes.h"

typedef struct HashTableHeader {
    char magic[16];
    HashTableOpts opts_;
    size_t cursize_;
    size_t slots_used_;
} HashTableHeader;

typedef struct HashTableEntry {
    const char* ht_key;
    void* ht_data;
} HashTableEntry;

static
uint64_t hash_key(const char* k) {
    /* Taken from http://www.cse.yorku.ca/~oz/hash.html */
    uint64_t hash = 5381;
    for ( ; *k; ++k) {
        hash *= 33;
        hash ^= (uint64_t)*k;
    }
    return hash;
}

inline static
size_t aligned_size(size_t s) {
    size_t s_8bytes = s & ~0x7;
    return s_8bytes == s ? s : (s_8bytes + 8);
}

inline static
HashTableHeader* header_of(HashTable* ht) {
    return (HashTableHeader*)ht->data_;
}

inline static
const HashTableHeader* cheader_of(const HashTable* ht) {
    return (const HashTableHeader*)ht->data_;
}

inline static
int is_64bit(const HashTable* ht) {
    return cheader_of(ht)->cursize_ > (1L << 32);
}

inline static
size_t node_size_opts(HashTableOpts opts) {
    return aligned_size(opts.key_maxlen + 1) + aligned_size(opts.object_datalen);
}

inline static
size_t node_size(const HashTable* ht) {
    return node_size_opts(cheader_of(ht)->opts_);
}

inline static
int entry_empty(const HashTableEntry et) {
    return !et.ht_key;
}

void* hashtable_of(HashTable* ht) {
    return (unsigned char*)ht->data_ + sizeof(HashTableHeader);
}


static
uint64_t get_table_at(const HashTable* ht, uint64_t ix) {
    if (is_64bit(ht)) {
        uint64_t* table = (uint64_t*)hashtable_of((HashTable*)ht);
        return table[ix];
    } else {
        uint32_t* table = (uint32_t*)hashtable_of((HashTable*)ht);
        return table[ix];
    }
}

static
void set_table_at(HashTable* ht, uint64_t ix, const uint64_t val) {
    if (is_64bit(ht)) {
        uint64_t* table = (uint64_t*)hashtable_of(ht);
        table[ix] = val;
    } else {
        uint32_t* table = (uint32_t*)hashtable_of(ht);
        table[ix] = val;
    }
}

void show_ht(const HashTable* ht) {
    fprintf(stderr, "HT {\n"
                "\tmagic = \"%s\",\n"
                "\tcursize = %d,\n"
                "\tslots used = %ld\n"
                "\n", cheader_of(ht)->magic, (int)cheader_of(ht)->cursize_, cheader_of(ht)->slots_used_);

    int i;
    for (i = 0; i < cheader_of(ht)->cursize_; ++i) {
        fprintf(stderr, "\tTable [ %d ] = %d\n",(int)i, (int)get_table_at(ht, i));
    }
    fprintf(stderr, "}\n");
}

static
HashTableEntry entry_at(const HashTable* ht, size_t ix) {
    ix = get_table_at(ht, ix);
    HashTableEntry r;
    if (ix == 0) {
        r.ht_key = 0;
        r.ht_data = 0;
        return r;
    }
    --ix;
    const size_t sizeof_table_elem = is_64bit(ht) ? sizeof(uint64_t) : sizeof(uint32_t);
    const char* node_data = (const char*)ht->data_
                            + sizeof(HashTableHeader)
                            + cheader_of(ht)->cursize_ * sizeof_table_elem;
    r.ht_key = node_data + ix * node_size(ht);
    r.ht_data = (void*)( node_data + ix * node_size(ht) + aligned_size(cheader_of(ht)->opts_.key_maxlen + 1) );
    return r;
}

HashTableOpts dht_zero_opts() {
    HashTableOpts r;
    r.key_maxlen = 0;
    r.object_datalen = 0;
    return r;
}

HashTable* dht_open(const char* fpath, HashTableOpts opts, int flags, char** err) {
    if (!fpath || !*fpath) return NULL;
    const int fd = open(fpath, flags, 0644);
    int needs_init = 0;
    if (fd < 0) {
        if (err) { *err = strdup("open call failed."); }
        return NULL;
    }
    HashTable* rp = (HashTable*)malloc(sizeof(HashTable));
    if (!rp) {
        if (err) { *err = NULL; }
        return NULL;
    }
    rp->fd_ = fd;
    rp->fname_ = strdup(fpath);
    if (!rp->fname_) {
        if (err) { *err = NULL; }
        close(rp->fd_);
        free(rp);
        return NULL;
    }
    struct stat st;
    fstat(rp->fd_, &st);
    rp->datasize_ = st.st_size;
    if (rp->datasize_ == 0) {
        needs_init = 1;
        rp->datasize_ = sizeof(HashTableHeader) + 7 * sizeof(uint32_t) + 3 * node_size_opts(opts);
        if (ftruncate(fd, rp->datasize_) < 0) {
            if (err) { *err = strdup("Could not allocate disk space."); }
            close(rp->fd_);
            free((char*)rp->fname_);
            free(rp);
            return NULL;
        }
    }
    const int prot = (flags == O_RDONLY) ?
                                PROT_READ
                                : PROT_READ|PROT_WRITE;
    rp->data_ = mmap(NULL, 
            rp->datasize_,
            prot,
            MAP_SHARED,
            rp->fd_,
            0);
    if (rp->data_ == MAP_FAILED) {
        if (err) { *err = strdup("mmap() call failed."); }
        close(rp->fd_);
        free((char*)rp->fname_);
        free(rp);
        return NULL;
    }
    if (needs_init) {
        strcpy(header_of(rp)->magic, "DiskBasedHash10");
        header_of(rp)->opts_ = opts;
        header_of(rp)->cursize_ = 7;
        header_of(rp)->slots_used_ = 0;
    } else if (strcmp(header_of(rp)->magic, "DiskBasedHash10")) {
        char start[16];
        strncpy(start, header_of(rp)->magic, 14);
        start[13] = '\0';
        if (!strcmp(start, "DiskBasedHash")) {
            if (err) { *err = strdup("Version mismatch. This code can only load version 1.0."); }
        } else {
            if (err) { *err = strdup("No magic number found."); }
        }
        dht_free(rp);
        return 0;
    } else if (header_of(rp)->opts_.key_maxlen != opts.key_maxlen
                || header_of(rp)->opts_.object_datalen != opts.object_datalen) {
        if (err) { *err = strdup("Options mismatch."); }
        dht_free(rp);
        return 0;
    }
    return rp;
}

void dht_free(HashTable* ht) {
    munmap(ht->data_, ht->datasize_);
    fsync(ht->fd_);
    close(ht->fd_);
    free((char*)ht->fname_);
    free(ht);
}

char random_char(void) {
    const char* available =
        "0123456789"
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    return available[rand() % (26*2 + 10)];
}


char* generate_tempname_from(const char* base) {
    char* res = (char*)malloc(strlen(base) + 21);
    if (!res) return NULL;
    strcpy(res, base);
    char* p = res;
    while (*p) ++p;
    *p++ = '.';
    int i;
    for (i = 0; i < 19; ++i) {
        *p++ = random_char();
    }
    *p = 0;
    return res;
}

size_t dht_reserve(HashTable* ht, size_t cap, char** err) {
    if (header_of(ht)->cursize_ / 2 > cap) {
        return header_of(ht)->cursize_ / 2;
    }
    const int starting_slots = cheader_of(ht)->slots_used_;
    const int min_slots = cap * 2 + 1;
    int i = 0;
    while (primes[i] && primes[i] < min_slots) ++i;
    const int n = primes[i];
    cap = n / 2;
    const size_t sizeof_table_elem = is_64bit(ht) ? sizeof(uint64_t) : sizeof(uint32_t);
    const size_t total_size = sizeof(HashTableHeader) + n * sizeof_table_elem + cap * node_size(ht);

    HashTable* temp_ht = (HashTable*)malloc(sizeof(HashTable));
    while (1) {
        temp_ht->fname_ = generate_tempname_from(ht->fname_);
        if (!temp_ht->fname_) {
            if (err) { *err = NULL; }
            free(temp_ht);
            return 0;
        }
        temp_ht->fd_ = open(temp_ht->fname_, O_EXCL | O_CREAT | O_RDWR, 0600 );
        if (temp_ht->fd_) break;
        free((char*)temp_ht->fname_);
    }
    if (ftruncate(temp_ht->fd_, total_size) < 0) {
        if (err) { *err = strdup("Could not allocate disk space."); }
        free((char*)temp_ht->fname_);
        free(temp_ht);
        return 0;
    }
    temp_ht->datasize_ = total_size;
    temp_ht->data_ = mmap(NULL, 
            temp_ht->datasize_,
            PROT_READ|PROT_WRITE,
            MAP_SHARED,
            temp_ht->fd_,
            0);
    if (temp_ht->data_ == MAP_FAILED) {
        if (err) {
            const int errorbufsize = 512;
            *err = (char*)malloc(errorbufsize);
            if (*err) {
                snprintf(*err, errorbufsize, "Could not mmap() new hashtable: %s.\n", strerror(errno));
            }
        }
        close(temp_ht->fd_);
        unlink(temp_ht->fname_);
        free((char*)temp_ht->fname_);
        free(temp_ht);
        return 0;
    }
    memcpy(header_of(temp_ht), header_of(ht), sizeof(HashTableHeader));
    header_of(temp_ht)->cursize_ = n;
    header_of(temp_ht)->slots_used_ = 0;

    HashTableEntry et;
    for (i = 0; i < header_of(ht)->slots_used_; ++i) {
        set_table_at(ht, 0, i + 1);
        et = entry_at(ht, 0);
        dht_insert(temp_ht, et.ht_key, et.ht_data, NULL);
    }

    const char* temp_fname = strdup(temp_ht->fname_);
    if (!temp_fname) {
        if (err) { *err = NULL; }
        unlink(temp_ht->fname_);
        dht_free(temp_ht);
        return 0;
    }

    dht_free(temp_ht);
    const HashTableOpts opts = header_of(ht)->opts_;

    munmap(ht->data_, ht->datasize_);
    close(ht->fd_);

    rename(temp_fname, ht->fname_);

    temp_ht = dht_open(ht->fname_, opts, O_RDWR, err);
    if (!temp_ht) {
        /* err is set by dht_open */
        return 0;
    }
    free((char*)ht->fname_);
    memcpy(ht, temp_ht, sizeof(HashTable));
    assert(starting_slots == cheader_of(ht)->slots_used_);
    return cap;
}

size_t dht_size(const HashTable* ht) {
    return cheader_of(ht)->slots_used_;
}

void* dht_lookup(const HashTable* ht, const char* key) {
    int h = hash_key(key) % cheader_of(ht)->cursize_;
    int i;
    for (i = 0; i < cheader_of(ht)->cursize_; ++i) {
        HashTableEntry et = entry_at(ht, h);
        if (!et.ht_key) return NULL;
        if (!strcmp(et.ht_key, key)) return et.ht_data;
        ++h;
        if (h == cheader_of(ht)->cursize_) h = 0;
    }
    fprintf(stderr, "dht_lookup: the code should never have reached this line.\n");
    return NULL;
}

int dht_insert(HashTable* ht, const char* key, const void* data, char** err) {
    if (strlen(key) >= header_of(ht)->opts_.key_maxlen) {
        if (err) { *err = strdup("Key is too long"); }
        return -EINVAL;
    }
    /* Max load is 50% */
    if (cheader_of(ht)->cursize_ / 2 <= cheader_of(ht)->slots_used_) {
        if (!dht_reserve(ht, cheader_of(ht)->slots_used_ + 1, err)) return -ENOMEM;
    }
    int h = hash_key(key) % cheader_of(ht)->cursize_;
    while (1) {
        HashTableEntry et = entry_at(ht, h);
        if (entry_empty(et)) break;
        if (!strcmp(et.ht_key, key)) {
            return 0;
        }
        ++h;
        if (h == cheader_of(ht)->cursize_) {
            h = 0;
        }
    }
    set_table_at(ht, h, header_of(ht)->slots_used_ + 1);
    ++header_of(ht)->slots_used_;
    HashTableEntry et = entry_at(ht, h);

    strcpy((char*)et.ht_key, key);
    memcpy(et.ht_data, data, cheader_of(ht)->opts_.object_datalen);

    return 1;
}

