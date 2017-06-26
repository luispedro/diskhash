#ifndef DISKHASH_H_INCLUDE_GUARD__
#define DISKHASH_H_INCLUDE_GUARD__
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif


/**
 * key_maxlen is the maximum key length not including the terminator NUL, i.e.,
 * diskhash will check that for every key you insert `strlen(key) <
 * opts.key_maxlen`.
 *
 * Internally, space is allocated on 8-Byte aligned boundaries, so numbers such
 * as 7, 15, 23, 31, ... (i.e., multiples of 8 minus 1 for NUL) are good
 * choices for key_maxlen.
 *
 * object_datalen is the number of Bytes that your data elements occupy.
 */
typedef struct HashTableOpts {
    size_t key_maxlen;
    size_t object_datalen;
} HashTableOpts;

typedef struct HashTable {
    int fd_;
    const char* fname_;
    void* data_;
    size_t datasize_;
} HashTable;


/** Zero-valued options
 */
HashTableOpts dht_zero_opts(void);

/** Open a hash table file
 *
 * fpath is the file path
 * flags are passed to call to open() and the user should read the documentation therein
 *
 * Values returned from dht_open must be freed with dht_free.
 *
 * Examples:
 *
 * Read-write:
 *
 *      HashTableOpts opts;
 *      opts.key_maxlen = 15;
 *      opts.object_datalen = 8;
 *      char* err;
 *      HashTable* ht = dht_open("hashtable.dht", opts, O_RDWR|O_CREAT, &err);
 *
 * Read-only:
 *
 *      char* err;
 *      HashTable* ht = dht_open("hashtable.dht", opts, O_RDONLY, &err);
 *
 * When opening an existing disk table, you can pass `{ 0, 0 }` (the return
 * value of `dht_zero_opts()`) as the options, in which case the values will be
 * taken from the table on disk. If you do pass values, they are checked
 * against the values on disk and it is an error if there is a mismatch.
 *
 * The last argument is an error output argument. If it is set to a non-NULL
 * value, then the memory must be released with free(). Passing NULL is valid
 * (and no error message will be produced). An error return with *err == NULL
 * will mean an out-of-memory error (when dht fails to allocate memory, it does
 * not try to allocate memory for an error message).
 */
HashTable* dht_open(const char* fpath, HashTableOpts opts, int flags, char**);

/** Lookup a value by key
 *
 * If the hash table was opened in read-write mode, then the memory returned
 * can be written to (the hash table itself does not inspect the values in any
 * way). Writing to a read-only hashtable will probably trigger a segmentation
 * fault.
 *
 * If the object is not found, returns NULL.
 *
 * Thread safety: multiple concurrent reads are perfectly safe. No guarantees
 * are given whenever writing is performed. Similarly, if you write to the
 * output of this function (the ht_data field), no guarantees are given.
 */
void* dht_lookup(const HashTable*, const char* key);

/** Insert a value.
 *
 * The hashtable must be opened in read write mode.
 *
 * If a value with the given key is already present in the table, then no
 * action is performed and 0 is returned. If you want to overwrite that value,
 * you can use `dht_lookup` and write to its output.
 *
 * This operation is typically O(1) amortized. However, if table is at capacity
 * when dht_insert is called, then it must be grown which can be a
 * time-consuming operation as all the values are copied to the newly allocated
 * memory block (see dht_reserve).
 *
 * Errors can occur if table expansion is needed and memory cannot be
 * allocated.
 *
 * Returns 1 if the value was inserted.
 *         0 if the key was already present in the table. The hash table was
 *         not modified.
 *         -EINVAL : key is too long
 *	   -ENOMEM : dht_reserve failed.
 *
 * The last argument is an error output argument. If it is set to a non-NULL
 * value, then the memory must be released with free(). Passing NULL is valid
 * (and no error message will be produced). An error return with *err == NULL
 * will mean an out-of-memory error (when dht fails to allocate memory, it does
 * not try to allocate memory for an error message).
 */
int dht_insert(HashTable*, const char* key, const void* data, char** err);

/** Preallocate memory for the table.
 *
 * Calling this function if the number of elements is known apriori can improve
 * performance. Additionally, if capacity exists, then dht_insert never fails.
 *
 * This function returns the actual capacity allocated (which may be more than
 * requested, but never less). Calling dht_reserve asking for _less_ capacity
 * than is currently used is a no-op.
 *
 * If capacity cannot be allocated, this function returns 0 (but no changes to
 * the hash table are made).
 *
 * This function can be used to query the current capacity by passing the value
 * 1 as the desired capacity.
 *
 * The last argument is an error output argument. If it is set to a non-NULL
 * value, then the memory must be released with free(). Passing NULL is valid
 * (and no error message will be produced).
 */
size_t dht_reserve(HashTable*, size_t capacity, char** err);

/**
 * Return the number of elements
 */
size_t dht_size(const HashTable*);

/** Free the hashtable and sync to disk.
 */
void dht_free(HashTable*);

/** For debug use only */
void show_ht(const HashTable*);


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* DISKHASH_H_INCLUDE_GUARD__*/
