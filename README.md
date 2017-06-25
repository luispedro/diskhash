# Disk-based hashtable

[![Travis](https://api.travis-ci.org/luispedro/diskhash.png)](https://travis-ci.org/luispedro/diskhash)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)


A simple disk-based hash table.

The code is in C, but Python and Haskell wrappers are provided too. The
wrappers follow similar APIs with variations to accomodate the language
specificity. They all use the same underlying code, so you can open a hashtable
created in C from Haskell, modify it within your Haskell code, and open the
result in Python (although Python's version currently only deals with integers,
stored as longs). This will work best for very simple types so that you can
control their binary representation (64-bit integers, for example).

## Examples

The following examples all create a hashtable to store longs (`int64_t`), then
set the value associated with the key `"key"` to 9. In the current API, the
maximum size of the keys needs to be pre-specified, which is the value `15`
below.

### Raw C

```c
#include <stdio.h>
#include <inttypes.h>
#include "diskhash.h"

int main(void) {
    HashTableOpts opts;
    opts.key_maxlen = 15;
    opts.object_datalen = sizeof(int64_t);
    char* err = NULL;
    HashTable* ht = dht_open("testing.dht", opts, O_RDWR|O_CREAT, &err);
    if (!ht) {
        if (!err) err = "Unknown error";
        fprintf(stderr, "Failed opening hash table: %s.\n", err);
        return 1;
    }
    long i = 9;
    dht_insert(ht, "key", &i);
    
    long* val = (long*) dht_lookup(ht, "key");
    printf("Looked up value: %l\n", *val);

    dht_free(ht);
    return 0;
}
```

### Haskell

In Haskell, you have different types/functions for read-write and read-only
hashtables.

Read write example:

```haskell
import Data.DiskHash
import Data.Int
main = do
    ht <- htOpenRW "testing.dht" 15
    htInsertRW ht "key" (9 :: Int64)
    val <- htLookupRW "key" ht
    print val
```

Read only example (`htLookupRO` is pure in this case):

```haskell
import Data.DiskHash
import Data.Int
main = do
    ht <- htOpenRO "testing.dht" 15
    let val :: Int64
        val = htLookupRO "key" ht
    print val
```


### Python

Python's interface is more limited and only integers are supported as values in
the hash table (they are stored as 64-bit integers).

```python
import diskhash
tb = diskhash.Str2int("testing.dht", 15)
tb.insert("key", 9)
print(tb.lookup("key"))
```

The Python interface is currently Python 3 only. Patches to extend it to 2.7
are welcome, but it's not a priority.

## Statibility

This is _beta_ software. It is good enough that I am using it, but the API can
change in the future with little warning. The binary format will be fixed once
there is an upload to PyPI or Stackage, but that format is versioned (the magic
string encodes its version, so changes can be detected).

License: MIT

