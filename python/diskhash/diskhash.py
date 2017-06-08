import ctypes
from os import path

from ctypes.util import find_library
for c in [
            find_library('dht'),
            './libdht.so',
            path.join(path.dirname(__file__), '../../..', 'libdht.so'),
            path.join(path.dirname(__file__), '../../../..', 'libdht.so')]:
    if c is not None and path.exists(c):
        break
else:
    raise IOError("Could not find libdht")
libdht = ctypes.cdll.LoadLibrary(c)

class HashTable(ctypes.Structure):
    pass

class HashTableOpts(ctypes.Structure):
    _fields_ = [
                ("key_maxlen", ctypes.c_size_t),
                ("object_datalen", ctypes.c_size_t)
               ]
    
class HashTableEntry(ctypes.Structure):
    _fields_ = [
                ("ht_key", ctypes.c_char_p),
                ("ht_object", ctypes.c_void_p)
               ]


def errorIfMinus1(v):
    if v == -1:
        raise ValueError("Error in libdht call")
    return v
hashtable_t = ctypes.POINTER(HashTable)

libdht.dht_open.argtypes = [ctypes.c_char_p, HashTableOpts, ctypes.c_int]
libdht.dht_open.restype = hashtable_t

libdht.dht_lookup.argtypes = [hashtable_t, ctypes.c_char_p]
libdht.dht_lookup.restype = HashTableEntry

libdht.dht_insert.argtypes = [hashtable_t, ctypes.c_char_p, ctypes.c_void_p]
libdht.dht_insert.restype = errorIfMinus1

libdht.dht_reserve.argtypes = [hashtable_t, ctypes.c_size_t]
libdht.dht_reserve.restype = ctypes.c_size_t

libdht.dht_free.argtypes = [hashtable_t]
libdht.dht_free.restype = None

libdht.show_ht.argtypes = [hashtable_t]
libdht.show_ht.restype = None

class DiskStr2Int(object):
    '''Disk based hash table encoding str -> int mapping'''
    def __init__(self, fname, max_keysize, mode):
        fname = fname.encode('utf-8')
        opts = HashTableOpts(max_keysize, 8)
        mode = {
                'w' : 66,
                'rw' : 66,
                'r' : 0,
                }[mode]
        self.ht = libdht.dht_open(fname, opts, mode)
        if not self.ht:
            raise IOError("Could not load hashtable")
        

    def lookup(self, key):
        '''Look up a value
        Parameters
        ----------
        key : bytes

        Returns
        -------
        val : int or None
        '''
        key = key.encode('utf-8')
        he = libdht.dht_lookup(self.ht, ctypes.c_char_p(key))
        r = ctypes.cast(he.ht_object, ctypes.POINTER(ctypes.c_long))
        if r:
            return r.contents.value

    def insert(self, key, datum):
        '''Insert a value into the hash table
        Parameters
        ----------
        key : bytes
            Key
        datum : value
        '''
        key = key.encode('utf-8')
        datum = ctypes.c_long(datum)
        return libdht.dht_insert(self.ht, key, ctypes.pointer(datum))

    def __del__(self):
        if self.ht:
            libdht.dht_free(self.ht)
