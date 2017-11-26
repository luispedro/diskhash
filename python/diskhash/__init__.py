from ._diskhash import Diskhash as _Diskhash
from .diskhash_version import __version__
from struct import Struct

class StructHash(object):
    def __init__(self, fname, keysize, structformat, mode, load=False):
        '''
        fname : file name
        keysize : max key size
        structformat : same argument as in the standard Python struct.Struct constructor
        mode: 'r' or 'rw'
        load: whether to load the hash table into memory
        '''
        self.s = Struct(structformat)
        self.dh = _Diskhash(fname, keysize, self.s.size, mode, int(load))

    def insert(self, key, *value):
        '''Insert a value into the hash

        Parameters
        ----------
        key: a string
        value: the value to insert

        The value will be passed to the `struct.pack` function with the format
        used to build this object

        Returns
        -------

        Whether the object was inserted (if an object already existed, it is
        *not* inserted).
        '''
        return self.dh.insert(key, memoryview(self.s.pack(*value)))

    def lookup(self, key):
        '''Lookup

        Returns None if the key is not found
        '''
        r = self.dh.lookup(key)
        if r is not None:
            return self.s.unpack(r)

    def reserve(self, n):
        '''Reserve space for future expansion

        Pre-reserving space can make building large hashes significantly
        faster.
        '''
        return self.dh.reserve(n)

    def size(self):
        'Return the size()'
        return self.dh.size()


class Str2int(StructHash):
    def __init__(self, fname, keysize, mode):
        StructHash.__init__(self, fname, keysize, "l", mode)

    def lookup(self, key):
        '''Returns the integer value'''
        val = StructHash.lookup(self, key)
        if val is not None:
            return val[0]
