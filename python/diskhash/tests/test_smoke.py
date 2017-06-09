from diskhash import Str2int
from os import unlink

filename = 'testing.dht'

def test_insert_check():
    ht = Str2int(filename, 17, 'rw')

    assert ht.lookup('key') is None
    ht.insert('key', 23)
    assert ht.lookup('key') == 23

    del ht

    ht = Str2int(filename, 17, 'r')
    assert ht.lookup('key') == 23
    del ht

    unlink(filename)
