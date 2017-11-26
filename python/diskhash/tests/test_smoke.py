from diskhash import Str2int
from os import unlink, path

filename = 'testing.dht'

def test_insert_check():
    if path.exists(filename):
        unlink(filename)
    ht = Str2int(filename, 17, 'rw')

    assert ht.size() == 0
    assert ht.lookup('key') is None
    ht.insert('key', 23)
    assert ht.lookup('key') == 23
    assert ht.size() == 1

    del ht

    ht = Str2int(filename, 17, 'r')
    assert ht.size() == 1
    assert ht.lookup('key') == 23
    del ht

    unlink(filename)
