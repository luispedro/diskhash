from random import random
print("#include <inttypes.h>")
print("uint64_t rtable [] = {")
for _ in range(256):
    val = 0
    for _ in range(64):
        val *= 2
        val += (1 if random() < .5 else 0)
    print("{}LLU,".format(val))
print("0 /* sentinel */")
print("};")

