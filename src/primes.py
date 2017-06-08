# This code is terribly inefficient, but only runs once.
def prime(n):
    '''Check if a number is prime'''
    if n % 2 == 0 or n % 3 == 0: return False
    if n == 1: return False
    if n <= 3: return True

    i = 5
    while i*i <= n:
        if n % i == 0 or n % (i+2) == 0:
            return False
        i += 6
    return True

def next_prime(p):
    if p % 2 == 0:
        p += 1
    while not prime(p):
        p += 2
    return p
p = 7
print("#include <inttypes.h>")
print("uint64_t primes [] = {")
while p < 2**46: # 64 Teraelements should be enough for everybody
    print("{}, ".format(p))
    p *= 1.7
    p = next_prime(int(p))
print("0 /* sentinel */")
print("};")

