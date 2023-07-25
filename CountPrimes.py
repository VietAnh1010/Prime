from math import isqrt
from itertools import count

def countPrimes(n):
    if n < 2:
        return 0
    if n < 3:
        return 1
    sqrtn = isqrt(n)
    mxndx = (sqrtn - 1) // 2

    smalls = [i for i in range(mxndx + 1)]
    roughs = [2 * i + 1 for i in range(mxndx + 1)]
    larges = [(n // (2 * i + 1) - 1) // 2 for i in range(mxndx + 1)]
    skips = [False] * (mxndx + 1)
    npc = 0
    mxri = mxndx

    for i in count(1):
        sqri = 2 * i * (i + 1)
        if sqri > mxndx:
            break
        if skips[i]:
            continue
        skips[i] = True
        bp = 2 * i + 1
        for k in range(sqri, mxndx + 1, bp):
            skips[k] = True

        ri = 0
        for k in range(mxri + 1):
            q = roughs[k]
            qi = q // 2
            if skips[qi]:
                continue
            d = bp * q
            larges[ri] = larges[k] - (larges[smalls[d // 2] - npc] if d <= sqrtn else smalls[(n // d - 1) // 2]) + npc
            roughs[ri] = q
            ri += 1

        m = mxndx
        for k in range((sqrtn // bp - 1) | 1, bp - 1, -2):
            c = smalls[k // 2] - npc
            e = bp * k // 2
            while m >= e:
                smalls[m] -= c
                m -= 1
        mxri = ri - 1
        npc += 1
    
    result = larges[0]
    for i in range(1, mxri + 1):
        result -= larges[i]

    result += (mxri + 1 + 2 * (npc - 1)) * mxri // 2
    for i in range(1, mxri + 1):
        p = roughs[i]
        m = n // p
        e = smalls[(m // p - 1) // 2] - npc
        if e <= i:
            break
        for k in range(i + 1, e + 1):
            result += smalls[(m // roughs[k] - 1) // 2]
        result -= (e - i) * (npc + i - 1)
    result += 1
    return result


for i in range(10):
    n = 10 ** i
    print(countPrimes(n))

