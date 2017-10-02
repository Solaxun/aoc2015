safe, trap = '.', '^'
initial = ".^^.^^^..^.^..^.^^.^^^^.^^.^^...^..^...^^^..^^...^..^^^^^^..^.^^^..^.^^^^.^^^.^...^^^.^^.^^^.^.^^.^."
cat = ''.join

def rows(n, row=initial):
    "The first n rows of tiles (given the initial row)."
    result = [row]
    for i in range(n-1):
        previous = safe + result[-1] + safe
        result.append(cat((trap if previous[i-1] != previous[i+1] else safe)
                          for i in range(1, len(previous) - 1)))
    return result

print([initial][-1])
print(cat(rows(40)).count(safe))


def foo():
    yield from range(20)

print(list(foo()))
print(list(range(10,3)))

from functools import reduce

res = reduce(lambda acc,cur: acc + cur ,
             [[1, 2, [3]], [], [1] ,[]],
             [])
print(res)

def mapcat(f,coll): return reduce(lambda acc,c: acc+c,map(f,coll),[])

def flatten(coll):
    if type(coll) == list: return mapcat(flatten,coll)
    return [coll]

print(flatten([1,[2,[3,4]],5]))


print(mapcat(lambda x: [x**2],range(1,4)))

def merge_dicts(res={},*dicts):
    if not dicts: return res
    res.update(dicts[0])
    return merge_dicts(res,*dicts[1:])

print(merge_dicts({},dict(a=1,b=2),
                  dict(c=3,d=4),
                  dict(e=5)))

x = [1,1,1,1]
x[1::2] = [0] * (len(x) // 2)
print(x)
print([tuple([x,y]) if x >3 else tuple(['nil','foo'])
       for x in range(10)
       for y in ['a','b','c']])

print(list(x ** 2 for x in range(1,11)))
import numpy as np
print(np.zeros((3,4)))


for x in range(10):
    if x % 2 == 0:
        print("yep... it's even! {}".format(x))
import itertools
combinations("abc",3)
permutations("abc",3)
print(list(permutations("abc",3)))
import requests
import collections
import requests
import numpy as np
