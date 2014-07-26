#!/usr/bin/env python

from random import random

def mkfloats(n=10):
    for i in range(n):
        print('%0.3f' % random())


if __name__ == '__main__':
    import sys
    
    if len(sys.argv) > 1:
        n = int(sys.argv[1])
        mkfloats(n)
    else:
        mkfloats()
