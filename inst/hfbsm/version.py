#!/usr/bin/env python

import sys, platform

def main():
    print (platform.python_implementation())
    print (platform.python_version())
    print (platform.python_build())
    print (platform.python_compiler())

if __name__ == '__main__':
    main()
    sys.exit(0)
