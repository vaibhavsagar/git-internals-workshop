#!/usr/bin/env python3

import zlib
import hashlib
from sys import argv

from resolve_ref import resolve_ref


def sha1_to_path(sha1, directory):
    prefix = sha1[:2]
    suffix = sha1[2:]
    path = '/'.join([directory, 'objects', prefix, suffix])
    return path


def hash(content):
    return hashlib.sha1(content).hexdigest()


def decompress(path):
    with open(path, 'rb') as compressed:
        return zlib.decompress(compressed.read())

if __name__ == "__main__":
    ref = resolve_ref(argv[1], '.git')
    path = sha1_to_path(ref, '.git')
    content = decompress(path)
    print(content)
    assert ref == hash(content)
