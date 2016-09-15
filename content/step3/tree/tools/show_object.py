#!/usr/bin/env python3

import zlib
import hashlib
from sys import argv
from re import compile

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


def sha1_to_content(sha1, directory='.git'):
    decompressed = decompress(sha1_to_path(sha1, directory))
    assert sha1 == hash(decompressed)
    header_end = decompressed.find(b'\x00')
    header = decompressed[:header_end].decode()
    object_type = header.split(' ')[0]
    content = decompressed[header_end+1:]
    if object_type == 'tree':
        tree_entry = compile(br'(\d+) (.*?)\x00([\x00-\xff]{20})')
        entries = [
            '\t'.join((mode.decode().zfill(6), sha1.hex(), name.decode()))
            for (mode, name, sha1) in
            tree_entry.findall(content)
        ]
        return [header, *entries]
    elif object_type in ('commit', 'blob'):
        return [header, content.decode()]

if __name__ == "__main__":
    ref = resolve_ref(argv[1], '.git')
    content = sha1_to_content(ref, '.git')[1:]
    [print(item) for item in content]
