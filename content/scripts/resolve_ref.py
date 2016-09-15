import os
from sys import argv


def sha1_to_directory(sha1, directory):
    prefix = sha1[:2]
    path = '/'.join([directory, 'objects', prefix])
    return path


def disambiguate_sha1(sha1, directory):
    prefix = sha1[:2]
    suffix = sha1[2:]
    matching_files = [
        file for file in os.listdir(sha1_to_directory(sha1, directory))
        if file.startswith(suffix)]
    if len(matching_files) == 0:
        raise Exception('No object exists with that SHA1.')
    elif len(matching_files) > 1:
        raise Exception('Ambiguous SHA1 provided.')
    else:
        return prefix + matching_files[0]


def resolve_ref(git_ref, directory):
    if git_ref == 'HEAD':
        with open('/'.join([directory, git_ref])) as f:
            pointed_ref = f.read()[5:-1]  # Remove trailing newline
            return resolve_ref(pointed_ref, directory)
    else:
        with open('/'.join([directory, git_ref])) as f:
            return f.read()[:-1]

if __name__ == "__main__":
    ref = argv[1]
    print(resolve_ref(ref, ".git"))
