import os
import os.path
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


def read_ref(ref_path):
        with open(ref_path) as f:
            return f.read()[:-1]


def resolve_ref(git_ref, directory):
    symref_path = '/'.join([directory, git_ref])
    refs_heads_path = '/'.join([directory, 'refs', 'heads', git_ref])
    refs_tags_path = '/'.join([directory, 'refs', 'tags', git_ref])
    if git_ref.startswith('ref'):  # content of symbolic ref
        return read_ref('/'.join([directory, git_ref]))
    elif os.path.isfile(symref_path):  # symbolic ref
        content = read_ref(symref_path)
        if content.startswith('ref: '):
            return resolve_ref(content[5:], directory)
        else:
            return content
    elif os.path.isfile(refs_heads_path):  # refs/heads/<ref>
        return read_ref(refs_heads_path)
    elif os.path.isfile(refs_tags_path):  # refs/tags/<ref>
        return read_ref(refs_tags_path)
    else:  # part or whole object reference
        return disambiguate_sha1(git_ref, directory)

if __name__ == "__main__":
    ref = argv[1]
    print(resolve_ref(ref, ".git"))
