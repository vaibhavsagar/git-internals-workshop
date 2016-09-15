from sys import argv


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
