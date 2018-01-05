import sys


files = sys.argv[1:]
for f in files:
    with open(f) as fh:
        content = fh.read()
    with open(f, "w") as fh:
        fh.write(content.lower())
