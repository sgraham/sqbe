"""
Converts qbe IL in text .ssa format to sqbe api calls. Not especially robust,
mostly used to convert/generate test code.
"""

import sys


def parse(lines):
    get_linkages
    for line in lines:
        print(line)


def main():
    with open(sys.argv[1], "rb") as f:
        contents = f.read().decode("utf-8")
    parse(contents.splitlines())


if __name__ == "__main__":
    main()
