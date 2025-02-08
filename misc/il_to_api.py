"""
Converts qbe IL in text .ssa format to sqbe api calls. Not especially robust,
mostly used to convert/generate test code.
"""

import sys


class Parser:
    def __init__(self, contents):
        self.contents = contents
        self.lex()
        self.parse()

    def lex(self):
        self.skip_whitespace()

    def parse():
        pass


def main():
    with open(sys.argv[1], "rb") as f:
        contents = f.read().decode("utf-8")
    p = Parser(contents)


if __name__ == "__main__":
    main()
