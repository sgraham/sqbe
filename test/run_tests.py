import os

ROOT_DIR = os.path.normpath(
    os.path.join(os.path.abspath(os.path.dirname(__file__)), "..")
)

import glob
import subprocess
import sys

TEST_CC = ["cl", "/nologo"]
GEN_CC = ["clang"]


def test_cc(in_file, out_bin):
    proc = subprocess.run(
        TEST_CC
        + [
            "/D_CRT_SECURE_NO_DEPRECATE",
            "/I../src",
            "../src/sqbe.c",
            in_file,
            "/link",
            "/out:%s" % out_bin,
        ],
        capture_output=True,
    )
    if proc.returncode != 0:
        print("test_cc failed")
        sys.stdout.write(proc.stdout.decode("utf-8"))
        sys.stderr.write(proc.stderr.decode("utf-8"))
        sys.exit(1)


def gen_cc(in_file, out_bin):
    subprocess.run(GEN_CC + [in_file, "-o", out_bin], check=True)


def get_expected_output(filename):
    output = []
    with open(filename, "r") as f:
        for line in f.readlines():
            out_prefix = "// OUT: "
            if line.startswith(out_prefix):
                output.append(line[len(out_prefix) :].rstrip())
    return "\n".join(output) + "\n"


# https://gist.github.com/NeatMonster/c06c61ba4114a2b31418a364341c26c0
class hexdump:
    def __init__(self, buf, off=0):
        self.buf = buf
        self.off = off

    def __iter__(self):
        last_bs, last_line = None, None
        for i in range(0, len(self.buf), 16):
            bs = bytearray(self.buf[i : i + 16])
            line = "{:08x}  {:23}  {:23}  |{:16}|".format(
                self.off + i,
                " ".join(("{:02x}".format(x) for x in bs[:8])),
                " ".join(("{:02x}".format(x) for x in bs[8:])),
                "".join((chr(x) if 32 <= x < 127 else "." for x in bs)),
            )
            if bs == last_bs:
                line = "*"
            if bs != last_bs or line != last_line:
                yield line
            last_bs, last_line = bs, line
        yield "{:08x}".format(self.off + len(self.buf))

    def __str__(self):
        return "\n".join(self)

    def __repr__(self):
        return "\n".join(self)


def do_test(f):
    print("%s... " % f, end="")
    sys.stdout.flush()
    expected = get_expected_output(f)
    test_cc(f, "tmp.exe")
    subprocess.run(["tmp.exe", "tmp.s"], check=True)
    gen_cc("tmp.s", "gen.exe")
    proc = subprocess.run(
        ["gen.exe"], capture_output=True, check=True, universal_newlines=True
    )
    got = proc.stdout
    if got != expected:
        print("FAILED")
        print("GOT:")
        print(got)
        print(hexdump(got.encode("utf-8")))
        print("EXPECTED:")
        print(expected)
        print(hexdump(expected.encode("utf-8")))
    else:
        print("ok")


def main():
    os.chdir(os.path.join(ROOT_DIR, "test"))
    for f in glob.glob("*.c"):
        do_test(f)


if __name__ == "__main__":
    main()
