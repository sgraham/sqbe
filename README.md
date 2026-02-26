<img alt="Dog with a Q collar tag" align="left" src="misc/sqbe.png">

**sqbe** ("scooby") is a hobby compiler backend, based on
[qbe](https://c9x.me/compile/).

If you're looking for a stable backend that accepts [text
input](https://c9x.me/compile/doc/il.html) and generates a `.s`, you
should choose [qbe](https://c9x.me/compile/).

sqbe differs from qbe in the following ways:

- Windows support (sqbe's `amd64_win` target was upstreamed to qbe `master`, but
  sqbe also builds on Windows)
- A C API (rather than file-based textual input)
- [Amalgamated build](https://github.com/sgraham/sqbe/releases/tag/nightly) for
  easy integration
- Context-ized implementation to be suitable for library use
- On macOS arm64 supports:
  - direct generation of Mach-O object files (rather than a `.s` to be
    assembled).
  - direct execution of assembled code for JIT uses, rather than emitting a
    file at all.

Planned features:

- Support for generating `rdi` debug information supported by
  [raddbg](https://github.com/EpicGamesExt/raddebugger/).
- Direct generation of object files on more platforms.
- Support for generating code directly into memory for JIT uses on more
  platforms.
