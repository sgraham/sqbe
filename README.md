<img alt="Dog with a Q collar tag" align="left" src="misc/sqbe.png">

**sqbe** ("scooby") is a hobby compiler backend, based on
[qbe](https://c9x.me/compile/).

If you're looking for a stable backend that accepts [text
input](https://c9x.me/compile/doc/il.html) and generates a `.s`, you
should choose [qbe](https://c9x.me/compile/).

In addition to qbe's features, sqbe adds the following features:
available on all targets yet):

- Windows support (sqbe's `amd64_win` target is being upstreamed to qbe, but
  sqbe also builds on Windows)
- A C API (rather than file-based textual input)
- [Amalgamated build](https://github.com/sgraham/sqbe/releases/tag/nightly) for
  simple project integration

Planned features:

- Support for generating `rdi` debug information supported by
  [raddbg](https://github.com/EpicGamesExt/raddebugger/).
- Direct generation of object files rather than .s to be assembled;
- Support for generating code directly into memory for JIT uses.
