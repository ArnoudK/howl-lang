# Howl Lang
Non existing language idea. It's a inspired by Go, Zig and a bit of Ocaml. 
## Idea's
 * Simple but explicit.
 * Memory managed.

It's not worked out yet, if you have idea's please share :).

Right now some (not fully fletched out concepts) are worked out as markdown in [lang-design/spec.md](lang-design/spec.md).

## Development

### Building
```bash
zig build
```

### Running
```bash
zig build run
```

### Testing
All tests can be run through the build system:
```bash
zig build test
```

Or use the provided test script:
```bash
./test.sh
```

**Note**: Individual test files use the `howl_lang_lib` module and must be run through the build system. Running `zig test` directly on individual test files will not work because they won't have access to the module imports defined in `build.zig`.
