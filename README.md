# Howl Lang

Non existing language idea. It's a inspired by Go, Zig and a bit of Ocaml.

Contact: [https://discord.gg/2ZdQ3cvVjF](https://discord.gg/2ZdQ3cvVjF)

[!CAUTION]
This is a work in progress vibe-coded language that's unfinished.

## Idea's

- Simple but explicit.
- Memory managed.

It's not worked out yet, if you have idea's please share :).

Right now some (not fully fletched out concepts) are worked out as markdown in [lang-design/spec.md](lang-design/spec.md).

## Installation / Set up

Currently a bit of a pain.

- Set up Fil-C
  It's inside the repo as a submodule. You need to initialize and update the submodule before building:
  Recommended to use `depth` option to avoid fetching unnecessary history:

```bash
git submodule init
git submodule update --depth 1
```

Install the requirements for Fil-C:

- CMake
- Build Essentials
- Ninja
- Ruby
- Ruby getoptlong
- patchelf

Pacman:

````
```bash
sudo pacman -Syu --needed cmake base-devel ninja ruby ruby-getoptlong patchelf
````

Go to the Fil-C directory:

```bash
cd fil-c
```

Use the `build all` script:

```bash
./build_all.sh
```

## Development

### Building

```bash
zig build
```

### Running

```bash
zig build run
```

### LSP Support

The project includes a Language Server Protocol (LSP) implementation for editor integration:

```bash
# Build and run the LSP server
zig build lsp
```

For detailed LSP setup instructions, see [LSP_README.md](LSP_README.md).

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
