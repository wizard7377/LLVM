# LLVM for Idris

This is an [LLVM](https://llvm.org/) library for [Idris](https://github.com/idris-lang/Idris2).

Inspired both by the LLVM C API, LLVM IR, and the Haskell [LLVM](https://hackage.haskell.org/package/llvm-hs) library, particularly the [pure portion](https://hackage.haskell.org/package/llvm-hs-pure).
Designed to be safe and easy to use, it seperates the creation of LLVM IR from the compilation of it.

In addition, it provides mutiple ways both to create LLVM IR and to use it.

## Usage 

There are three "parts" of using this library:

- Building the IR
- Encoding the IR
- Using the IR

Building the IR can be done in one of two ways.
Firstly, it can be done with a combination of raw writing of fields and syntax sugar.
Note that "raw" in this case does not mean "unsafe".

Rather, merely that it lacks a nice API to manage naming, blocks, etc.
The second way, much more in-line with both the C API and Haskell package, is to use a `Builder` monad.
This is easier, and more idiomatic.

There are also two ways to encode and then use the IR.
The first of these, the one more in line with current Idris solutions, just outputs the IR to text.
This however, will soon be discouraged in favour of the "foriegn" way of doing it, which directly builds the representation using the C API.

After it is translated into bitcode or a C value, we can then perform anaylisis on it, run it, etc.

## Structure

It consists of three sepererate libraries, `llvm-idr`, `llvm-ffi`, and `llvm-test`.
`llvm-ffi` consists solely of the raw C interface to LLVM, and the utilities used to define to it.
Essientally just a long list of `%foriegn` decleartions.

`llvm-idr` is the core package, responsiple for:

- Defining the structure of the IR in Idris
- Using the functions in `llvm-ffi`
- Defining how to run, compile, build the LLVM IR

`llvm-test` is self explanatory.

## Building 

The Idris LLVM library is primarly built using `make` and the basic `idris2` build tool, it also has limited support for `pack`.

### Prerequisites 

The following things are *always* required to build LLVM

- `idris2`
- `make`, as well as `binutils`, in particular `gas` and `ld`
- `llvm` itself
- A C compiler

Also, because of how the simple text interface is designed, `llvm` is required to *run* this with that interface.

#### Optional 

- Ripgrep (`rg`): used to create a listing of holes in the project
- `rlwrap`: REPL interaction
- `python`: `getSrc` utility (which lists all the source files for easy modification of `.ipkg` files)

## Status

Currently, this is a solo project, that is still very much unfinished. 
Eventually, I do hope to get this to a state where it can be used for the creation of a backend for Idris, as well as potentially another language implemented in Idris.

## License

This work is licensed under [BSD-2](./LICENSE)