# ghc-progress

Progress bar for GHC compilation

## Install

```ShellSession
$ nix profile install github:evanrelf/ghc-progress
```

## Usage

Pipe `stdout` and `stderr` from your Haskell build to `ghc-progress`:

```ShellSession
$ cabal build |& ghc-progress
Resolving dependencies...
Build profile: -w ghc-9.2.8 -O1
In order, the following will be built (use -v for more details):
 - ShellCheck-0.9.0 (lib) (first run)
 - ShellCheck-0.9.0 (exe:shellcheck) (first run)
Configuring library for ShellCheck-0.9.0..
Preprocessing library for ShellCheck-0.9.0..
Building library for ShellCheck-0.9.0..
[ 4 of 28] Compiling ShellCheck.AST
```

If you just want the progress bar without any other logging, use the `--quiet`
flag:

```ShellSession
$ cabal build |& ghc-progress --quiet
[ 4 of 28] Compiling ShellCheck.AST
```
