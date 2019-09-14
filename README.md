# HMM (Haskell Micro Machines)

Remake of the [Micro Machines game](https://en.wikipedia.org/wiki/Micro_Machines_(video_game_series)) in Haskell.

## How to run

With stack:

```sh
$ stack build
$ stack exec hmm
```

With cabal:

```sh
$ cabal new-build
$ cabal new-exec hmm
```

There is a `shell.nix` for NixOS users, so just:

```sh
$ nix-shell
```

or if you’re using the `direnv` just:

```sh
$ direnv allow
```

Nix-derivations for haskell dependencies are kept in the `./nix` directory.
Also there is a `Makefile` for your convenience.

## Usage

There are a few CLI options to play with.

* `-w` – Window width
* `-h` – Window height
* `-l` – Level to start

For example:

```sh
cabal new-exec -- -w 800 -h 600 -l level1
```

## Contributing

TODO
