# Roota

## Build

### Server

TODO still need to get this to build with nix

### Client (Miso Frontent)

Make sure you have nix installed (see miso's readme for help here).
For a fast nix+cabal build:

    $ nix-shell -A client.env
    nix-shell$ cd roota-client
    nix-shell$ cabal new-build
    $ firefox roota-client/dist-newstyle/build/x86_64-linux/ghcjs-0.2.0/roota-client-0.1.0.0/x/client/build/client/client.jsexe/index.html

Or for a slow full nix build:

    $ nix-build
    $ firefox result/bin/client.jsexe/index.html

## Deploy

