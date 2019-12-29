with (import (builtins.fetchTarball {
                url = "https://github.com/dmjio/miso/archive/f11b6a9eb8b69d71ac777975b13d3632d931f61e.tar.gz";
                sha256 = "1wl9vpqxshzrlanm9rpvgkiay3xa1abvkyknnp5z41kgfw63ypdl";
              }) {});

pkgs.haskell.packages.ghc865.callCabal2nix "roota" ./. {
  miso = miso-jsaddle;
}