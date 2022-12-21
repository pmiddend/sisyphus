with (import
  (builtins.fetchTarball {
    url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.tar.gz";
  })
{ });
let
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. { miso = miso-jsaddle; };
  client = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. { };
in
{
  inherit dev;
  release = client;
  isomorphic = pkgs.runCommand "sisyphus.org" { inherit client; } ''
    mkdir -p $out
    ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=${client}/bin/app.jsexe/all.js.externs \
      ${client}/bin/app.jsexe/all.js > temp.js
    mv temp.js $out/all.js
  '';
  inherit pkgs;
}


