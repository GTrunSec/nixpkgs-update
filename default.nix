{ pkgs ? import (import ./nix/sources.nix).nixpkgs {config = { allowBroken = true; };}, 
returnShellEnv ? pkgs.lib.inNixShell
}:

let

  compiler = pkgs.haskell.packages."ghc865";

  inherit (pkgs.haskell.lib) dontCheck doJailbreak overrideCabal;

  pkg = compiler.developPackage {
    root = ./.;
    overrides = self: super: {
       partial-order = doJailbreak super.partial-order;
    };
    source-overrides = { };
    inherit returnShellEnv;
  };

in pkg.overrideAttrs (attrs: {
  propagatedBuildInputs = with pkgs; [
    nix
    git
    getent
    gitAndTools.hub
    jq
    tree
    gist
  ];
})
