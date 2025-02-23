{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.opam-repository.follows = "opam-repository";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      follows = "opam-nix/flake-utils";
    };
  };

  nixConfig = {
    extra-substituters = [ "https://henrytill.cachix.org" ];
    extra-trusted-public-keys = [
      "henrytill.cachix.org-1:EOoUIk8e9627viyFmT6mfqghh/xtfnpzEtqT4jnyn1M="
    ];
  };

  outputs =
    {
      self,
      flake-utils,
      opam-nix,
      nixpkgs,
      ...
    }@inputs:
    let
      package = "hbt";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        scope = on.buildOpamProject' { resolveArgs.with-test = true; } ./. {
          ocaml-base-compiler = "5.2.0";
        };
        overlay = final: prev: {
          hbt-core = prev.hbt-core.overrideAttrs (as: {
            nativeBuildInputs = as.nativeBuildInputs ++ [ pkgs.tzdata ];
          });
        };
      in
      {
        legacyPackages = scope.overrideScope overlay;
        packages.default = self.legacyPackages.${system}.${package};
      }
    );
}
