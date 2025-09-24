{
  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    opam-repository-oxcaml = {
      url = "github:oxcaml/opam-repository";
      flake = false;
    };
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.opam-repository.follows = "opam-repository";
    };
    flake-utils = {
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
      opam-repository,
      opam-repository-oxcaml,
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
          ocaml-base-compiler = "5.3.0";
        };
        overlay = final: prev: {
          hbt-core = prev.hbt-core.overrideAttrs (as: {
            nativeBuildInputs = as.nativeBuildInputs ++ [ pkgs.tzdata ];
          });
        };
        scopeOx =
          on.buildOpamProject'
            {
              repos = [
                "${opam-repository}"
                "${opam-repository-oxcaml}"
              ];
              resolveArgs.with-test = true;
            }
            ./.
            {
              ocaml-variants = "5.2.0+ox";
            };
        overlayOx = final: prev: {
          hbt-core = prev.hbt-core.overrideAttrs (as: {
            nativeBuildInputs = as.nativeBuildInputs ++ [ pkgs.tzdata ];
          });
          ocaml-variants = prev.ocaml-variants.overrideAttrs (as: {
            preBuild = ''
              sed -i "s|/usr/bin/env|${pkgs.coreutils}/bin/env|g" Makefile Makefile.* ocaml/Makefile.*
            '';
            nativeBuildInputs = as.nativeBuildInputs ++ [ pkgs.rsync ];
          });
        };
      in
      {
        legacyPackages = scope.overrideScope overlay;
        legacyPackagesOx = scopeOx.overrideScope overlayOx;
        packages = rec {
          hbt = self.legacyPackages.${system}.${package};
          hbt-ox = self.legacyPackagesOx.${system}.${package};
          default = hbt;
        };
      }
    );
}
