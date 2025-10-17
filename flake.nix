{
  nixConfig = {
    extra-substituters = [ "https://henrytill.cachix.org" ];
    extra-trusted-public-keys = [
      "henrytill.cachix.org-1:EOoUIk8e9627viyFmT6mfqghh/xtfnpzEtqT4jnyn1M="
    ];
  };

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
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        on = opam-nix.lib.${system};

        version = "0.1.0-${self.shortRev or self.dirtyShortRev}";

        applyOverrides = pkgs: isStatic: final: prev: {
          hbt-core = prev.hbt-core.overrideAttrs (as: {
            CPP_FLAGS = ''-DVERSION="${version}"'';
            nativeBuildInputs = as.nativeBuildInputs ++ [ pkgs.tzdata ];
          });
          hbt = prev.hbt.overrideAttrs (
            as:
            pkgs.lib.optionalAttrs isStatic {
              buildPhase = ''
                runHook preBuild
                dune build -p hbt --profile static -j $NIX_BUILD_CORES
                runHook postBuild
              '';
            }
          );
        };

        applyOxOverrides = pkgs: final: prev: {
          ocaml-variants = prev.ocaml-variants.overrideAttrs (as: {
            preBuild = ''
              sed -i "s|/usr/bin/env|${pkgs.coreutils}/bin/env|g" Makefile Makefile.* ocaml/Makefile.*
            '';
            nativeBuildInputs = as.nativeBuildInputs ++ [ pkgs.rsync ];
          });
        };

        devPackagesQuery = {
          ocaml-lsp-server = "*";
          ocamlformat = "*";
          odig = "*";
        };

        mkRegularScope =
          isStatic:
          let
            pkgs =
              let
                ps = nixpkgs.legacyPackages.${system};
              in
              if isStatic then ps.pkgsMusl else ps;
            query = devPackagesQuery // {
              ocaml-base-compiler = "5.3.0";
            };
            scope = on.buildOpamProject' {
              inherit pkgs;
              resolveArgs.with-test = true;
            } ./. query;
            overlay = applyOverrides pkgs isStatic;
          in
          scope.overrideScope overlay;

        mkOxScope =
          isStatic:
          let
            pkgs =
              let
                ps = nixpkgs.legacyPackages.${system};
              in
              if isStatic then ps.pkgsMusl else ps;
            scope = on.buildOpamProject' {
              inherit pkgs;
              repos = [
                "${opam-repository}"
                "${opam-repository-oxcaml}"
              ];
              resolveArgs.with-test = true;
            } ./. { ocaml-variants = "5.2.0+ox"; };
            overlay = pkgs.lib.composeExtensions (applyOverrides pkgs isStatic) (applyOxOverrides pkgs);
          in
          scope.overrideScope overlay;

        mkDevPackages =
          s:
          let
            lib = nixpkgs.legacyPackages.${system}.lib;
          in
          builtins.attrValues (lib.getAttrs (builtins.attrNames devPackagesQuery) s);

        mkShell =
          s:
          nixpkgs.legacyPackages.${system}.mkShell {
            inputsFrom = [
              s.hbt
              s.hbt-core
            ];
            packages = mkDevPackages s ++ [ ];
            shellHook = ''
              export ODIG_CACHE_DIR=$(mktemp -d)
            '';
          };

        legacyPackages = mkRegularScope false;
        legacyPackagesOx = mkOxScope false;
        legacyPackagesStatic = mkRegularScope true;
        legacyPackagesOxStatic = mkOxScope true;
      in
      {
        packages = rec {
          hbt = legacyPackages.hbt;
          hbt-ox = legacyPackagesOx.hbt;
          hbt-static = legacyPackagesStatic.hbt;
          hbt-ox-static = legacyPackagesOxStatic.hbt;
          default = hbt;
        };

        devShells.default = mkShell legacyPackages;
      }
    );
}
