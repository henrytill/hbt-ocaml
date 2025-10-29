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

        devPackagesQuery = {
          ocaml-lsp-server = "*";
          ocamlformat = "*";
          ocp-browser = "*";
          ocp-index = "*";
          down = "*";
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
              resolveArgs = {
                with-test = true;
              };
            } ./. query;
            overlay = applyOverrides pkgs isStatic;
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
          let
            pkgs = nixpkgs.legacyPackages.${system};
            ocpEnv = pkgs.buildEnv {
              name = "ocp-env";
              paths = s.hbt.buildInputs ++ s.hbt.propagatedBuildInputs;
            };
            ocp-browser-wrapped = pkgs.writeShellScriptBin "ocp-browser" ''
              exec ${s.ocp-browser}/bin/ocp-browser --no-opamlib -I "${ocpEnv}/lib/ocaml/${s.ocaml.version}/site-lib" "$@"
            '';
            ocp-index-wrapped = pkgs.writeShellScriptBin "ocp-index" ''
              if [ $# -eq 0 ]; then
                exec ${s.ocp-index}/bin/ocp-index
              else
                cmd="$1"
                shift
                exec ${s.ocp-index}/bin/ocp-index "$cmd" --no-opamlib -I "${ocpEnv}/lib/ocaml/${s.ocaml.version}/site-lib" "$@"
              fi
            '';
            devPackages = pkgs.lib.filter (p: p != s.ocp-browser && p != s.ocp-index) (mkDevPackages s);

            mkToplevelPath =
              ps:
              let
                toplevelDir = pkg: "${pkg}/lib/ocaml/${s.ocaml.version}/site-lib/toplevel";
              in
              pkgs.lib.concatStringsSep ":" (map toplevelDir ps);

            toplevelPath = mkToplevelPath [
              s.ocamlfind
              s.down
            ];
          in
          pkgs.mkShell {
            inputsFrom = [
              s.hbt
              s.hbt-core
            ];
            packages = devPackages ++ [
              ocp-browser-wrapped
              ocp-index-wrapped
            ];
            shellHook = ''
              export OCAML_TOPLEVEL_PATH="${toplevelPath}"
            '';
          };

        legacyPackages = mkRegularScope false;
        legacyPackagesStatic = mkRegularScope true;
      in
      {
        packages = rec {
          hbt = legacyPackages.hbt;
          hbt-static = legacyPackagesStatic.hbt;
          default = hbt;
        };

        devShells.default = mkShell legacyPackages;
      }
    );
}
