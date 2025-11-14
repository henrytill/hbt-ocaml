{
  inputs = {
    self.submodules = true;
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
          hbt-cli = prev.hbt-cli.overrideAttrs (
            as:
            pkgs.lib.optionalAttrs isStatic {
              buildPhase = ''
                runHook preBuild
                dune build -p hbt-cli --profile static -j $NIX_BUILD_CORES
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
            } self query;
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
              paths = s.hbt-cli.buildInputs ++ s.hbt-cli.propagatedBuildInputs;
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
              s.hbt-cli
            ];
            packages = devPackages ++ [
              ocp-browser-wrapped
              ocp-index-wrapped
              pkgs.yaml-language-server
            ];
            EMACSLOADPATH = "${s.dune}/share/emacs/site-lisp:";
            OCAML_TOPLEVEL_PATH = "${toplevelPath}";
          };

        legacyPackages = mkRegularScope false;
        legacyPackagesStatic = mkRegularScope true;
      in
      {
        packages = rec {
          hbt-cli = legacyPackages.hbt-cli;
          hbt-cli-static = legacyPackagesStatic.hbt-cli;
          default = hbt-cli;
        };

        devShells.default = mkShell legacyPackages;
      }
    );
}
