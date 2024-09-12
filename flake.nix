{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.opam-repository.follows = "opam-repository";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      follows = "opam-nix/flake-utils";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
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
        scope = on.buildOpamProject { resolveArgs.with-test = true; } package ./. {
          ocaml-base-compiler = "5.2.0";
        };
        overlay = final: prev: {
          ${package} = prev.${package}.overrideAttrs (as: {
            nativeBuildInputs = as.nativeBuildInputs ++ [ pkgs.tzdata ];
            # The output of Unix.mktime is dependent on the time
            # zone of the user's environment, and expect test outputs
            # were generated in this time zone.
            preBuild = "export TZ=America/Los_Angeles";
          });
        };
      in
      {
        legacyPackages = scope.overrideScope' overlay;
        packages.default = self.legacyPackages.${system}.${package};
      }
    );
}
