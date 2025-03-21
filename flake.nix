{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };

  outputs = { self, flake-utils, opam-nix, nixpkgs }:
    let package = "lc3";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        on = opam-nix.lib.${system};

        devPackagesQuery = {
          ocaml-lsp-server = "*";
          ocamlformat = "*";
        };
        query = devPackagesQuery // {
          ocaml-base-compiler = "*";
        };

        scope = on.buildOpamProject' { } ./. query;
        overlay = final: prev: {
          ${package} = prev.${package}.overrideAttrs (_: {
            doNixSupport = false;
          });
        };

        scope' = scope.overrideScope overlay;
        main = scope'.${package};
        devPackages = builtins.attrValues
          (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
      in
      with pkgs; {
        legacyPackages = scope';
        packages.default = main;

        devShells.default = mkShell {
          inputsFrom = [ main ];
          buildInputs = devPackages;
        };

        formatter = nixpkgs-fmt;
      });
}
