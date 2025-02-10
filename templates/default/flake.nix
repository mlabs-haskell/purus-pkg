{
  description = "Example flake to develop with Purus with purus-pkg";

  inputs = {
    nixpkgs.follows = "purus-pkg/nixpkgs";
    purus-pkg.url =
      "git+ssh://git@github.com/mlabs-haskell/purus-pkg?ref=jaredponn/initial-implementation";
  };

  outputs = { nixpkgs, purus-pkg, ... }:
    let
      systems = [ "x86_64-linux" ];
      eachSystem = nixpkgs.lib.genAttrs systems;
    in {
      devShells = eachSystem (system:
        let
          pkgs = import nixpkgs {
            overlays = [
              # Overlay to provide the dependencies necessary for purus
              purus-pkg.overlays.default
            ];
            inherit system;
          };
        in {
          default = pkgs.mkShell {
            packages = [
              pkgs.purs
              pkgs.purus
              (pkgs.purusPackages.purusPkgWithPackages [
                # Put purus packages here, and the nix machinery will
                # create a registry s.t. purus-pkg can find these packages.
                purus-pkg.packages.${system}.prelude
              ])
            ];
          };
        });
    };
}
