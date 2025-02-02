# Nix module for building the Haskell purus-pkg
{ ... }: {
  perSystem = { config, pkgs, ... }:
    let
      purus-pkg-project = pkgs.haskell-nix.project' {
        src = ./.;
        compiler-nix-name = "ghc966";

        # inputMap."https://chap.intersectmbo.org/" =
        #   "${inputs.cardano-haskell-packages}";

        shell = {
          tools = {
            cabal = { };
            hlint = { };
          };

          buildInputs = [ pkgs.purs pkgs.purus ];

          shellHook = config.pre-commit.installationScript;

        };
      };
      purus-pkg-project-flake = purus-pkg-project.flake { };
    in {
      inherit (purus-pkg-project-flake)
      # provides the `default` shell i.e., `nix develop` will put you in a shell suitable for using `cabal`, etc.
        devShells
        # provides all the packages from cabal
        packages
        # provides all the test executables from cabal
        checks;
    };
}
