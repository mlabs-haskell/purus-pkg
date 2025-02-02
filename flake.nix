{
  description = "purus-pkg";

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs";

    # Flakes as modules (allows us to organize the repo into modules)
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Code quality automation (primarily used for pre-commit hooks)
    git-hooks.url = "github:cachix/git-hooks.nix";

    # Haskell.nix
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    # purus
    purus.url =
      "github:mlabs-haskell/purus?ref=jaredponn/create-purus-flake-output";

    # # Crypto overlays necessary for Plutus
    # iohk-nix.url = "github:input-output-hk/iohk-nix";
    # iohk-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs";

    # # Cardano Haskell packages
    # cardano-haskell-packages.url =
    #   "github:IntersectMBO/cardano-haskell-packages?ref=repo";
    # cardano-haskell-packages.flake = false;
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./purus-pkg.nix

        ./pre-commit.nix
        ./pkgs.nix
      ];
      systems = [ "x86_64-linux" ];
    };
}
