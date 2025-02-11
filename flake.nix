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
    purus.url = "github:mlabs-haskell/purus";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        # Executables, testsuites, etc.
        ./build.nix
        ./apps/build.nix
        ./testsuites/build.nix

        # Code quality
        ./pre-commit.nix

        # Modifying the `pkgs`
        ./pkgs.nix
      ];
      systems = [ "x86_64-linux" ];
    };
}
