{ inputs, ... }: {

  imports = [ inputs.git-hooks.flakeModule ];

  perSystem = { config, ... }: {
    devShells.pre-commit = config.pre-commit.devShell;

    pre-commit.settings = {
      hooks = {
        nixfmt-classic.enable = true;
        deadnix.enable = true;
        typos = {
          enable = true;
          settings.ignored-words = [
            # things like `GHC.TypeLits` shouldn't be a spelling error
            "Lits"
          ];
          excludes = [ "fourmolu.yaml" ];
        };

        fourmolu.enable = true;
        ormolu.settings.defaultExtensions =
          [ "ImportQualifiedPost" "TypeApplications" ];
      };
    };
  };
}
