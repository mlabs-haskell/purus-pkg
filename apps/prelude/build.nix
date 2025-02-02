{ config, ... }: {
  perSystem = { pkgs, ... }: {
    packages.prelude =
      (pkgs.extend config.flake.overlays.default).purusPackages.mkPurusPackage {
        name = "prelude";
        src = ./.;
      };
  };
}
