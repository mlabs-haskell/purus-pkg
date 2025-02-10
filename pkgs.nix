# Nix module for overlaying our `pkgs`
{ inputs, ... }: {
  perSystem = { pkgs, system, ... }: {

    _module.args = {
      pkgs = import inputs.nixpkgs {
        inherit system;

        # Overlay our packages with haskell-nix.
        inherit (inputs.haskell-nix) config;

        overlays = [
          inputs.haskell-nix.overlay

          (_self: _super: {
            inherit (inputs.purus.packages.${system}) purus purs;
          })

          # inputs.iohk-nix.overlays.crypto
          # inputs.iohk-nix.overlays.haskell-nix-crypto
        ];
      };
    };
  };
}
