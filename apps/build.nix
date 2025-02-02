# `./apps` contains the applications which are orchestrated together
{ config, inputs, ... }: {

  imports = [ ./prelude/build.nix ];

  perSystem = { config, pkgs, ... }: {
    packages = {
      purus-pkg = config.packages."purus-pkg:exe:purus-pkg".overrideAttrs
        (_self: super: {
          nativeBuildInputs = super.nativeBuildInputs or [ ]
            ++ [ pkgs.makeWrapper ];

          postInstall = ''
            wrapProgram $out/bin/purus-pkg \
                --prefix PATH : ${pkgs.lib.getBin pkgs.purus}/bin \
                --prefix PATH : ${pkgs.lib.getBin pkgs.purs}/bin
          '';

        });

      purus-pkg-local-registry =
        config.packages."purus-pkg-local-registry:exe:purus-pkg-local-registry";
    };
  };

  flake.overlays.default = (self: _super: {
    purs = inputs.purus.packages.${self.system}.purs;
    purus = inputs.purus.packages.${self.system}.purus;
    purus-pkg = config.flake.packages.${self.system}."purus-pkg";
    purus-pkg-local-registry =
      config.flake.packages.${self.system}."purus-pkg-local-registry";

    purusPackages = self.lib.makeExtensible (final: {
      purus-pkg = final.purusPkgWithPackages [ ];

      /* *
         Creates an instance of purus-pkg with access to the provided
         packages
      */
      purusPkgWithPackages = listOfPackages:
        self.writeShellApplication {
          name = "purus-pkg";
          runtimeInputs = [ self.purus-pkg ];
          text = ''
            case "''${1:-}" in
                install)
                    shift
                    purus-pkg install \
                        --local-registry ${
                          final.mkLocalRegistry listOfPackages
                        }/local-registry.json \
                        "$@"
                ;;
                *)
                    purus-pkg "$@"
                ;;
            esac
          '';
        };

      /* *
         Creates a Purus package

         # Type
         ```
         mkPurusPackage :: AttrSet ->  Derivation
         ```
      */
      mkPurusPackage = args@{ ... }:
        self.stdenv.mkDerivation (args // {
          nativeBuildInputs = [ self.purus-pkg self.purus self.purs ]
            ++ args.nativeBuildInputs or [ ];
          buildPhase = ''
            purus-pkg check
          '';
          installPhase = ''
            cp -r . "$out"
          '';
        });

      /* *
         Creates a Purus package in the file `$out/local-registry.json`

         # Type
         ```
         mkLocalRegistry :: [Derivation] ->  Derivation
         ```
      */
      mkLocalRegistry = listOfPurusPackages:
        self.stdenv.mkDerivation {
          name = "purus-local-registry";
          nativeBuildInputs = [ self.purus-pkg-local-registry ];
          dontUnpack = true;
          installPhase = ''
            mkdir -p "$out"
            purus-pkg-local-registry build \
                ${
                  if listOfPurusPackages == [ ] then
                    ""
                  else
                    self.lib.escapeShellArgs ([ "--purus-package-path" ]
                      ++ self.lib.intersperse "--purus-package-path"
                      listOfPurusPackages)
                } \
                > "$out/local-registry.json"
          '';
        };
    });
  });
}
