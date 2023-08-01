{
  nixConfig.bash-prompt = "[nix(cabal-audit)] ";
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/fe0dabfd8acf96f1b5cff55766de6284517868cf";
    # "path:///srv/github.com/podenv/hspkgs";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;

      # Enable hie output for a ghc package
      withHie = drv:
        let
          ndrv = pkgs.haskell.lib.appendBuildFlags drv
            [ "--ghc-options=-fwrite-ide-info" ];
        in ndrv.overrideAttrs (_: prevAttrs: {
          installPhase = ''
            ${prevAttrs.installPhase}

            # Put the hie file next to the .hi
            if [ -d "$out/lib/ghc-*/lib/*ghc*/$pkgId" ]; then
              hie="$(realpath $out/lib/ghc-*/lib/*ghc*/$pkgId/)"
              echo "Copying hie files to $hie..."
              pushd dist/build/
                for hiefile in $(${pkgs.findutils}/bin/find . -type f -name "*.hie"); do
                  mkdir -p "$hie/$(dirname $hiefile)";
                  cp "$hiefile" "$hie/$hiefile"
                done
              popd
            fi
          '';
        });

      # Call withHie for all package set attributes
      withAllHie = packagesSet:
        builtins.mapAttrs (name: value:
          if builtins.elem name [ "mkDerivation" "ghc" ]
          || !(builtins.isAttrs value) || !(builtins.hasAttr "pname" value) then
            value
          else
            withHie value) packagesSet;

      haskellExtendWithHie = hpFinal: hpPrev:
        withAllHie hpPrev // ({
          cabal-audit = hpPrev.callCabal2nix "cabal-audit" self { };
          ghc = hpPrev.ghc.overrideAttrs (prev: {
            patches = (hpPrev.ghc.patches or [ ])
              ++ [ ./0001-Enable-hie-file-generation.patch ];
          });
        });

      haskellExtendBase = hpFinal: hpPrev: {
        cabal-audit = hpPrev.callCabal2nix "cabal-audit" self { };
      };

      hsPkgs = pkgs.hspkgs.extend haskellExtendBase;

      baseTools = with pkgs; [
        cabal-install
        hsPkgs.cabal-fmt
        hlint
        fourmolu
        hsPkgs.doctest
      ];

    in {
      packages."x86_64-linux".default =
        pkgs.haskell.lib.justStaticExecutables hsPkgs.cabal-audit;
      packages."x86_64-linux".test = hsPkgs.ghc-byteorder;
      packages."x86_64-linux".ghc = hsPkgs.ghc;
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.cabal-audit ];
        buildInputs = with pkgs; [ ghcid haskell-language-server ] ++ baseTools;
      };
    };
}
