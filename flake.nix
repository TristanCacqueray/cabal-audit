{
  nixConfig.bash-prompt = "[nix(cabal-audit)] ";
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/39f3b89b82ce345f4b1837003a3cc020d041dbe9";
    # "path:///srv/github.com/podenv/hspkgs";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;

      # Enable simplified core
      withHiCore = drv:
        pkgs.haskell.lib.appendBuildFlags drv
        [ "--ghc-options=-fwrite-if-simplified-core" ];

      # Call withHix for all package set attributes
      withAllHiCore = packagesSet:
        builtins.mapAttrs (name: value:
          if builtins.elem name [ "mkDerivation" "ghc" ]
          || !(builtins.isAttrs value) || !(builtins.hasAttr "pname" value) then
            value
          else
            withHiCore value) packagesSet;

      pluginDrv = pkgs.haskell.lib.dontHaddock
        (pkgs.hspkgs.callCabal2nix "cabal-audit-plugin" ./cabal-audit-plugin
          { });
      pluginSoPath = "${pluginDrv}/lib/ghc-9.6.2/lib/libcabal-audit-plugin.so";
      pluginArgs = [
        "-fplugin-trustworthy"
        "-fplugin-library='${pluginSoPath};cabal-audit-plugin;CabalAudit.Plugin;[]'"
      ];

      # Enable hix output for a ghc package
      withHix = drv:
        let
          ndrv =
            pkgs.haskell.lib.appendBuildFlags (pkgs.haskell.lib.dontHaddock drv)
            (map (a: "--ghc-options=${a}") pluginArgs);
        in ndrv.overrideAttrs (_: prevAttrs: {
          installPhase = ''
            ${prevAttrs.installPhase}

            # Put the hix file next to the .hi
            pkgLibDir="$(find $out -type d -name $pkgId)"
            if [ -d $pkgLibDir ]; then
              echo "Copying hix files to $pkgLibDir..."
              pushd dist/build/
                for hixfile in $(find . -type f -name "*.hix"); do
                  mkdir -p "$pkgLibDir/$(dirname $hixfile)";
                  cp "$hixfile" "$pkgLibDir/$hixfile"
                done
              popd
            else
              echo "Skipping hix file for non library package"
            fi
          '';
        });

      # Call withHix for all package set attributes
      withAllHix = packagesSet:
        builtins.mapAttrs (name: value:
          if builtins.elem name [ "mkDerivation" "ghc" "cabal-audit-plugin" ]
          || !(builtins.isAttrs value) || !(builtins.hasAttr "pname" value) then
            value
          else
            withHix value) packagesSet;

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
            patches = (hpPrev.ghc.patches or [ ]) ++ [
              ./ghc-libraries-patches/0001-Enable-hie-file-generation.patch
            ];
          });
        });

      haskellExtendBase = hpFinal: hpPrev: {
        cabal-audit-command =
          hpPrev.callCabal2nix "cabal-audit-command" ./cabal-audit-command { };
        cabal-audit-plugin =
          hpPrev.callCabal2nix "cabal-audit-plugin" ./cabal-audit-plugin { };
        cabal-audit-hi =
          hpPrev.callCabal2nix "cabal-audit-hi" ./cabal-audit-hi { };
        cabal-audit-hie =
          hpPrev.callCabal2nix "cabal-audit-hie" ./cabal-audit-hie { };
        cabal-audit-test =
          hpPrev.callCabal2nix "cabal-audit-test" ./cabal-audit-test { };
      };

      hsPkgs = pkgs.hspkgs.extend haskellExtendBase;

      haskellExtendWithHix = hpFinal: hpPrev:
        withAllHix (hpPrev // (haskellExtendBase hpFinal hpPrev));
      hsPkgsTest = pkgs.hspkgs.extend haskellExtendWithHix;

      haskellExtendWithHiCore = hpFinal: hpPrev:
        withAllHiCore (hpPrev // (haskellExtendBase hpFinal hpPrev) // {
          ghc = hpPrev.ghc.overrideAttrs (prev: {
            buildPhase = ''
              runHook preBuild

              # hadrianFlagsArray is created in preConfigure
              newFlags=$(echo $hadrianFlags | sed 's/flavour=release/flavour=release+hi_core/')
              echo "hadrianFlags: $newFlags ''${hadrianFlagsArray[@]}"

              # We need to go via the bindist for installing
              hadrian $newFlags "''${hadrianFlagsArray[@]}" binary-dist-dir

              runHook postBuild
            '';
          });
        });
      hsPkgsHiCore = pkgs.hspkgs.extend haskellExtendWithHiCore;

      baseTools = with pkgs; [
        cabal-install
        hsPkgs.cabal-fmt
        hlint
        fourmolu
        hsPkgs.doctest
      ];

    in {
      pluginExtend = haskellExtendWithHix;
      hiCoreExtend = haskellExtendWithHiCore;
      packages."x86_64-linux".default =
        pkgs.haskell.lib.justStaticExecutables hsPkgs.cabal-audit-command;
      packages."x86_64-linux".test = hsPkgsTest.tagged;
      packages."x86_64-linux".plugin = pluginDrv;
      packages."x86_64-linux".ghc = hsPkgs.ghc;
      devShell."x86_64-linux" = hsPkgsHiCore.shellFor {
        packages = p: [
          p.cabal-audit-plugin
          p.cabal-audit-command
          p.cabal-audit-hi
          # p.cabal-audit-hie
          p.cabal-audit-test
        ];
        buildInputs = with pkgs; [ ghcid haskell-language-server ] ++ baseTools;
      };
      devShells."x86_64-linux".test = hsPkgsHiCore.shellFor {
        packages = p: [ p.cabal-audit-test ];
        buildInputs = with pkgs; [ ghcid haskell-language-server ] ++ baseTools;
      };
    };
}
