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

      haskellExtend = hpFinal: hpPrev: {
        cabal-audit = hpPrev.callCabal2nix "cabal-audit" self { };
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

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
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.cabal-audit ];
        buildInputs = with pkgs; [ ghcid haskell-language-server ] ++ baseTools;
      };
    };
}
