{
    description = "Haskell dev env";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    outputs = {self, nixpkgs }:
      let
        system = "x86_64-linux";
        pkgs = import nixpkgs {inherit system;};
      in{
        devShells.${system}.default = pkgs.mkShell{
            buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.ghc
            zlib
        ];

        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [pkgs.zlib];
      };
      packages.${system}.default = pkgs.haskellPackages.callCabal2nix "test" ./. {};
    };
}
