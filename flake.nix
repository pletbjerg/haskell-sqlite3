{
  description = "A flake for Haskell sqlite3 bindings";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    sqlite3.url = "github:pletbjerg/sqlite3";
  };

  outputs = { self, nixpkgs, sqlite3 }: 
  let system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      version = "0.1.0.0";

      # Create the project
      project = pkgs.haskellPackages.developPackage
        {
            root = ./.;
            name = "haskell-sqlite3";
            modifier = pkgs.haskell.lib.compose.overrideCabal (drv:
                {
                    # Some awkwardness from abnormal default behavior from
                    # `cabal2nix` automatically hard coding the conversion from
                    # `sqlite3` to `sqlite`
                    # See
                    # [here](https://github.com/NixOS/cabal2nix/blob/021a48f4b4942462154b06fd81429a248638f87f/cabal2nix/src/Distribution/Nixpkgs/Haskell/FromCabal/Name.hs)
                    librarySystemDepends = [ sqlite3.packages.${system}.default ];
                    libraryPkgconfigDepends = [ sqlite3.packages.${system}.default ];
                });

        };
  in {
    packages.${system} = { 
        default = project; 
    };

    devShells.${system} = { 
        default = project.envFunc { withHoogle = true; };
    };

    # For `nix fmt` to use `nixpkgs-fmt` from
    # [here](https://github.com/nix-community/nixpkgs-fmt)
    formatter.${system} = pkgs.nixpkgs-fmt;
  };
}
