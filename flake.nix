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
                { librarySystemDepends = 
                    [ sqlite3.packages.${system}.default ]
                    ++ (drv.librarySystemDepends or []); 
                });

        };
  in {
    packages.${system} = { 
        default = project; 
    };

    devShells.${system} = { 
        default = project.envFunc { withHoogle = true; };
    };
  };
}
