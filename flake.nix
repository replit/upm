{
  description = "Universal Package Manager";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; upm = pkgs.callPackage ./default.nix {}; in
        {
          packages = [ upm ];
          defaultPackage = upm;
        }
      );
}
