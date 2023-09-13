{
  description = "Universal Package Manager";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          rev = if self ? rev then "0.0.0-${builtins.substring 0 7 self.rev}" else "0.0.0-dirty";

          upm = pkgs.callPackage ./upm.nix {
            inherit rev;
          };
        in
        {
          devShells.default = pkgs.mkShell {
            packages = [
              pkgs.nodejs
              pkgs.nodePackages.yarn
            ];

            inputsFrom = [
              upm
            ];
          };

          packages = {
            default = upm;
            inherit upm;
          };
        }
      );
}
