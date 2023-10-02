{
  nixpkgs,
  rev,
  self,
  system,
}: let
  pkgs = nixpkgs.legacyPackages.${system};
in rec {
  default = upm;
  devShell = pkgs.callPackage ./devshell {};
  fmt = pkgs.callPackage ./fmt {};
  upm = pkgs.callPackage ./upm {inherit rev;};
}
