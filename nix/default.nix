{
  nixpkgs,
  nix-editor,
  rev,
  self,
  system,
  buildGoCache
}: let
  pkgs = nixpkgs.legacyPackages.${system};
  nix-editor-pkg = nix-editor.packages.${system}.nix-editor;
in rec {
  default = upm;
  devShell = pkgs.callPackage ./devshell {nix-editor = nix-editor-pkg;};
  fmt = pkgs.callPackage ./fmt {};
  upm = pkgs.callPackage ./upm {inherit rev; inherit buildGoCache;};
}
