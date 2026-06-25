{
  nixpkgs,
  go2nix,
  go2nix-nixpkgs,
  nix-editor,
  rev,
  self,
  system,
}:
let
  pkgs = nixpkgs.legacyPackages.${system};
  go2nixPkgs = go2nix-nixpkgs.legacyPackages.${system};
  goEnv = go2nix.lib.mkGoEnv {
    inherit (go2nixPkgs) callPackage go;
    go2nix = go2nix.packages.${system}.go2nix;
  };
  go2nixNix = go2nixPkgs.nixVersions.nix_2_34;
  go2nixPlugin = go2nix.packages.${system}.go2nix-nix-plugin;
  nix-editor-pkg = nix-editor.packages.${system}.nix-editor;
in
rec {
  default = upm;
  devShell = pkgs.callPackage ./devshell { nix-editor = nix-editor-pkg; };
  fmt = pkgs.callPackage ./fmt { };
  upm = go2nixPkgs.callPackage ./upm {
    inherit
      self
      rev
      goEnv
      ;
  };
  go2nix-nix = go2nixNix;
  go2nix-nix-plugin = go2nixPlugin;
}
