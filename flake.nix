{
  description = "Universal Package Manager";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/808c0d8c53c7ae50f82aca8e7df263225cf235bf";
  # nix-editor is a dev dependency in this project. At Replit, we use
  # nix-editor directly too, so nix-editor is already in the
  # environment.
  inputs.nix-editor.url = "github:replit/nix-editor";
  inputs.nix-editor.inputs.nixpkgs.follows = "nixpkgs";

  inputs.build-go-cache.url = "github:numtide/build-go-cache/1f0056d94c4097cf8cfc9ed9c1864e400f689c6b";
  inputs.build-go-cache.inputs.nixpkgs.follows = "nixpkgs";

  outputs = {
    self,
    nixpkgs,
    nix-editor,
    build-go-cache,
  }: let
    systems = [
      "aarch64-darwin"
      "aarch64-linux"
      "x86_64-darwin"
      "x86_64-linux"
    ];
    eachSystem = nixpkgs.lib.genAttrs systems;
    rev =
      if self ? rev
      then "0.0.0-${builtins.substring 0 7 self.rev}"
      else "0.0.0-dirty";
  in {
    packages = eachSystem (system:
      import ./nix {
        inherit self nixpkgs rev system nix-editor;
        inherit (build-go-cache.legacyPackages.${system}) buildGoCache;
      });
    devShells = eachSystem (system: {
      default = self.packages.${system}.devShell;
    });
    formatter = eachSystem (system: self.packages.${system}.fmt);
  };
}
