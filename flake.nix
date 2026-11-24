{
  description = "Universal Package Manager";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.drvgo.url = "github:drvlang/drvgo";
  # nix-editor is a dev dependency in this project. At Replit, we use
  # nix-editor directly too, so nix-editor is already in the
  # environment.
  inputs.nix-editor.url = "github:replit/nix-editor";
  inputs.nix-editor.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    {
      self,
      nixpkgs,
      nix-editor,
      drvgo,
    }:
    let
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      eachSystem = nixpkgs.lib.genAttrs systems;
      rev = if self ? rev then "0.0.0-${builtins.substring 0 7 self.rev}" else "0.0.0-dirty";
    in
    {
      packages = eachSystem (
        system:
        import ./nix {
          inherit
            self
            nixpkgs
            rev
            system
            nix-editor
            drvgo
            ;
        }
      );
      devShells = eachSystem (system: {
        default = self.packages.${system}.devShell;
      });
      formatter = eachSystem (system: self.packages.${system}.fmt);
      checks.x86_64-linux.upm-drvgo-smoke =
        let upm = self.packages.x86_64-linux.upm-drvgo; in
        nixpkgs.legacyPackages.x86_64-linux.runCommand "upm-drvgo-smoke" { } ''
          test "$(${upm}/bin/upm --version)" = "upm ${rev}"
          test -s ${upm}/pypi_map.sqlite
          echo 'import requests' > main.py
          test "$(${upm}/bin/upm --lang python3-pip guess --all)" = requests
          touch "$out"
        '';
      checks.x86_64-linux.lint = import ./nix/lint {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
      };
    };
}
