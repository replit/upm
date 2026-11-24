{
  nixpkgs,
  nix-editor,
  drvgo,
  rev,
  self,
  system,
}:
let
  pkgs = nixpkgs.legacyPackages.${system};
  nix-editor-pkg = nix-editor.packages.${system}.nix-editor;
in
rec {
  default = upm;
  devShell = pkgs.callPackage ./devshell { nix-editor = nix-editor-pkg; };
  fmt = pkgs.callPackage ./fmt { };
  upm = pkgs.callPackage ./upm { inherit self rev; };
  test-suite = pkgs.writeShellApplication {
    name = "upm-test-suite";
    runtimeInputs = [
      upm
      pkgs.bash
      pkgs.gotestsum
      pkgs.git
      pkgs.coreutils
      pkgs.stdenv.cc
    ] ++ devShell.nativeBuildInputs;
    text = ''
      venv=$(mktemp -d)
      trap 'rm -rf "$venv"' EXIT
      python -m venv "$venv"
      export VIRTUAL_ENV="$venv"
      export PATH="$VIRTUAL_ENV/bin:$PATH"
      export npm_config_script_shell="${pkgs.bash}/bin/sh"
      export UV_PYTHON="${pkgs.python311}/bin/python3.11"
      gotestsum --junitfile junit.xml -- -count=1 ./test-suite
    '';
  };
} // pkgs.lib.optionalAttrs (builtins.hasAttr system drvgo.packages) {
  upm-drvgo = pkgs.callPackage ./upm/drvgo.nix { inherit drvgo rev system; inherit (self.packages.${system}.upm) goModules; };
}
