{
  nixpkgs,
  nix-editor,
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
      gotestsum --junitfile junit.xml -- -count=1 ./test-suite
    '';
  };
}
