{ nixpkgs, nix-editor, system }:
let
  pkgs = nixpkgs.legacyPackages.${system};
  src = pkgs.lib.fileset.toSource {
    root = ../.;
    fileset = pkgs.lib.fileset.unions [
      ../cmd
      ../internal
      ../resources
      ../test-suite
      ../go.mod
      ../go.sum
      ../Makefile
    ];
  };
  # The release version includes the Git revision; unrelated commits should
  # not invalidate the test binary or the check.
  packages = import ./default.nix {
    inherit nixpkgs nix-editor system;
    self = src;
    rev = "0.0.0-test";
  };
  suite = pkgs.runCommand "upm-integration-check" {
    nativeBuildInputs = [ pkgs.cacert ];
  } ''
    export HOME="$TMPDIR/home"
    mkdir -p "$HOME"
    cp -R ${src} source
    chmod -R u+w source
    cd source
    ${packages.test-suite}/bin/upm-test-suite
  '';
  # A fixed-output build may use the network. Key its only output to the
  # ordinary derivation so changing any declared input invalidates success.
  marker = builtins.unsafeDiscardStringContext (builtins.baseNameOf suite.drvPath);
in
suite.overrideAttrs (old: {
  outputHashAlgo = "sha256";
  outputHashMode = "flat";
  outputHash = builtins.hashString "sha256" marker;
  preferHashedMirrors = false;
  buildCommand = old.buildCommand + ''
    printf '%s' '${marker}' > "$out"
  '';
})
