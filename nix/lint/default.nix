{ pkgs }:
pkgs.buildGoModule {
  pname = "upm-lint";
  version = "0";
  src = pkgs.lib.fileset.toSource {
    root = ../..;
    fileset = pkgs.lib.fileset.unions [
      ../../cmd
      ../../internal
      ../../resources
      ../../test-suite
      ../../go.mod
      ../../go.sum
      ../../Makefile
    ];
  };
  vendorHash = "sha256-A4CU4C5SmEZP6Q+CVHjp+Jy4UxRXAnpvhzrsdq4NlsM=";
  proxyVendor = true;
  nativeBuildInputs = [ pkgs.golangci-lint ];
  buildPhase = ''
    runHook preBuild
    export GOLANGCI_LINT_CACHE="$TMPDIR/golangci-lint"
    make generated
    golangci-lint run
    runHook postBuild
  '';
  doCheck = false;
  installPhase = ''
    touch "$out"
  '';
}
