{
  buildGoModule,
  self,
  goEnv,
  rev,
  gnumake,
  lib,
  makeWrapper,
}:
let
  pname = "upm";
  filteredSrc = builtins.path {
    name = "${pname}-src";
    path = self;
    filter =
      path: _:
      builtins.all (block: (builtins.baseNameOf path) != block) [
        ".github"
        ".semaphore"
        "packaging"
        "scripts"
        "test-suite"
        ".goreleaser.yml"
        ".replit"
        "replit.nix"
      ];
  };
  pypiMapSrc = builtins.path {
    name = "${pname}-pypi-map-src";
    path = self;
    filter =
      path: _:
      let
        rel = lib.removePrefix (toString self + "/") (toString path);
        prefixes = [
          "Makefile"
          "go.mod"
          "go.sum"
          "internal/api"
          "internal/backends/python"
          "internal/config"
          "internal/util"
          "resources"
        ];
      in
      path == toString self
      || builtins.any (prefix: rel == prefix || lib.hasPrefix (prefix + "/") rel) prefixes
      || builtins.any (prefix: lib.hasPrefix (rel + "/") prefix) prefixes;
  };
  pypiMapDb = buildGoModule {
    pname = "${pname}-pypi-map";
    version = rev;
    src = pypiMapSrc;
    nativeBuildInputs = [ gnumake ];
    buildPhase = ''
      runHook preBuild
      make internal/backends/python/pypi_map.sqlite
      runHook postBuild
    '';
    installPhase = ''
      cp internal/backends/python/pypi_map.sqlite $out
    '';
    vendorHash = "sha256-A4CU4C5SmEZP6Q+CVHjp+Jy4UxRXAnpvhzrsdq4NlsM=";
    proxyVendor = true;
    doCheck = false;
  };
in
goEnv.buildGoApplication {
  pname = "upm";
  version = rev;
  goLock = ../../go2nix.toml;
  src = filteredSrc;

  ldflags = [
    "-X github.com/replit/upm/internal/cli.version=${rev}"
  ];

  nativeBuildInputs = [
    makeWrapper
  ];

  subPackages = [ "cmd/upm" ];

  postInstall = ''
    cp ${pypiMapDb} $out/pypi_map.sqlite

    wrapProgram $out/bin/upm \
      --set PYPI_MAP_DB "$out/pypi_map.sqlite"
  '';

  doCheck = false;
}
