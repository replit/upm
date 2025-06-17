{
  self,
  buildGoModule,
  rev,
  makeWrapper,
}:
buildGoModule rec {
  pname = "upm";
  version = rev;
  src = builtins.path {
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

  ldflags = [
    "-X github.com/replit/upm/internal/cli.version=${rev}"
  ];

  preBuild = ''
    go generate ./internal/backends/python
  '';

  buildInputs = [
    makeWrapper
  ];

  subPackages = [ "cmd/upm" ];

  postInstall = ''
    make internal/backends/python/pypi_map.sqlite
    mv internal/backends/python/pypi_map.sqlite $out/

    wrapProgram $out/bin/upm \
      --set PYPI_MAP_DB "$out/pypi_map.sqlite"
  '';

  vendorHash = "sha256-A4CU4C5SmEZP6Q+CVHjp+Jy4UxRXAnpvhzrsdq4NlsM=";
  proxyVendor = true; # we only support proxyVendor with buildGoCache just now

  doCheck = false;
}
