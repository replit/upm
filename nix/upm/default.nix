{
  buildGoModule,
  rev,
  makeWrapper,
}:
buildGoModule {
  pname = "upm";
  version = rev;

  src = ../../.;

  vendorHash = "sha256-JVhAQcu2+ZzAV+RfQgHnsYgIDpc9GHwmu4wbnIraxoU=";

  ldflags = [
    "-X github.com/replit/upm/internal/cli.version=${rev}"
  ];

  preBuild = ''
    go generate ./internal/backends/python
  '';

  buildInputs = [makeWrapper];

  subPackages = ["cmd/upm"];

  postInstall = ''
    make internal/backends/python/pypi_map.sqlite
    mv internal/backends/python/pypi_map.sqlite $out/

    wrapProgram $out/bin/upm \
      --set PYPI_MAP_DB "$out/pypi_map.sqlite"
  '';

  doCheck = false;
}
