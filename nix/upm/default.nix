{
  buildGoModule,
  statik,
  rev,
  makeWrapper,
}:
buildGoModule {
  pname = "upm";
  version = rev;

  src = ../../.;

  vendorSha256 = "sha256-RXpw5JGKUPBkjSO7ZNnXO6XtOwF+y3Gd9bXPP2bBDj4=";

  ldflags = [
    "-X github.com/replit/upm/internal/cli.version=${rev}"
  ];

  preBuild = ''
    ${statik}/bin/statik -src resources -dest internal -f
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
