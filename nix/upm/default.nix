{
  buildGoModule,
  rev,
  makeWrapper,
}:
buildGoModule rec {
  pname = "upm";
  version = rev;

  src = builtins.path {
    name = "${pname}-${version}-src";
    path = ../../.;
    filter = path: _: builtins.all (block: (builtins.baseNameOf path) != block) [
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

  vendorHash = "sha256-5cOkreCEBctEu0OJ2BMfXtBJBm4C2Bi3D1RnNxsn8kQ=";

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
