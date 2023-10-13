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
<<<<<<< HEAD
    filter = path: type: type != "directory" ||
      builtins.baseNameOf path != "test-suite";
=======
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
>>>>>>> 4b269225ade535c1d15e2821b1bff8fc799d55a8
  };

  vendorHash = "sha256-2F2/BcHUEpbYxmAW1SsIBbn6U2VWinWjdxMvsbzfKsc=";

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
