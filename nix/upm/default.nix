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
    filter = path: _: builtins.any (allow: builtins.baseNameOf path == allow) [
      "cmd"
      "internal"
      "nix"
      "resources"
      ".gitignore"
      "flake.lock"
      "flake.nix"
      "go.mod"
      "go.sum"
      "LICENSE.md"
      "Makefile"
      "README.md"
    ];
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
