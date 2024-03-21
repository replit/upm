{
  buildGoModule,
  rev,
  makeWrapper,
  buildGoCache,
  lib,
  runCommand,
}:
let
  vendorHash = "sha256-7t87/28pBxU7A+SRvw6xxJQ2wErZ/76OJC8feNEQ80E=";

  goCache = buildGoCache {
    # keep this up-to-date in CI with:
    # $ nix run 'github:numtide/build-go-cache#get-external-imports' -- ./. imported-packages
    importPackagesFile = ./imported-packages;
    # FIXME: Somehow we get cache invalidation everytime if we don't do this, also it uses a source filter
    src = runCommand "go-mod" {} ''
      install -D ${../../go.mod} $out/go.mod
      install -D ${../../go.sum} $out/go.sum
    '';
    inherit vendorHash;
  };
in
buildGoModule rec {
  pname = "upm";
  version = rev;
  src = builtins.path {
    name = "${pname}-src";
    path = ../../.;
    filter = path: _:
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

  buildInputs = [makeWrapper goCache];

  subPackages = ["cmd/upm"];

  postInstall = ''
    make internal/backends/python/pypi_map.sqlite
    mv internal/backends/python/pypi_map.sqlite $out/

    wrapProgram $out/bin/upm \
      --set PYPI_MAP_DB "$out/pypi_map.sqlite"
  '';

  inherit vendorHash;
  proxyVendor = true; # we only support proxyVendor with buildGoCache just now

  doCheck = false;
}
