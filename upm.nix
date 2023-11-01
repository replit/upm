{ buildGoModule, statik, rev, go_1_20, makeWrapper } :

let
  buildGo120Module = buildGoModule.override { go = go_1_20; };

in
buildGo120Module {
    pname = "upm";
    version = rev;

    src = ./.;

    vendorSha256 = "sha256-HJ17fDWI7NY1HR6zyLre6jaYDxXf2oMICSwwhIzIThg=";

    ldflags = [
      "-X github.com/replit/upm/internal/cli.version=${rev}"
    ];

    preBuild = ''
        ${statik}/bin/statik -src resources -dest internal -f
        go generate ./internal/backends/python
    '';

    buildInputs = [ makeWrapper ];

    subPackages = ["cmd/upm"];

    postInstall = ''
      make internal/backends/python/pypi_map.sqlite
      mv internal/backends/python/pypi_map.sqlite $out/

      wrapProgram $out/bin/upm \
        --set PYPI_MAP_DB "$out/pypi_map.sqlite"
    '';

    doCheck = false;
}
