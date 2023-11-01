{ buildGoModule, statik, rev, go_1_17, makeWrapper } :

let

  # TODO: buildGo117Module is not available in nixpkgs right now because it doesn't work
  # on darwin yet. Once that has been fixed, we won't need this override anymore.
  buildGo117Module = buildGoModule.override { go = go_1_17; };

in
buildGo117Module {
    pname = "upm";
    version = rev;

    src = ./.;

    vendorSha256 = "sha256-adPAwVN/2dbTBr1kt29rrfy+5dDEpCuZ5v5mPnv8sFk=";

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
