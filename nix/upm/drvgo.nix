{ drvgo, rev, system, runCommand, makeWrapper }:
let
  src = drvgo.lib.projectSource {
    root = ../..;
    files = [ ../../go.mod ../../go.sum ../../cmd ../../internal ../../resources ];
  };
  build = args: drvgo.lib.mkPackage ({ inherit system src; cgoEnabled = true; } // args);
  command = build {
    name = "upm";
    package = "./cmd/upm";
    ldflags = "-X github.com/replit/upm/internal/cli.version=${rev}";
  };
  generator = build {
    name = "upm-generator";
    package = "./internal/backends/python/gen_pypi_map";
  };
  data = ../../internal/backends/python;
  database = runCommand "upm-pypi-map" { } ''
    mkdir -p "$out"
    ${generator}/bin/gen_pypi_map gen -out "$out/pypi_map.sqlite" \
      -pkgsfile ${data}/pkgs.json -legacypkgsfile ${data}/pypi_packages.json \
      -bq ${data}/download_stats.json
    test -s "$out/pypi_map.sqlite"
  '';
in
runCommand "upm-${rev}" { nativeBuildInputs = [ makeWrapper ]; } ''
  mkdir -p "$out/bin"
  ln -s ${database}/pypi_map.sqlite "$out/pypi_map.sqlite"
  makeWrapper ${command}/bin/upm "$out/bin/upm" --set PYPI_MAP_DB "$out/pypi_map.sqlite"
''
