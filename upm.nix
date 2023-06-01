{ buildGoModule, statik, rev, go_1_17 } :

let

  # TODO: buildGo117Module is not available in nixpkgs right now because it doesn't work
  # on darwin yet. Once that has been fixed, we won't need this override anymore.
  buildGo117Module = buildGoModule.override { go = go_1_17; };

in
buildGo117Module {
    pname = "upm";
    version = rev;

    src = ./.;

    vendorSha256 = "sha256-0g/j6QO3Q3+xM+rKEPe9jgMRfrnueUy5hHdqBTsIfsg=";

    ldflags = [
      "-X github.com/replit/upm/internal/cli.version=${rev}"
    ];

    preBuild = ''
        ${statik}/bin/statik -src resources -dest internal -f
        go generate ./internal/backends/python
    '';

    doCheck = false;
}
