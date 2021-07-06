{ buildGoModule, statik, rev } :

buildGoModule {
    pname = "upm";
    version = rev;

    src = ./.;

    vendorSha256 = "1fjk4wjcqdkwhwgvx907pxd9ga8lfa36xrkh64ld5b8d0cv62mzv";

    buildFlagsArray = [
      "-ldflags=-X github.com/replit/upm/internal/cli.version=${rev}"
    ];

    preBuild = ''
        ${statik}/bin/statik -src resources -dest internal -f
        go generate ./internal/backends/python
    '';

    doCheck = false;
}
