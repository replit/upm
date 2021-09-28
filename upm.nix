{ buildGoModule, statik, rev } :

buildGoModule {
    pname = "upm";
    version = rev;

    src = ./.;

    vendorSha256 = "1a4y55gyzin4gdvgx5dkh0bkm1jv24cwj106nd35i1liwwy3w89c";

    buildFlagsArray = [
      "-ldflags=-X github.com/replit/upm/internal/cli.version=${rev}"
    ];

    preBuild = ''
        ${statik}/bin/statik -src resources -dest internal -f
        go generate ./internal/backends/python
    '';

    doCheck = false;
}
