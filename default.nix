{ lib, buildGoModule, fetchFromGitHub, statik }:

buildGoModule rec {
    name = "upm";
    version = "908674b";

    src = fetchFromGitHub{
        owner = "replit";
        repo = "upm";
        rev = "${version}";
        sha256 = "0y50q2fxz1q1xbxirjka70y247x70xxz8kj1h8dmgrzd793k3y6s";
    };

    vendorSha256 = "1fjk4wjcqdkwhwgvx907pxd9ga8lfa36xrkh64ld5b8d0cv62mzv";

    preBuild = ''
        ${statik}/bin/statik -src resources -dest internal -f
        go generate ./internal/backends/python
    '';

    doCheck = false;
}
