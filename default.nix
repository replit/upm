{ pkgs ? import <nixpkgs>{} } :
let
    inherit(pkgs)
        buildGoModule
        statik
        git
        python3
        runCommand;
in
let
    packageName = "upm";

    src = pkgs.copyPathToStore ./.;
    versionFile = builtins.fetchurl https://api.github.com/repos/replit/upm/commits/master;
    revision = runCommand "get-rev" {
        nativeBuildInputs = [ git python3 ];
        # impure, do every time, see https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/fetchgitlocal/default.nix#L9
        dummy = builtins.currentTime;
    } ''
        if [ -d ${src}/.git ]; then
            cd ${src}
            git rev-parse --short HEAD | tr -d '\n' > $out
        else
            cat ${versionFile} | python3 -c "import sys, json; print(json.load(sys.stdin)['sha'][0:7])" | tr -d '\n' > $out
        fi
    '';
in buildGoModule {
    pname = packageName;
    version = builtins.readFile revision;

    inherit src;

    vendorSha256 = "1fjk4wjcqdkwhwgvx907pxd9ga8lfa36xrkh64ld5b8d0cv62mzv";

    preBuild = ''
        ${statik}/bin/statik -src resources -dest internal -f
        go generate ./internal/backends/python
    '';

    doCheck = false;
}
