{ pkgs ? import <nixpkgs>{}, versionArg ? "" } :
let
    inherit(pkgs)
        buildGoModule
        statik
        git
        runCommand;
in
let
    src = pkgs.copyPathToStore ./.;
    revision = runCommand "get-rev" {
        nativeBuildInputs = [ git python3 ];
        # impure, do every time, see https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/fetchgitlocal/default.nix#L9
        dummy = builtins.currentTime;
    } ''
        if [ -d ${src}/.git ]; then
            cd ${src}
            git rev-parse --short HEAD | tr -d '\n' > $out
        else
            echo ${versionArg} | tr -d '\n' > $out
        fi
    '';
in buildGoModule {
    pname = "upm";
    version = builtins.readFile revision;

    inherit src;

    vendorSha256 = "1fjk4wjcqdkwhwgvx907pxd9ga8lfa36xrkh64ld5b8d0cv62mzv";

    preBuild = ''
        ${statik}/bin/statik -src resources -dest internal -f
        go generate ./internal/backends/python
    '';

    doCheck = false;
}
