{ pkgs ? import <nixpkgs>{} } :
let
    buildGoModule = pkgs.buildGoModule;
    statik = pkgs.statik;
    git = pkgs.git;
    runCommand = pkgs.runCommand;
in
let
    src = pkgs.copyPathToStore ./.;
    revision = runCommand "get-rev" {
        nativeBuildInputs = [ git ];
        dummy = builtins.currentTime;
    } "GIT_DIR=${src}/.git git rev-parse --short HEAD | tr -d '\n' > $out";
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
