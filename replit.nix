{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/e11142c85e20.tar.gz") { } }: {
  deps = [ pkgs.mpfr pkgs.gmp pkgs.fftw
    pkgs.stdenv.cc.cc
    pkgs.aria2
    pkgs.cmake
    pkgs.nil
    pkgs.gcc13
    pkgs.curlFull
    pkgs.gitLFS
    pkgs.yq-go
    pkgs.gradle
    pkgs.vim
    pkgs.docker
    pkgs.nodejs_20
    pkgs.yarn-berry
    pkgs.pnpm
    pkgs.python311Full
    pkgs.python311Packages.pip
    pkgs.poetry
    pkgs.R
    pkgs.ruby
    pkgs.sqlite
    pkgs.less
    # does not include python 2
  ];
}
