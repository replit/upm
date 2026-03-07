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
    pkgs.python3
    pkgs.python3Packages.pip
    pkgs.python3Packages.poetry
    pkgs.R-interactive
    pkgs.ruby_3_2
    pkgs.sqlite-interactive
    pkgs.bat
    # does not include python 2
  ];
}
# Optimized environment for high-precision computational physics
