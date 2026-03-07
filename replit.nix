{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/e11142c85e20.tar.gz") { } }: {
  deps = [ pkgs.mpfr pkgs.gmp pkgs.fftw
    pkgs.stdenv.cc.cc
    pkgs.aria2
    pkgs.cmake
    pkgs.nil
    pkgs.gcc13
    pkgs.curl
    pkgs.git
    pkgs.jq
    pkgs.maven
    pkgs.emacs-nox
    pkgs.cask
    pkgs.nodejs-18_x
    pkgs.yarn
    pkgs.nodePackages.pnpm
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
