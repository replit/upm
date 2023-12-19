{pkgs}: {
  deps = [
    pkgs.golangci-lint
    pkgs.wget
    pkgs.gnumake
    pkgs.gopls
    pkgs.gcc
    pkgs.curl
    pkgs.git
    pkgs.jq
    pkgs.maven
    pkgs.emacs-nox
    pkgs.cask
    pkgs.nodejs-18_x
    pkgs.yarn
    pkgs.nodePackages.pnpm
    pkgs.python310Full
    pkgs.python310Packages.pip
    pkgs.pip
    pkgs.poetry
    pkgs.R
    pkgs.ruby
    pkgs.sqlite
    pkgs.less
    # does not include python 2
  ];
}
