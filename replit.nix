{ pkgs }: {
    deps = [
        pkgs.wget
        pkgs.gnumake
        pkgs.go_1_17
        pkgs.gopls
        pkgs.gcc
        pkgs.curl
        pkgs.git
        pkgs.jq
        pkgs.maven
        pkgs.emacs-nox
        pkgs.cask
        pkgs.nodejs-16_x
        pkgs.yarn
        pkgs.python39Full
        pkgs.python39Packages.pip
        pkgs.python39Packages.poetry
        pkgs.R
        pkgs.ruby
        pkgs.sqlite
        pkgs.less
      # does not include python 2
    ];
}

