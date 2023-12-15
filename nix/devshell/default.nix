{
  bun,
  go,
  mkShell,
  nodejs,
  nodePackages,
  nix-editor,
  poetry,
  python311Full,
  python311Packages,
  golangci-lint,
}:
mkShell {
  name = "upm";
  packages = [
    bun
    go
    golangci-lint
    nix-editor
    nodejs
    nodePackages.pnpm
    nodePackages.yarn
    python311Packages.pip
    poetry
    python311Full
  ];
}
