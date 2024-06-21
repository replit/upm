{
  bun,
  go,
  mkShell,
  nodejs,
  nodePackages,
  nix-editor,
  poetry,
  python310Full,
  python310Packages,
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
    python310Packages.pip
    python310Packages.flit
    poetry
    python310Full
  ];
}
