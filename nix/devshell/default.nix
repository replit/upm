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
  uv,
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
    poetry
    python310Full
    uv
  ];
}
