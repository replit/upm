{
  bun,
  go,
  mkShell,
  nodejs,
  nodePackages,
  nix-editor,
  poetry,
  python311Full,
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
    poetry
    python311Full
  ];
}
