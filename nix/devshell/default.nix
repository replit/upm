{
  bun,
  go,
  mkShell,
  nodejs,
  nodePackages,
  nix-editor,
  pnpm_8,
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
    pnpm_8
    nodePackages.yarn
    python310Packages.pip
    poetry
    python310Full
  ];
}
