{
  bun,
  go,
  mkShell,
  nodejs,
  nodePackages,
  nix-editor,
  poetry,
  python311Full,
}:
mkShell {
  name = "upm";
  packages = [
    bun
    go
    nodejs
    nodePackages.pnpm
    nodePackages.yarn
    nix-editor
    poetry
    python311Full
  ];
}
