{
  bun,
  go,
  mkShell,
  nodejs,
  nodePackages,
  nix-editor,
  poetry,
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
  ];
}
