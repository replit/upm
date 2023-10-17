{
  bun,
  go,
  mkShell,
  nodejs,
  nodePackages,
  nix-editor,
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
  ];
}
