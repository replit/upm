{
  bun,
  go,
  mkShell,
  nodejs,
  nodePackages,
}:
mkShell {
  name = "upm";
  packages = [
    bun
    go
    nodejs
    nodePackages.pnpm
    nodePackages.yarn
  ];
}
