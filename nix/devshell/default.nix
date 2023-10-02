{
  go,
  mkShell,
}:
mkShell {
  name = "upm";
  packages = [
    go
  ];
}
