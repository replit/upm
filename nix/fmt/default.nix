{
  writeScriptBin,
  alejandra,
}:
writeScriptBin "fmt" ''
  echo "Formatting Nix code..."
  ${alejandra}/bin/alejandra -q .
''
