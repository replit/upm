{
  buildGoCache,
  runCommand,
}:
buildGoCache {
  # keep this up-to-date in CI with:
  # $ nix run 'github:numtide/build-go-cache#get-external-imports' -- ./. imported-packages
  importPackagesFile = ./imported-packages;
  # FIXME: Somehow we get cache invalidation everytime if we don't do this, also it uses a source filter
  src = runCommand "go-mod" {} ''
    install -D ${../../go.mod} $out/go.mod
    install -D ${../../go.sum} $out/go.sum
  '';
  vendorHash = "sha256-A4CU4C5SmEZP6Q+CVHjp+Jy4UxRXAnpvhzrsdq4NlsM=";
  proxyVendor = true;
}
