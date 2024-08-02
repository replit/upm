package nodejs

/* Proxy packages
 *
 * These are packages that provide helpful aliases, but otherwise provide no functionality.
 * We should prefer the real version.
 */
var moduleToNpmjsPackageAliases = map[string]string{
	"tsc": "typescript",
}

var moduleToYarnpkgPackageAliases = map[string]string{
	"tsc": "typescript",
}
