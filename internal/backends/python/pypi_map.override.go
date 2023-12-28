package python

/*
Manual module -> package mapping overrides
*/
var moduleToPypiPackageOverride = map[string][]string{
	"grpc_status":  {"grpcio-status"},       // 2nd most popular
	"nvd3":         {"python-nvd3"},         // not popular enough 6th in popularity
	"requirements": {"requirements-parser"}, // popular rlbot depends on it, but doesn't supply requires_dist
	"base62":       {"pybase62"},            // it was overridden by base-62 which wins due to name match but is less popular by far
	"faiss":        {"faiss-cpu"},           // faiss is offered as precompiled wheels, faiss-cpu and faiss-gpu.
	"graphics":     {"graphics.py"},         // this package is popular, but the module doesn't match the package name https://anh.cs.luc.edu/python/hands-on/3.1/handsonHtml/graphics.html#a-graphics-introduction
	"replit.ai":    {"replit-ai"},           // Replit's AI package
	"glm":          {"PyGLM", "glm"},        // Both of these packages are valid, but PyGLM is a library, glm is an executable.
}

/* Proxy packages
 *
 * These are packages that provide helpful aliases, but otherwise provide no functionality.
 * We should prefer the real version.
 */
var moduleToPypiPackageAliases = map[string][]string{
	"bs4":      {"beautifulsoup4"},
	"discord":  {"discord.py"},
	"psycopg2": {"psycopg2-binary"}, // psycopg2 is a source package, psycopg2-binary is the dist wheel
}
