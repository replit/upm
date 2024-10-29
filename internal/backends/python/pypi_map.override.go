package python

var tf_variants = [...]string{"tensorflow-cpu", "tensorflow-cpu-aws", "tensorflow-gpu", "tensorflow-intel", "tf-nightly", "tf-nightly-cpu", "tf-nightly-cpu-aws", "tf-nightly-gpu", "tf-nightly-intel"}

/*
Manual module -> package mapping overrides
*/
var moduleToPypiPackageOverride = map[string][]string{
	"grpc_status":          {"grpcio-status"},                                        // 2nd most popular
	"nvd3":                 {"python-nvd3"},                                          // not popular enough 6th in popularity
	"requirements":         {"requirements-parser"},                                  // popular rlbot depends on it, but doesn't supply requires_dist
	"base62":               {"pybase62"},                                             // it was overridden by base-62 which wins due to name match but is less popular by far
	"edgar":                {"edgartools", "edgar"},                                  // Two different tools to access the Edgar service provided by the SEC, edgartools is more active
	"faiss":                {"faiss-cpu"},                                            // faiss is offered as precompiled wheels, faiss-cpu and faiss-gpu.
	"graphics":             {"graphics.py"},                                          // this package is popular, but the module doesn't match the package name https://anh.cs.luc.edu/python/hands-on/3.1/handsonHtml/graphics.html#a-graphics-introduction
	"replit.ai":            {"replit-ai"},                                            // Replit's AI package
	"glm":                  {"PyGLM", "glm"},                                         // Both of these packages are valid, but PyGLM is a library, glm is an executable.
	"hubspot":              {"hubspot-api-client"},                                   // "hubspot" is a namesquatted library that has not been touched since 2014
	"jwt":                  {"PyJWT", "jwt"},                                         // Both of these packages are valid, but PyJWT is more popular
	"ldclient":             {"ldclient-py", "ldclient"},                              // LaunchDarkly client
	"tensorflow":           append([]string{"tensorflow"}, tf_variants[:]...),        // Avoid suggesting conflicting tensorflow builds
	"tensorflow-federated": {"tensorflow-federated", "tensorflow-federated-nightly"}, // Avoid suggesting conflicting tensorflow-federated packages
	"tensorflow1":          {"tensorflow"},                                           // Avoid confusion.
	"z3":                   {"z3-solver", "z3"},                                      // Popular library from Microsoft Research vs abandoned beta project
}

/* Proxy packages
 *
 * These are packages that provide helpful aliases, but otherwise provide no functionality.
 * We should prefer the real version.
 */
var moduleToPypiPackageAliases = map[string]string{
	"bs4":      "beautifulsoup4",
	"discord":  "discord.py",
	"psycopg2": "psycopg2-binary", // psycopg2 is a source package, psycopg2-binary is the dist wheel
}
