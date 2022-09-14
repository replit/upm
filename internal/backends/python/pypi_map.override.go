package python

/*
Manual module -> package mapping overrides
*/
var moduleToPypiPackageOverride = map[string]string{
	"grpc_status":  "grpcio-status",       // 2nd most popular
	"nvd3":         "python-nvd3",         // not popular enough 6th in popularity
	"requirements": "requirements-parser", // popular rlbot depends on it, but doesn't supply requires_dist
	"base62":       "pybase62",            // it was overridden by base-62 which wins due to name match but is less popular by far
}
