package cli

// outputFormat is an enum representing the argument of the --format
// option.
type outputFormat int

// Values for outputFormat.
const (
	// --format=table
	outputFormatTable outputFormat = iota

	// --format=json
	outputFormatJSON
)
