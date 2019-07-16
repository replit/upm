package table

// Table represents a set of simple tabular data. Tables have a list
// of header cells and a list of rows. Each row must be the same
// length as the list of header cells. Tables can be formatted nicely
// to stdout. Construct a table with the New or FromStructs functions,
// and then use the AddRow, SortBy, and Print methods.
type Table struct {
	headers []string
	rows    [][]string
}
