// Package table provides a simple API for outputting tabular data to
// stdout. It is used to implement --format=table.
package table

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"reflect"
	"sort"
	"strings"

	"github.com/replit/upm/internal/util"
	"golang.org/x/crypto/ssh/terminal"
)

// New creates a new table with the given headers. The table has no
// rows; add them with AddRow. The headers should all be unique.
func New(headers ...string) Table {
	seen := map[string]bool{}
	for _, header := range headers {
		if seen[header] {
			util.Panicf("duplicate table header: %s", header)
		} else {
			seen[header] = true
		}
	}
	return Table{headers: headers}
}

// FromStructs creates a new table from the given slice of structs.
// The table headers are generated from the struct field reflection
// metadata: each struct field must have a reflection metadata key
// "pretty" whose value is the header to display. The only allowed
// field types in the struct are string and []string. The strings are
// used as table cells directly, while the slices are concatenated
// with commas first.
func FromStructs(structs interface{}) Table {
	sv := reflect.ValueOf(structs)
	st := reflect.TypeOf(structs).Elem()

	indices := []int{}
	headers := []string{}
	for i := 0; i < st.NumField(); i++ {
		nonempty := false
		for j := 0; j < sv.Len(); j++ {
			if sv.Index(j).Field(i).Len() > 0 {
				nonempty = true
				break
			}
		}
		if !nonempty {
			continue
		}
		indices = append(indices, i)
		header := st.Field(i).Tag.Get("pretty")
		headers = append(headers, header)
	}

	t := Table{headers: headers}
	for j := 0; j < sv.Len(); j++ {
		row := []string{}
		for _, i := range indices {
			var value string
			rfield := sv.Index(j).Field(i)
			switch rfield.Kind() {
			case reflect.String:
				value = rfield.String()
			case reflect.Slice:
				parts := []string{}
				for j := 0; j < rfield.Len(); j++ {
					str := rfield.Index(j).String()
					parts = append(parts, str)
				}
				value = strings.Join(parts, ", ")
			}
			row = append(row, value)
		}
		t.AddRow(row...)
	}
	return t
}

// AddRow adds a row at the end of a table. The length of the row must
// be the same as the number of headers in the table, or a panic will
// be generated.
func (t *Table) AddRow(row ...string) {
	if len(row) != len(t.headers) {
		util.Panicf(
			"wrong number of columns in table row (%d != %d)",
			len(row), len(t.headers),
		)
	}
	t.rows = append(t.rows, row)
}

// tableSorter is a dummy struct used to sort a table by a given
// column index. It implements sort.Interface.
type tableSorter struct {
	table Table
	index int
}

// Len implements sort.Interface. It returns the number of rows in the
// table.
func (ts *tableSorter) Len() int {
	return len(ts.table.rows)
}

// Swap implements sort.Interface. It swaps the given rows, mutating
// the table.
func (ts *tableSorter) Swap(i, j int) {
	ts.table.rows[i], ts.table.rows[j] = ts.table.rows[j], ts.table.rows[i]
}

// Less implements sort.Interface. It compares the given rows by
// looking at the sort column.
func (ts *tableSorter) Less(i, j int) bool {
	return ts.table.rows[i][ts.index] < ts.table.rows[j][ts.index]
}

// SortBy sorts a table by the column with the given header. The
// header must exist in the table, or a panic is generated. Since
// tables cannot have duplicate headers, any column can be specified
// unambiguously.
func (t *Table) SortBy(header string) {
	var index int
	found := false
	for i := range t.headers {
		if t.headers[i] == header {
			index = i
			found = true
			break
		}
	}
	if !found {
		util.Panicf("no such header: %s", header)
	}
	sorter := &tableSorter{table: *t, index: index}
	sort.Sort(sorter)
}

// printOrPage either prints text to stdout or invokes the 'less'
// utility to display it. 'less' is invoked if stdout is connected to
// a tty, the provided width is too wide for the tty, and 'less' is
// actually installed.
func printOrPage(text string, width int) {
	termWidth, _, err := terminal.GetSize(1)
	if err != nil || width < termWidth {
		fmt.Print(text)
		return
	}

	less, err := exec.LookPath("less")
	if err != nil {
		fmt.Print(text)
		return
	}

	util.ProgressMsg("less -S")

	cmd := exec.Cmd{
		Path: less,
		Args: []string{"less", "-S"},
		// Normally, LANG or equivalent environment variables
		// will be set, so less will use the right charset out
		// of the box. Unfortunately this doesn't happen in
		// Docker, so we have to configure less manually
		// (otherwise it will display some non-ASCII
		// characters as escape sequences). See the man page
		// for less.
		Env:    append(os.Environ(), "LESSCHARSET=utf-8"),
		Stdout: os.Stdout,
		Stderr: os.Stderr,
	}
	stdin, err := cmd.StdinPipe()
	if err != nil {
		util.Die("connecting pipe to pager stdin: %s", err)
	}

	if _, err := io.WriteString(stdin, text); err != nil {
		util.Die("writing to pager: %s", err)
	}
	if err := stdin.Close(); err != nil {
		util.Die("closing pipe to pager stdin: %s", err)
	}

	if err := cmd.Run(); err != nil {
		util.Die("running pager: %s", err)
	}
}

// Print writes the table to stdout, aligning columns by inserting
// whitespace. If the table is too wide for the current terminal, and
// the 'less' utility is installed, Print invokes it with the -S
// option to truncate long lines and allow horizontal scrolling.
func (t *Table) Print() {
	lines := []string{}
	widths := make([]int, len(t.headers))
	for j := range t.headers {
		widths[j] = len([]rune(t.headers[j]))
	}
	for i := range t.rows {
		for j := range t.rows[i] {
			if len([]rune(t.rows[i][j])) > widths[j] {
				widths[j] = len([]rune(t.rows[i][j]))
			}
		}
	}
	fields := make([]string, len(t.headers))
	for j := range t.headers {
		padding := widths[j] - len([]rune(t.headers[j]))
		fields[j] = t.headers[j] + strings.Repeat(" ", padding)
	}
	lines = append(lines, strings.Join(fields, "   "))
	for j := range t.headers {
		fields[j] = strings.Repeat("-", widths[j])
	}
	lines = append(lines, strings.Join(fields, "   "))
	for i := range t.rows {
		for j := range t.rows[i] {
			padding := widths[j] - len([]rune(t.rows[i][j]))
			fields[j] = t.rows[i][j] + strings.Repeat(" ", padding)
		}
		lines = append(lines, strings.Join(fields, "   "))
	}
	// A bit of a hack; we should really compute this directly
	// from the widths array, but this is simple.
	totalWidth := len([]rune(lines[1]))
	printOrPage(strings.Join(lines, "\n")+"\n", totalWidth)
}
