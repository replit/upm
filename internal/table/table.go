package table

import (
	"fmt"
	"sort"
	"strings"

	"github.com/replit/upm/internal/util"
)

func New(headers ...string) Table {
	return Table{headers: headers}
}

func (t *Table) AddRow(row ...string) {
	if len(row) != len(t.headers) {
		util.Panicf(
			"wrong number of columns in table row (%d != %d)",
			len(row), len(t.headers),
		)
	}
	t.rows = append(t.rows, row)
}

type tableSorter struct {
	table Table
	index int
}

func (ts *tableSorter) Len() int {
	return len(ts.table.rows)
}

func (ts *tableSorter) Swap(i, j int) {
	ts.table.rows[i], ts.table.rows[j] = ts.table.rows[j], ts.table.rows[i]
}

func (ts *tableSorter) Less(i, j int) bool {
	return ts.table.rows[i][ts.index] < ts.table.rows[j][ts.index]
}

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

func (t *Table) Print() {
	widths := make([]int, len(t.headers))
	for j := range t.headers {
		widths[j] = len(t.headers[j])
	}
	for i := range t.rows {
		for j := range t.rows[i] {
			if len(t.rows[i][j]) > widths[j] {
				widths[j] = len(t.rows[i][j])
			}
		}
	}
	fields := make([]string, len(t.headers))
	for j := range t.headers {
		padding := widths[j] - len(t.headers[j])
		fields[j] = t.headers[j] + strings.Repeat(" ", padding)
	}
	fmt.Println(strings.Join(fields, "   "))
	for j := range t.headers {
		fields[j] = strings.Repeat("-", widths[j])
	}
	fmt.Println(strings.Join(fields, "   "))
	for i := range t.rows {
		for j := range t.rows[i] {
			padding := widths[j] - len(t.rows[i][j])
			fields[j] = t.rows[i][j] + strings.Repeat(" ", padding)
		}
		fmt.Println(strings.Join(fields, "   "))
	}
}
