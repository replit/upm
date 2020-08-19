package main

import (
	"fmt"
	"io"
)

func DumpMapToGoVar(name string, m map[string]string, writer io.Writer) {
	fmt.Fprintf(writer, "var %v= map[string]string{\n", name)

	for key, value := range m {
		fmt.Fprintf(writer, "\t\"%v\":  \"%v\",\n", key, value)
	}
	fmt.Fprintf(writer, "}\n\n")
}

