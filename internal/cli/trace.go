package cli

import (
	"fmt"
	"os"

	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

func MaybeTrace() {
	fmt.Println("UPM.MaybeTrace")
	if os.Getenv("UPM_TRACE") != "1" {
		return
	}

	replid := os.Getenv("REPL_ID")
	if replid == "" {
		return
	}

	fmt.Println("UPM.starting trace")
	tracer.Start(
		tracer.WithService("upm"),
		tracer.WithGlobalTag("replid", replid),
		tracer.WithServiceVersion(getVersion()),
	)
}
