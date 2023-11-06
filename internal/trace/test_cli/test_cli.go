package main

import (
	"fmt"
	"os"
	"os/exec"

	"github.com/replit/upm/internal/trace"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

func main() {
	tracer.Start(
		tracer.WithService("upm_wrapper"),
	)
	defer tracer.Stop()

	span := tracer.StartSpan("upm_wrapper")
	defer span.Finish()
	var traceID string
	var spanID string
	if w3Cctx, ok := span.Context().(ddtrace.SpanContextW3C); ok {
		traceID = trace.GetHexTraceID(w3Cctx)
		spanID = trace.GetHexSpanID(w3Cctx)
	}

	cmd := exec.Command("upm", "guess", "-a", "-f", "-l", "nodejs")
	cmd.Env = os.Environ()
	cmd.Env = append(cmd.Env, "UPM_TRACE=1")
	cmd.Env = append(cmd.Env, "REPL_ID=ABCDE")
	cmd.Env = append(cmd.Env, fmt.Sprintf("DD_TRACE_ID=%s", traceID))
	cmd.Env = append(cmd.Env, fmt.Sprintf("DD_SPAN_ID=%s", spanID))

	stdout, err := cmd.Output()

	if err != nil {
		fmt.Println(err.Error())
		return
	}

	// Print the output
	fmt.Println(string(stdout))
}
