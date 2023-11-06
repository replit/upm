package trace

import (
	"context"
	"os"

	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

var (
	globalDDTraceID string
	globalDDSpanID  string
)

func MaybeTrace(serviceVersion string) bool {
	if os.Getenv("UPM_TRACE") != "1" {
		return false
	}

	replid := os.Getenv("REPL_ID")
	if replid == "" {
		return false
	}

	globalDDTraceID = os.Getenv("DD_TRACE_ID")
	globalDDSpanID = os.Getenv("DD_SPAN_ID")
	os.Unsetenv("DD_TRACE_ID")
	os.Unsetenv("DD_SPAN_ID")

	tracer.Start(
		tracer.WithService("upm"),
		tracer.WithGlobalTag("replid", replid),
		tracer.WithServiceVersion(serviceVersion),
	)
	return true
}

func StartSpanFromExistingContext(name string) (ddtrace.Span, context.Context) {
	ctx := context.Background()
	parentContext, _ := GetParentContext()
	if parentContext == nil {
		return tracer.StartSpanFromContext(ctx, name)
	}
	return tracer.StartSpanFromContext(ctx, name, WithParentContext(parentContext))
}

func GetParentContext() (*SpanContext, error) {
	traceID := globalDDTraceID
	spanID := globalDDSpanID
	if traceID == "" || spanID == "" {
		return nil, nil
	}
	parentContext := &SpanContext{}
	err := parentContext.ParseTraceID(traceID)
	if err != nil {
		return nil, err
	}
	err = parentContext.ParseSpanID(spanID)
	if err != nil {
		return nil, err
	}
	return parentContext, nil
}

func WithParentContext(c *SpanContext) ddtrace.StartSpanOption {
	return func(cfg *ddtrace.StartSpanConfig) {
		cfg.Parent = c
	}
}
