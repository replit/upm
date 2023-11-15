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

func MaybeTrace(serviceVersion string) func() {
	if os.Getenv("UPM_TRACE") != "1" {
		return nil
	}

	replid := os.Getenv("REPL_ID")
	if replid == "" {
		return nil
	}

	globalDDTraceID = os.Getenv("DD_TRACE_ID")
	globalDDSpanID = os.Getenv("DD_SPAN_ID")
	os.Unsetenv("DD_TRACE_ID")
	os.Unsetenv("DD_SPAN_ID")

	logger, err := NewDatadogLogger()
	if err != nil {
		return nil
	}

	rules := []tracer.SamplingRule{
		// send 100.00% of traces
		tracer.ServiceRule("upm", 1.0000),
	}
	tracer.Start()

	tracer.Start(
		tracer.WithService("upm"),
		tracer.WithGlobalTag("replid", replid),
		tracer.WithServiceVersion(serviceVersion),
		tracer.WithLogger(logger),
		tracer.WithSamplingRules(rules),
	)
	return func() {
		tracer.Stop()
		logger.Close()
	}
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
