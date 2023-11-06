/*
This SpanContext implements
[ddtrace.SpanContextW3C](https://github.com/DataDog/dd-trace-go/blob/49273dfcc871f3329614c40cd98116b6d44cdb12/ddtrace/ddtrace.go#L25)
, implementation is mostly based on
https://github.com/DataDog/dd-trace-go/blob/49273dfcc871f3329614c40cd98116b6d44cdb12/ddtrace/tracer/spancontext.go#L85

We have our own version because that one was made private.
*/

package trace

import (
	"encoding/binary"
	"encoding/hex"
	"errors"
	"fmt"
	"strconv"
	"strings"

	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace"
)

type SpanContext struct {
	traceID traceID
	spanID  uint64
}

type traceID [16]byte // traceID in big endian, i.e. <upper><lower>

var ErrSpanContextCorrupted = errors.New("span context corrupted")

func (c *SpanContext) TraceID128() string {
	return c.traceID.HexEncoded()
}

func (c *SpanContext) TraceID128Bytes() [16]byte {
	return c.traceID
}

func (c *SpanContext) TraceID() uint64 {
	return c.traceID.Lower()
}

func (c *SpanContext) SpanID() uint64 {
	return c.spanID
}

func (c *SpanContext) ForeachBaggageItem(handler func(k, v string) bool) {
}

func GetHexSpanID(c ddtrace.SpanContextW3C) string {
	return fmt.Sprintf("%016x", c.SpanID())
}

func GetHexTraceID(c ddtrace.SpanContextW3C) string {
	return fmt.Sprintf("%032x", c.TraceID128Bytes())
}

func (c *SpanContext) ParseTraceID(v string) error {
	if len(v) > 32 {
		v = v[len(v)-32:]
	}
	v = strings.TrimLeft(v, "0")
	var err error
	if len(v) <= 16 { // 64-bit trace id
		var tid uint64
		tid, err = strconv.ParseUint(v, 16, 64)
		c.traceID.SetLower(tid)
	} else { // 128-bit trace id
		idUpper := v[:len(v)-16]
		//nolint:ineffassign
		c.traceID.SetUpperFromHex(idUpper)
		var l uint64
		l, err = strconv.ParseUint(v[len(idUpper):], 16, 64)
		c.traceID.SetLower(l)
	}
	if err != nil {
		return ErrSpanContextCorrupted
	}
	return nil
}

func (c *SpanContext) ParseSpanID(v string) error {
	var err error
	c.spanID, err = strconv.ParseUint(v, 16, 64)
	if err != nil {
		return err
	}
	return nil
}

func (t *traceID) HexEncoded() string {
	return hex.EncodeToString(t[:])
}

func (t *traceID) Lower() uint64 {
	return binary.BigEndian.Uint64(t[8:])
}

func (t *traceID) Upper() uint64 {
	return binary.BigEndian.Uint64(t[:8])
}

func (t *traceID) SetLower(i uint64) {
	binary.BigEndian.PutUint64(t[8:], i)
}

func (t *traceID) SetUpper(i uint64) {
	binary.BigEndian.PutUint64(t[:8], i)
}

func (t *traceID) SetUpperFromHex(s string) error {
	u, err := strconv.ParseUint(s, 16, 64)
	if err != nil {
		return fmt.Errorf("malformed %q: %s", s, err)
	}
	t.SetUpper(u)
	return nil
}
