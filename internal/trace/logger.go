package trace

import "os"

type DatadogLogger struct {
	file *os.File
}

func NewDatadogLogger() (*DatadogLogger, error) {
	file, err := os.Create("/tmp/upm.dd.log")
	if err != nil {
		return nil, err
	}

	return &DatadogLogger{
		file: file,
	}, nil
}

func (l *DatadogLogger) Log(msg string) {
	l.file.WriteString(msg)
	l.file.WriteString("\n")
}

func (l *DatadogLogger) Close() {
	l.file.Close()
}
