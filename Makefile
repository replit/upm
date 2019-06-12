.PHONY: all
all: cmd/upm/upm

cmd/upm/upm: cmd/upm/*.go internal/*.go
	cd cmd/upm && go build

.PHONY: clean
clean:
	cd cmd/upm && go clean
