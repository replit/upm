.PHONY: upm
upm: cmd/upm/upm ## Build the UPM binary

SOURCES := $(shell find cmd internal -type d -o -name "*.go")

cmd/upm/upm: $(SOURCES)
	cd cmd/upm && go build

.PHONY: dev
dev: ## Run a shell with UPM source code and all package managers inside Docker
	docker build . -f Dockerfile.dev -t upm:dev
	docker run -it --rm -v "$$PWD:/upm" upm:dev

.PHONY: light
light: ## Build a Docker image with just the UPM binary
	docker build . -f Dockerfile.light -t upm:light

.PHONY: full
full: ## Build a Docker image with the UPM binary and all package managers
	docker build . -f Dockerfile.full -t upm:full

.PHONY: clean
clean: ## Remove build artifacts
	cd cmd/upm && go clean

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2
