.PHONY: all
upm: cmd/upm/upm ## Build the UPM binary

cmd/upm/upm: cmd/upm/*.go internal/*.go
	cd cmd/upm && go build

.PHONY: docker
docker: ## Run a shell with UPM inside Docker
	docker build . -f Dockerfile.dev -t upm-dev
	docker run -it --rm -v "$$PWD:/upm" upm-dev sh -isc "source /upm/scripts/docker-env.sh"

.PHONY: docker-bin
docker-bin: ## Build a Docker image with just the UPM binary
	docker build . -f Dockerfile.bin -t upm

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
