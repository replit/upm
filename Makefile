.PHONY: all
upm: cmd/upm/upm ## Build the UPM binary

cmd/upm/upm: cmd/upm/*.go internal/*.go
	cd cmd/upm && go build

.PHONY:
docker: ## Run a shell with UPM inside Docker
	docker build . -t upm
	docker run -it --rm -v "$$PWD:/upm" upm sh -isc "source /upm/scripts/docker-env.sh"

.PHONY: clean
clean: ## Remove build artifacts
	cd cmd/upm && go clean

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST) | \
		sed 's/^/  make /'       | \
		sed 's/:[^#]*[#]# /|/'   | \
		sed 's/%/LANG/'          | \
		column -t -s'|' >&2
