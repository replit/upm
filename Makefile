.PHONY: upm
upm: cmd/upm/upm ## Build the UPM binary

SOURCES := $(shell find cmd internal -type d -o -name "*.go")
RESOURCES := $(shell find resources)

cmd/upm/upm: $(SOURCES) $(RESOURCES)
	go run github.com/rakyll/statik -src resources -dest internal -f
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

.PHONY: doc
doc: ## Open Godoc in web browser
	docker build . -f Dockerfile.godoc -t upm:godoc
	@echo
	@echo "starting godoc; your browser will be opened once it is ready" >&2
	@scripts/browse-godoc.bash &
	@docker run -it --rm -p 6060:6060 \
		-v "$$PWD:/tmp/go/src/github.com/replit/upm" \
		upm:godoc

.PHONY: deploy
deploy: light full ## Publish UPM Docker images to Docker Hub
	docker tag upm:light replco/upm:light
	docker tag upm:full replco/upm:full
	docker push replco/upm:light
	docker push replco/upm:full

.PHONY: clean
clean: ## Remove build artifacts
	rm -rf cmd/upm/upm internal/statik

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2
