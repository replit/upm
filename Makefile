SOURCES := $(shell find cmd internal -type d -o -name "*.go")
RESOURCES := $(shell find resources)
GENERATED := internal/backends/python/pypi_map.sqlite
LD_FLAGS := "-X 'github.com/replit/upm/internal/cli.version=$${VERSION:-development version}'"

export GO111MODULE=on

.PHONY: upm
upm: cmd/upm/upm ## Build the UPM binary

install: cmd/upm/upm
	go install ./cmd/upm

internal/backends/python/pypi_map.sqlite: internal/backends/python/download_stats.json
	cd internal/backends/python; go run ./gen_pypi_map -bq download_stats.json -pkg python -out pypi_map.sqlite -cache cache -cmd gen

.PHONY: generated
generated: internal/statik/statik.go $(GENERATED)

cmd/upm/upm: $(SOURCES) $(RESOURCES) generated
	cd cmd/upm && go build -ldflags $(LD_FLAGS)

build-release: $(SOURCES) $(RESOURCES) generated
	goreleaser build

internal/statik/statik.go: $(shell find resources -type f)
	go run github.com/rakyll/statik -src resources -dest internal -f

clean-gen:
	rm -f $(GENERATED)
	rm -rf internal/statik

.PHONY: dev
dev: ## Run a shell with UPM source code and all package managers inside Docker
	docker build . -f Dockerfile.dev -t upm:dev
	docker run -it --rm -v "$$PWD:/upm" upm:dev

.PHONY: light
light: ## Build a Docker image with just the UPM binary
	docker build . -f Dockerfile.light -t upm:light --build-arg VERSION

.PHONY: full
full: ## Build a Docker image with the UPM binary and all package managers
	docker build . -f Dockerfile.full -t upm:full --build-arg VERSION

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
deploy: light full ## Publish UPM snapshot Docker images to Docker Hub
	docker tag upm:light replco/upm:light
	docker tag upm:full replco/upm:full
	docker tag upm:full replco/upm:latest
	docker push replco/upm:light
	docker push replco/upm:full
	docker push replco/upm:latest

.PHONY: pkgbuild
pkgbuild: ## Update and test PKGBUILD
	git clean -fdX packaging/aur
	docker build . -f Dockerfile.arch -t upm:arch
	docker run -it --rm -v "$$PWD/packaging/aur:/upm" upm:arch \
		/tmp/update-pkgbuild.bash

.PHONY: clean
clean: clean-gen ## Remove build artifacts
	rm -rf cmd/upm/upm dist

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2

.PHONY: test
test: internal/statik/statik.go ## Run the tests
	go test ./... -v