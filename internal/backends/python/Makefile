.PHONY: gen_pypi_map

gen_pypi_map:
	go run ./gen_pypi_map bq -gcp $(GCP_PROJECT_NAME)
	go run ./gen_pypi_map test -force
	go run ./gen_pypi_map updatepkgs
	go run ./gen_pypi_map gen
