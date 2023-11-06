package rlang

import (
	"context"
	"encoding/json"
	"io"
	"os"

	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

// RConfig represents the JSON structure of the package manager file
type RConfig struct {
	Packages []RPackage `json:"packages"`
}

// RPackage represents the JSON structure of a package dependency
type RPackage struct {
	Name    string `json:"name"`
	Version string `json:"version,omitempty"`
}

func (config RConfig) hasPackage(pkg RPackage) bool {
	for _, installed := range config.Packages {
		if installed.Name == pkg.Name {
			return true
		}
	}

	return false
}

// RAdd adds an external package dependency
func RAdd(ctx context.Context, pkg RPackage) {
	span, ctx := tracer.StartSpanFromContext(ctx, "RAdd")
	defer span.Finish()
	if file, err := os.Open("./Rconfig.json"); err == nil {
		var config RConfig

		decoder := json.NewDecoder(file)

		if err = decoder.Decode(&config); err != nil {
			panic(err)
		}

		if config.hasPackage(pkg) {
			return
		}

		file.Close()

		file, err = os.Create("./Rconfig.json")
		if err != nil {
			panic(err)
		}

		encoder := json.NewEncoder(file)
		encoder.SetIndent("", "\t")

		config.Packages = append(config.Packages, pkg)

		err = encoder.Encode(&config)
		if err != nil {
			panic(err)
		}

		file.Close()
	} else if os.IsNotExist(err) {
		file, err := os.Create("./Rconfig.json")
		if err != nil {
			panic(err)
		}

		_, err = file.WriteString("{\n\t\"packages\": []\n}")
		if err != nil {
			panic(err)
		}

		file.Close()

		RAdd(ctx, pkg)
	} else {
		panic(err)
	}
}

// RRemove removes an extenal package dependency
func RRemove(ctx context.Context, pkg RPackage) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "RRemove")
	defer span.Finish()
	file, err := os.Open("./Rconfig.json")
	if err != nil {
		panic(err)
	}

	var config RConfig

	decoder := json.NewDecoder(file)

	if err = decoder.Decode(&config); err != nil {
		file.Close()
		panic(err)
	}

	if !config.hasPackage(pkg) {
		file.Close()
		return
	}

	file.Close()

	file, err = os.Create("./Rconfig.json")
	if err != nil {
		panic(err)
	}

	encoder := json.NewEncoder(file)
	encoder.SetIndent("", "\t")

	for index, installed := range config.Packages {
		if installed.Name == pkg.Name {
			config.Packages = append(config.Packages[:index], config.Packages[index+1:]...)
			break
		}
	}

	err = encoder.Encode(&config)
	if err != nil {
		panic(err)
	}

	file.Close()
}

// RLock backs up the contents of the spec file to the lock file
func RLock(ctx context.Context) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "RLock")
	defer span.Finish()
	lock, err := os.Create("./Rconfig.lock.json")
	if err != nil {
		panic(err)
	}

	defer lock.Close()

	config, err := os.Open("./Rconfig.json")
	if err != nil {
		panic(err)
	}

	defer config.Close()

	contents, err := io.ReadAll(config)
	if err != nil {
		panic(err)
	}

	if _, err := lock.Write(contents); err != nil {
		panic(err)
	}
}

// RGetSpecFile gets the contents of the spec file
func RGetSpecFile() RConfig {
	file, err := os.Open("./Rconfig.json")
	if err != nil {
		panic(err)
	}

	var config RConfig

	decoder := json.NewDecoder(file)

	defer file.Close()
	if err = decoder.Decode(&config); err != nil {
		panic(err)
	}

	return config
}

// RGetLockFile gets the contents of the lock file
func RGetLockFile() RConfig {
	file, err := os.Open("./Rconfig.lock.json")
	if err != nil {
		panic(err)
	}

	var config RConfig

	decoder := json.NewDecoder(file)

	defer file.Close()
	if err = decoder.Decode(&config); err != nil {
		panic(err)
	}

	return config
}
