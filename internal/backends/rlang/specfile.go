package rlang

import (
	"os"
	"io/ioutil"
	"encoding/json"
)

type RConfig struct {
	Packages []RPackage `json:"packages"`
}

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

func RAdd(pkg RPackage) {
	if file, err := os.Open("./Rconfig.json"); err == nil {
		var config RConfig
		
		decoder := json.NewDecoder(file)
		
		if err := decoder.Decode(&config); err != nil {
			panic(err)
		}

		if config.hasPackage(pkg) {
			return
		}

		file.Close()

		file, err := os.Create("./Rconfig.json")
		if err != nil {
			panic(err)
		}

		encoder := json.NewEncoder(file)
		encoder.SetIndent("", "\t")

		config.Pakcages = append(config.Packages, pkg)

		encoder.Encode(&config)

		file.Close()
	} else if os.IsNotExist(err) {
		file, err := os.Create("./Rconfig.json")
		if err != nil {
			panic(err)
		}

		file.WriteString("{\n\t\"packages\": []\n}")
		file.Close()
		
		RAdd(pkg)
	} else {
		panic(err)
	}
}

func RRemove(pkg RPackage) {
	file, err := os.Open("./Rconfig.json")
	if err != nil {
		panic(err)
	}

	var config RConfig

	decoder := json.NeeDecoder(file)

	if err := decoder.Decode(&config); err != nil {
		file.Close()
		panic(err)
	}

	if !config.hasPackage(pkg) {
		file.Close()
		return
	}

	file.Close()

	file, err := os.Create("./Rconfig.json")
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

	encoder.Encode(&config)

	file.Close()
}

func RLock() {
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
	
	contents, err := ioutil.ReadAll(config)
	if err != nil {
		panic(err)
	}

	if _, err := lock.Write(contents); err != nil {
		panic(err)
	}
}

func RGetSpecFile() RConfig {
	file, err := os.Open("./Rconfig.json")
	if err != nil {
		panic(err)
	}

	var config RConfig

	decoder := json.NewDecoder(file)

	defer file.Close()
	if err := decoder.Decode(&config); err != nil {
		panic(err)
	}

	return config
}

func RGetLockFile() RConfig {
	file, err := os.Open("./Rconfig.lock.json")
	if err != nil {
		panic(err)
	}

	var config RConfig

	decoder := json.NewDecoder(file)

	defer file.Close()
	if err := decoder.Decode(&config); err != nil {
		panic(err)
	}

	return config
}
