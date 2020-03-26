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
	if file, err := os.Open("~/Rconfig.json"); err == nil {
		var config RConfig
		
		decoder := json.NewDecoder(file)
		
		if err := decoder.Decode(&config); err == nil {
			if config.hasPackage(pkg) {
				return
			}

			file.Close()
			
			if file, err = os.Create("~/Rconfig.json"); err == nil {
				encoder := json.NewEncoder(file)
				encoder.SetIndent("", "\t")

				config.Packages = append(config.Packages, pkg)

				encoder.Encode(&config)

				file.Close()
			} else {
				panic(err)
			}
		} else {
			file.Close()
			panic(err)
		}
	} else if os.IsNotExist(err) {
		if file, err := os.Create("~/Rconfig.json"); err == nil {
			file.WriteString("{\n\t\"packages\": []\n}")
			file.Close()
			
			RAdd(pkg)
		} else {
			panic(err)
		}
	} else {
		panic(err)
	}
}

func RRemove(pkg RPackage) {
	if file, err := os.Open("~/Rconfig.json"); err == nil {
		var config RConfig

		decoder := json.NewDecoder(file)
		
		if err := decoder.Decode(&config); err == nil {
			if !config.hasPackage(pkg) {
				return
			}

			file.Close()
			
			if file, err = os.Create("~/Rconfig.json"); err == nil {
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
			} else {
				panic(err)
			}
		} else {
			file.Close()
			panic(err)
		}
	} else {
		panic(err)
	}
}

func RLock() {
	if lock, err := os.Create("~/Rconfig.json.lock"); err == nil {
		defer lock.Close()

		if config, err := os.Open("~/Rconfig.json"); err == nil {
			defer config.Close()

			if contents, err := ioutil.ReadAll(config); err == nil {
				if _, err := lock.Write(contents); err != nil {
					panic(err)
				}
			} else {
				panic(err)
			}
		} else {
			panic(err)
		}
	} else {
		panic(err)
	}
}

func RGetSpecFile() RConfig {
	if file, err := os.Open("~/Rconfig.json"); err == nil {
		var config RConfig

		decoder := json.NewDecoder(file)
		
		if err := decoder.Decode(&config); err == nil {
			file.Close()
			return config
		} else {
			file.Close()
			panic(err)
		}
	} else {
		panic(err)
	}
}

func RGetLockFile() RConfig {
	if file, err := os.Open("~/Rconfig.json.lock"); err == nil {
		var config RConfig

		decoder := json.NewDecoder(file)
		
		if err := decoder.Decode(&config); err == nil {
			file.Close()
			return config
		} else {
			file.Close()
			panic(err)
		}
	} else {
		panic(err)
	}
}
