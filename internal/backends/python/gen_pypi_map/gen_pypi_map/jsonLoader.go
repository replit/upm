package main

import (
  "fmt"
  "encoding/json"
  "os"
)

func LoadJson(path string) map[string]PackageInfo{
  packages := map[string]PackageInfo{}

  file, err := os.Open(path)
  if err != nil {
    fmt.Println(err)
  }

  var info PackageInfo;
  decoder := json.NewDecoder(file)
  for decoder.More() {
    decoder.Decode(&info)
    packages[info.Name] = info
  }

  return packages
}

