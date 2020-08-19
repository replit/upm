package main

import (
  "fmt"
  "encoding/json"
  "os"
)

func LoadCache(path string) (PackageCache, error) {
  packages := map[string]PackageInfo{}

  file, err := os.Open(path)
  if err != nil {
    return packages, fmt.Errorf("Could not open cache: %v", err)
  }

  decoder := json.NewDecoder(file)
  for decoder.More() {
    var info PackageInfo;
    decoder.Decode(&info)
    packages[info.Name] = info
  }

  return packages, nil
}

func MergeCache(baseCache PackageCache, caches... PackageCache) PackageCache {
  for _, cache := range caches {
    for packageName, packageInfo := range cache {
      if _, ok := baseCache[packageName]; ok {
        info := baseCache[packageName]
        //fmt.Println("Merging", packageInfo, "into", info)
        info.Downloads = packageInfo.Downloads
        baseCache[packageName] = info
      } else {
        //fmt.Println("Inserting", packageInfo, "into cache")
        baseCache[packageName] = packageInfo
      }
    }
  }
  return baseCache
}

