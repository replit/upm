package main

import (
  "fmt"
  "sync"
  "sort"
  "net/http"
  "encoding/json"
)

type PackageInfo struct {
  Name string       `json:"name"`
  Downloads int     `json:"downloads,string"`
  Version string

  // Specific to the dist we use to get modules from
  Modules []string
  MD5  string  `json:"md5_digest"`
}

type PackageURL struct {
  Filename string
  URL      string
  PackageType string
  UploadTime  string
  Size int64
  MD5  string  `json:"md5_digest"`
}

type PackageData struct {
  Info     PackageInfo
  Releases map[string] []PackageURL
}

func processPackage(packageName string) (PackageData, error) {
  resp, err := http.Get("https://pypi.org/pypi/" + packageName + "/json")
  if err != nil {
    return PackageData{}, err
  }
  defer resp.Body.Close()

  decoder := json.NewDecoder(resp.Body)

  data := PackageData{}
  decoder.Decode(&data)

  return data, nil
}

func main() {
  // Load info from the package cache
  packageCache := LoadJson("bq.json")

  // Scan pypi for all packages
  discoveredPackages := 0
  packages, _ := NewPackageIndex("https://pypi.org/simple/")
  //packages := FakePackageIndex("numpy", "python-twitter", "aa-stripe")

  // Each package is handled in a seperate goroutine, max 300 at once
  infoQueue := make(chan PackageInfo)
  concurrencyLimiter := make(chan struct{}, 300)
  var wg sync.WaitGroup

  for packages.Next() {
    discoveredPackages++
    packageName := packages.Package()

    go func() {
      // Register with the wait group
      wg.Add(1)
      defer wg.Done()

      // Push to the limiter channel and pop when done
      concurrencyLimiter <- struct{}{}
      defer func(){<-concurrencyLimiter}()

      // Get the package metadata from pypi
      packageData, err := processPackage(packageName)

      if err != nil {
        fmt.Println("Encountered error while fetching", packageName, err)
        return
      }

      // Determine which dist we want to use to determine modules
      latest := packageData.Releases[packageData.Info.Version]
      if len(latest) == 0 {
        fmt.Println(packageName, ":No packages for latest release")
      }

      distPriorities := map[string]int{
        "bdist_wheel": 2,
        "sdist": 1,
      }

      // Sort the releases by priority we want to parse
      sort.Slice(latest, func(a, b int) bool {
        return distPriorities[latest[a].PackageType] < distPriorities[latest[b].PackageType]
      })

      // Check if module extraction is out of date
      if latest[0].MD5 != packageCache[packageName].MD5 {

        // Download the distribution and extract the modules
        modules, err := GetModules(latest[0])
        if err != nil {
          fmt.Println("Encounter error while resolving packages for", packageName, err)
          return
        }

        packageData.Info.Modules = modules
        packageData.Info.MD5 = latest[0].MD5
      }

      infoQueue <- packageData.Info
    }()
  }

  processedPackages := 0
  for info := range infoQueue {
    processedPackages++
    if processedPackages % 10 == 0 {
      //fmt.Println(processedPackages, "/", discoveredPackages)
    }
    fmt.Println(info)

    // If we've processed everything, close the channel
    if processedPackages == discoveredPackages {
      close(infoQueue)
    }
  }

  //processPackage("python-twitter")
  //processPackage("numpy-quaternion")
  //processPackage("numpy")

}

