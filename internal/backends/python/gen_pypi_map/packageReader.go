package main

import (
  "fmt"
  "errors"
  "strings"
  "net/http"
  "bufio"
  "path"
  "io"
)

func parseTopLevel(reader io.Reader) []string {
  modules := make([]string, 0)

  scanner := bufio.NewScanner(reader)
  scanner.Split(bufio.ScanLines)
  for scanner.Scan() {
    modules = append(modules, scanner.Text())
  }

  return modules
}


func extractWheel(reader ArchiveReader) ([]string, error) {
  packages := []string{}
  for {
    name, err := reader.Next()

    // Handle errors and EOF
    if err != nil {
      if err == io.EOF {
        // If we never found any modules, error
        if len(packages) == 0 {
          return nil, errors.New("No top level modules found")
        }

        return packages, nil
      }

      return nil, err
    }

    // top_level.txt contains a list of all the top level modules of a package
    if path.Base(name) == "top_level.txt" {
      toplevel, err := reader.Reader()
      if err != nil {
        return nil, err
      }

      return parseTopLevel(toplevel), nil
    }

    // In wheels, the packages are in the root of the archive
    if path.Base(name) == "__init__.py" {
      packageDir := path.Dir(name)
      components := strings.Split(packageDir, "/")
      if len(components) == 1 {
        packages = append(packages, components[0])
      }
    }
  }
}

func extractSdist(reader ArchiveReader) ([]string, error) {
  packages := []string{}
  for {
    name, err := reader.Next()

    // Handle tar reader errors
    if err != nil {
      if err == io.EOF {
        // We made it to the end of the package without finding a manifest,
        // guess from the package structure
        //return nil, errors.New("EOF reached without top_level.txt")
        if len(packages) == 0 {
          return nil, errors.New("No top level modules found")
        }

        return packages, nil
      } 

      return nil, err
    }

    // top_level.txt is a python package distribution file that contains a list
    // of the top level modules defined in a package
    if path.Base(name) == "top_level.txt" {
      toplevel, err := reader.Reader()
      if err != nil {
        return nil, err
      }

      return parseTopLevel(toplevel), nil
    }

    // In sdist, package is in a folder with the name of the distribution
    if path.Base(name) == "__init__.py" {
      packageDir := path.Dir(name)
      components := strings.Split(packageDir, "/")
      if len(components) == 2 {
        packages = append(packages, components[1])
      }
    }
  }
}

func GetModules(pkg PackageURL) ([]string, error) {
  // Get the package file
  resp, err := http.Get(pkg.URL)
  if err != nil {
    return nil, err
  }
  defer resp.Body.Close()

  // Get the right archive reader
  var reader ArchiveReader
  if path.Ext(pkg.Filename) == ".zip" {
    reader, err = MakeZipReader(resp.Body, pkg.Size)
    if err != nil {
      return nil, err
    }
  } else if path.Ext(pkg.Filename) == ".whl" {
    reader, err = MakeZipReader(resp.Body, pkg.Size)
    if err != nil {
      return nil, err
    }
  } else if path.Ext(pkg.Filename) == ".gz" {
    reader, err = MakeTarballReader(resp.Body)
    if err != nil {
      return nil, err
    }
  } else {
    return nil, fmt.Errorf("Unknown file type: %v", path.Ext(pkg.Filename))
  }

  if pkg.PackageType == "bdist_wheel" {
    return extractWheel(reader)
  } else if pkg.PackageType == "bdist_egg" {
    return extractWheel(reader)
  } else if pkg.PackageType == "sdist" {
    return extractSdist(reader)
  }

  return nil, fmt.Errorf("Unknown package type: %v", pkg.PackageType)
}
