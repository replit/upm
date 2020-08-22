package haskell

import (
    "encoding/json"
    "net/http"
    "net/url"
    "io/ioutil"
    "regexp"
    "github.com/replit/upm/internal/api"
    "github.com/replit/upm/internal/util"
    "strings"
    "os"
)

var HaskellBackend = api.LanguageBackend {
    Name:                "haskell-stack",
    Specfile:            "package.yaml",
    Lockfile:            "stack.yaml",
    // also present: a package-name.cabal file, a stack.yaml.lock that stores hashes
    FilenamePatterns:    []string{"*.hs"},
    Quirks:              api.QuirksAddRemoveAlsoLocks,
    GetPackageDir:       func () string {
        return ".stack-work"
    },
    // Stack actually uses two package directories:
    // ~/.stack/ for stackage packages and .stack-work for others
    Search:              func(q string) []api.PkgInfo {
        return searchFunction(q,false)
    },
    Info:                func(i api.PkgName) api.PkgInfo {
        return searchFunction(string(i),true)[0]
    },
    Add:                 Add,
    Remove:              func(map[api.PkgName]bool) {},
    // Stack's baseline philosophy is that build plans are always reproducible.
    // Which means locking is automatic. Versions are precise. This spares a lot of spec trouble.
    // https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/
    Lock:                func(){},
    Install:             util.NotImplemented,
    // lock will be used for non-stackage packages and spec for stackage ones
    ListSpecfile:        func()map[api.PkgName]api.PkgSpec{
        return map[api.PkgName]api.PkgSpec{}
    },
    ListLockfile:        func()map[api.PkgName]api.PkgVersion{
        return map[api.PkgName]api.PkgVersion{}
    },
    Guess:               func()(map[api.PkgName]bool,bool){
        return map[api.PkgName]bool{},false
    },

    
}

func searchFunction(query string, indiv bool) []api.PkgInfo {
    // search stackage[hoogle] first
    res, err:= http.Get("https://hoogle.haskell.org/?mode=json&format=text&hoogle="+url.QueryEscape(query)+"%20is%3Apackage")
    if err!= nil {
        util.Die("hoogle response:"+ err.Error())
    }
    var results = []map[string]string{}
    data, _ := ioutil.ReadAll(res.Body)
    res.Body.Close()
    json.Unmarshal(data, &results)
    finresults := []api.PkgInfo{}
    if indiv {results = results[:1]} // so we can skip implementing the search function again
    for _,result := range results{
        if result["item"] == ""{
            continue
        }
        getCabal, err := http.Get(result["url"] + "/src/" + result["item"][8:] + ".cabal")
        if err!= nil {
            util.Die("hackage [cabal file] response:" + err.Error())
        }
        cabal, _ := ioutil.ReadAll(getCabal.Body)
        getCabal.Body.Close()
        cabalRegexp := regexp.MustCompile(`(\S+): +(.+)`)
        cabalDataSlice := cabalRegexp.FindAllSubmatch(cabal, -1)
        cabalData := make(map[string]string)
        for _,s := range cabalDataSlice{
            cabalData[string(s[1])] = string(s[2])
        }
        info := api.PkgInfo{
            Name:           result["item"][8:],
            Description:    strings.ReplaceAll(result["docs"], "\n", ""),
            Version:        cabalData["version"],
            HomepageURL:    result["url"] ,
            SourceCodeURL:  cabalData["homepage"], 
            // homepage tends to be the github page 
            BugTrackerURL:  cabalData["bug-reports"],
            Author:         cabalData["author"],
            // sometimes this is author <email>
            // other times it's just author and the email is in cabalData["maintainer"]
            // possible todo ^^
            License:        cabalData["license"],
            // will implement Dependencies later
        }
        finresults = append(finresults,info)
    }
    if indiv && (len(finresults) == 0 || finresults[0].Name != query){
        util.Die("cannot find package " + query)
    }
    return finresults
}


var initialCabal = 
`name:                project
version:             0.0.0
build-type:          Simple
cabal-version:       >=1.10

executable main
  hs-source-dirs:      .
  main-is:             main.hs
  default-language:    Haskell2010
  build-depends:       
    base >= 4.7 && < 5
`

//TODO: more sophisticated cabal/yaml parsing
func Add(packages map[api.PkgName]api.PkgSpec, projectName string){
    if _,err := os.Open("./project.cabal"); os.IsNotExist(err) {
        file, _ := os.Create("./project.cabal")
        file.Write([]byte(initialCabal))
        file.Close()
        file, _ = os.Create("./stack.yaml")
        file.Write([]byte("resolver: lts-16.10\n"))
        file.Close()
    }
    for name, version := range(packages){
        packageInfo := searchFunction(string(name),true)[0]
        onStackage := !strings.Contains(string(packageInfo.Description), "Not on Stackage")
        if !onStackage{
            if contents,_ := ioutil.ReadFile("stack.yaml"); !strings.Contains(string(contents),"extra-deps"){
                f, _ := os.OpenFile("stack.yaml", os.O_APPEND | os.O_WRONLY, 0644)
                f.Write([]byte("extra-deps:\n"))
                f.Close()
            }
            if version == ""{
                version = api.PkgSpec(packageInfo.Version)
            }
            f, _ := os.OpenFile("stack.yaml", os.O_APPEND | os.O_WRONLY, 0644)
            f.WriteString("- " + string(name) + "-" + string(version) + "\n")
            f.Close()     
        } 
        f, _ := os.OpenFile("project.cabal", os.O_APPEND | os.O_WRONLY, 0644)
        f.WriteString("    , " + string(name) + "\n" )
        f.Close() 
    }
}
    
     
