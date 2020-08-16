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
)

var HaskellBackend = api.LanguageBackend {
    Name:                "haskell-stack",
    Specfile:            "package.yaml",
    Lockfile:            "stack.yaml.lock",
    // also present: a .cabal and stack.yaml
    FilenamePatterns:    []string{"*.hs"},
    Quirks:              api.QuirksLockAlsoInstalls,
    GetPackageDir:       func () string {
        return ".stack-work"
    },
    // note : Stack actually uses two package directories:
    // ~/.stack/ for stackage packages and .stack-work for others
    Search:              func(q string) []api.PkgInfo {
        return searchFunction(q,false)
    },
    Info:                func(i api.PkgName) api.PkgInfo {
        return searchFunction(string(i),true)[0]
    },
    Add:                 func(map[api.PkgName]api.PkgSpec, string){},
    Remove:              func(map[api.PkgName]bool) {},
    Lock:                util.NotImplemented,
    Install:             util.NotImplemented,
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
    return finresults
}



    
     
