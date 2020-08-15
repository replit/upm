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
    FileNamePatterns:    []string{"*.hs"},
    Quirks:              api.QuirksLockAlsoInstalls,
    GetPackageDir:       func () string {return ".stack-work"},
    // note : Stack actually uses two package directories:
    // ~/.stack/ for stackage packages and .stack-work for others
    Search:              searchFunction,
    Info:                infoFunction,
    


}

type hoogleModuleOrPackage struct{
    url string
    name string
}
type hoogleResult struct{
    url string
    module hoogleModuleOrPackage
    hpackage hoogleModuleOrPackage `json:"package"`
    // because package is a keyword
    item string
    htype string `json:"type"`
    // type is also a keyword
    docs string
}

func searchFunction(query string, indiv bool = false) []api.PkgInfo {
    // search stackage[hoogle] first
    res, err:= http.Get("https://hoogle.haskell.org/?mode=json&format=text&hoogle="+url.QueryEscape(query)+"%20is%3Apackage")
    if err!= nil {
        util.Die("hoogle response:"+ err.Error())
    }
    var results []hoogleResult
    data, _ := ioutil.Readall(res.Body)
    res.Body.Close()
    json.Unmarshal(data, &results)
    finresults = []api.PkgInfo{}
    if indiv {results = [results[0]]} // so we can skip implementing the search function again
    for result := range results{
        getCabal, err := http.Get(result.url + "/src/" + result.item[8:] + ".cabal")
        if err!= nil {
            util.Die("hackage [cabal file] response:" + err.Error())
        }
        cabal, _ := ioutil.ReadAll(getCabal.Body)
        cabalRegexp := regexp.MustCompile(`\n(\S+): +(.?)\n`)
        cabalDataSlice := cabalRegexp.FindAllSubmatch(cabal, -1)
        cabalData := make(map[string]string)
        for s := range cabalDataSlice{
            cabalData[string(s[0])] = string(s[1])
        }
        var info api.PkgInfo{
            Name:           result.item[8:]
            Description:    strings.ReplaceAll(result.docs, "\n", " ")
            Version:        cabalData["version"]
            HomePageURL:    result.url 
            SoourceCodeURL: cabalData["homepage"] //yes, homepage tends to be the github page 
            BugTrackerURL:  cabalData["bug-reports"]
            Author:         cabalData["author"]
            License:        cabalData["license"]
            // will implement Dependencies later
        }
        finresults = append(finresults,info)
    }
    return finresults
}

InfoFunction = func(name string) {return searchFunction(name, indiv=true)[0]}

    
     
