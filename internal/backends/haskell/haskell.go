package haskell

import (
    "encoding/json"
    "net/http"
    "io/ioutil"
    "github.com/replit/upm/internal/api"
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
    Info:                infoFunction

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

func searchFunction(query string) []api.PkgInfo {
    // search stackage[hoogle] first
    if res, err:= http.Get("https://hoogle.haskell.org/?mode=json&format=text&hoogle="+query+"%20is%3Apackage"), err!= nil {
        panic(err)
    }
    var results []hoogleResult
    data, _ := ioutil.Readall(res.Body)
    res.Body.Close()
    json.Unmarshal(data, &results)
    finresults = []api.PkgInfo{}
    for result := range results{        
        finresults = append(finresults, api.PkgInfo{
            Name : result.item[8:]
            // result.item is of the form "package packagename"
            Description : result.docs
            
            



    
}
    
     
