package dotnet

import (
	"encoding/json"
	"encoding/xml"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// Used the documentation provided here https://docs.microsoft.com/en-us/nuget/api/overview

// nuget.org info lookup result
type infoResult struct {
	Versions []string `json:"versions"`
}

// nuget.org .nuspec file package repository data
type repository struct {
	XMLName xml.Name `xml:"repository"`
	Type    string   `xml:"type,attr"`
	URL     string   `xml:"url,attr"`
	Commit  string   `xml:"commit,attr"`
}

// nuget.org .nuspec file package metadata
type packageMetadata struct {
	XMLName     xml.Name   `xml:"metadata"`
	ID          string     `xml:"id"`
	Version     string     `xml:"version"`
	Title       string     `xml:"title"`
	Author      string     `xml:"author"`
	Description string     `xml:"description"`
	License     string     `xml:"license"`
	Repository  repository `xml:"repository"`
	ProjectURL  string     `xml:"projectUrl"`
}

// nuget.org .nuspec file data
type nugetPackage struct {
	XMLName  xml.Name        `xml:"package"`
	Metadata packageMetadata `xml:"metadata"`
}

// nuget.org search service result entry
type searchResultData struct {
	ID          string
	Version     string
	Description string
	ProjectURL  string
}

// nuget.org search service result record
type searchResult struct {
	TotalHits int
	Data      []searchResultData
}

const searchQueryURL = "https://azuresearch-usnc.nuget.org/query"

// find the first ten projects that match the query string on nuget.org
func search(query string) []api.PkgInfo {
	pkgs := []api.PkgInfo{}
	queryURL := fmt.Sprintf("%s?q=%s&take=10", searchQueryURL, url.QueryEscape(query))

	res, err := http.Get(queryURL)
	if err != nil {
		util.Die("failed to query for packages: %s", err)
	}
	defer res.Body.Close()

	if res.StatusCode != http.StatusOK {
		return pkgs
	}

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		util.Die("Could not read response: %s", err)
	}

	var searchResult searchResult
	err = json.Unmarshal(body, &searchResult)
	if err != nil {
		util.Die("Could not unmarshal response data: %s", err)
	}

	for _, data := range searchResult.Data {
		pkgs = append(pkgs, api.PkgInfo{
			Name:          data.ID,
			Version:       data.Version,
			Description:   data.Description,
			SourceCodeURL: data.ProjectURL,
		})
	}

	return pkgs
}

// looks up all the versions of the package and gets retails for the latest version from nuget.org
func info(pkgName api.PkgName) api.PkgInfo {
	lowID := url.PathEscape(strings.ToLower(string(pkgName)))
	infoURL := fmt.Sprintf("https://api.nuget.org/v3-flatcontainer/%s/index.json", lowID)

	res, err := http.Get(infoURL)
	if err != nil {
		util.Die("failed to get the versions: %s", err)
	}
	defer res.Body.Close()
	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		util.Die("could not read response: %s", err)
	}
	var infoResult infoResult
	err = json.Unmarshal(body, &infoResult)
	if err != nil {
		util.Die("could not read json body: %s", err)
	}
	latestVersion := infoResult.Versions[len(infoResult.Versions)-1]
	util.ProgressMsg(fmt.Sprintf("latest version of %s is %s", pkgName, latestVersion))
	specURL := fmt.Sprintf("https://api.nuget.org/v3-flatcontainer/%s/%s/%s.nuspec", lowID, url.PathEscape(latestVersion), lowID)
	util.ProgressMsg(fmt.Sprintf("Getting spec from %s", specURL))
	res, err = http.Get(specURL)
	if err != nil {
		util.Die("failed to get the spec: %s", err)
	}
	defer res.Body.Close()
	body, err = ioutil.ReadAll(res.Body)
	if err != nil {
		util.Die("could not read response: %s", err)
	}
	var nugetPackage nugetPackage
	err = xml.Unmarshal(body, &nugetPackage)
	if err != nil {
		util.Die(fmt.Sprintf("failed to read spec %s", err))
	}

	pkgInfo := api.PkgInfo{
		Name:          nugetPackage.Metadata.ID,
		Version:       nugetPackage.Metadata.Version,
		Description:   nugetPackage.Metadata.Description,
		Author:        nugetPackage.Metadata.Author,
		License:       nugetPackage.Metadata.License,
		SourceCodeURL: nugetPackage.Metadata.Repository.URL,
		HomepageURL:   nugetPackage.Metadata.ProjectURL,
	}
	return pkgInfo
}
