// The MIT License (MIT)
//
// Copyright (c) 2016 Fredy Wijaya
// Search() function lightly edited by Dan Stowell for repl.it, February 2020
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

package java

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"
)

const (
	mavenURL string = "http://search.maven.org/solrsearch/select?q="
)

type SearchDoc struct {
	Group          string `json:"g"`
	Artifact       string `json:"a"`
	Version        string `json:"latestVersion"`
	PackageType    string `json:"p"`
	CurrentVersion string `json:"v"`
}

type SearchResult struct {
	Response struct {
		Docs []SearchDoc `json:"docs"`
	} `json:"response"`
}

func mavenSearch(searchURL string) ([]SearchDoc, error) {
	res, err := http.Get(searchURL)
	if err != nil {
		return []SearchDoc{}, err
	}
	defer res.Body.Close()

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		fmt.Printf("Could not read response\n")
		return []SearchDoc{}, err
	}

	var searchResult SearchResult
	if err := json.Unmarshal(body, &searchResult); err != nil {
		fmt.Printf("Failed to decode response %q\n", body)
		return []SearchDoc{}, err
	}

	return searchResult.Response.Docs, nil
}

func Search(keyword string) ([]SearchDoc, error) {
	searchURL := mavenURL + url.QueryEscape(keyword)

	return mavenSearch(searchURL)
}

func Info(name string) (SearchDoc, error) {
	parts := strings.Split(string(name), ":")

	var searchURL string
	if len(parts) >= 2 {
		searchURL = fmt.Sprintf("%sg:%s+AND+a:%s&core=gav", mavenURL, url.QueryEscape(fmt.Sprintf("%q", parts[0])), url.QueryEscape(fmt.Sprintf("%q", parts[1])))
	} else {
		searchURL = fmt.Sprintf("%sa:%s&core=gav", mavenURL, url.QueryEscape(fmt.Sprintf("%q", parts[0])))
	}

	docs, err := mavenSearch(searchURL)

	if err != nil {
		return SearchDoc{}, err
	}

	var latest = SearchDoc{}
	if len(docs) > 0 {
		latest = docs[0]
	}

	return latest, nil
}
