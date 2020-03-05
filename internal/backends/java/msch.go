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
	"net/http"
	"net/url"
)

const (
	mavenURL string = "http://search.maven.org/solrsearch/select?q="
)

type SearchDoc struct {
  Group    string `json:"g"`
  Artifact string `json:"a"`
  Version  string `json:"latestVersion"`
}

type SearchResult struct {
	Response struct {
		Docs []SearchDoc `json:"docs"`
	} `json:"response"`
}

func Search(keyword string) ([]SearchDoc, error) {
  searchUrl := mavenURL + url.QueryEscape(keyword)
	res, err := http.Get(searchUrl)
	if err != nil {
		return []SearchDoc{}, err
	}
	defer res.Body.Close()
	decoder := json.NewDecoder(res.Body)
	var searchResult SearchResult
	err = decoder.Decode(&searchResult)
	if err != nil {
		return []SearchDoc{}, err
	}
  return searchResult.Response.Docs, nil
}