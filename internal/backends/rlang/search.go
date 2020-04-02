package rlang

import (
	"strings"
	"strconv"
	"sort"
	"net/http"
	"net/url"
	"encoding/json"
)

type CranHitSource struct {
	Date             string `json:"date"`
	Author           string `json:"Author"`
	MD5sum           string `json:"MD5sum"`
	NeedsCompilation string `json:"NeedsCompilation"`
	URL              string `json:"URL"`
	Title            string `json:"Title"`
	Packaged         string `json:"Packaged"`
	Encoding         string `json:"Encoding"`
	DatePublished    string `json:"Date/Publication"`
	Version          string `json:"Version"`
	Maintainer       string `json:"Maintainer"`
	Enhances         string `json:"Enhances"`
	Imports          string `json:"Imports"`
	Authors          string `json:"Authors@R"`
	RevDeps          int    `json:"revdeps"`
	BugReports       string `json:"BugReports"`
	Suggests         string `json:"Suggests"`
	Depends          string `json:"Depends"`
	LinkingTo        string `json:"LinkingTo"`
	Package          string `json:"Package"`
	License          string `json:"License"`
	CranDBFileDate   string `json:"crandb_file_date"`
	RoxygenNote      string `json:"RoxygenNote"`
	Type             string `json:"Type"`
	Description      string `json:"Description"`
	Repository       string `json:"Repository"`
}

type CranHit struct {
	Index  string        `json:"_index"`
	Type   string        `json:"_type"`
	Id     string        `json:"_id"`
	Score  float32       `json:"_score"`
	Source CranHitSource `json:"_source"`
}

type CranHits struct {
	Total    int       `json:"total"`
	MaxScore float32   `json:"max_score"`
	Hits     []CranHit `json:"hits"`
}

type CranShards struct {
	Total      int `json:"total"`
	Successful int `json:"successful"`
	Skipped    int `json:"skipped"`
	Failed     int `json:"failed"`
}

type CranResponse struct {
	Took     int        `json:"took"`
	TimedOut bool       `json:"timed_out"`
	Shards   CranShards `json:"_shards"`
	Hits     CranHits   `json:"hits"`
}

func searchPackages(name string, size int) CranResponse {
	searchUrl := "http://search.r-pkg.org/package/_search?q=" + url.QueryEscape(name) + "&size=" + strconv.Itoa(size)

	if req, err := http.Get(searchUrl); err == nil {
		var res CranResponse
		
		decoder := json.NewDecoder(req.Body)
		
		if err = decoder.Decode(&res); err == nil {
			return res
		} else {
			panic(err)
		}
	} else {
		panic(err)
	}
}

func SearchPackages(name string) []CranHit {
	res := searchPackages(name + "*", 0) // needed in order to get the total amount of matching packages
	res = searchPackages(name + "*", res.Hits.Total)

	hits := []CranHit {}

	for _, hit := range res.Hits.Hits {
		if strings.Contains(hit.Id, name) {
			hits = append(hits, hit)
		}
	}

	sort.Slice(hits, func(i1 int, i2 int) bool {
		return hits[i1].Score < hits[i2].Score
	})

	if len(hits) > 50 {
		return hits[:50]
	} else {
		return hits
	}
}

func SearchPackage(name string) *CranHit {
	res := searchPackages(name + "*", 0) // needed in order to get the total amount of matching packages
	res = searchPackages(name + "*", res.Hits.Total)

	for _, hit := range res.Hits.Hits {
		if hit.Id == name {
			return &hit
		}
	}

	return nil
}
