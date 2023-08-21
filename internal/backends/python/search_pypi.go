package python

import (
	"fmt"
	"net/http"
	"net/url"
	"strings"

	"github.com/replit/upm/internal/api"
	"golang.org/x/net/html"
)

var WhiteSpaceChars = " \n\t"

// Port of https://github.com/asadmoosvi/pypi-search/blob/main/pypi_search/search.py
func SearchPypi(query string) ([]api.PkgInfo, error) {
	endpoint := fmt.Sprintf("https://pypi.org/search/?q=%s", url.QueryEscape(query))
	req, err := http.NewRequest("GET", endpoint, nil)
	if err != nil {
		return nil, err
	}
	resp, err := api.HttpClient.Do(req)
	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("Request to %s failed with %s", endpoint, resp.Status)
	}
	if err != nil {
		return nil, err
	}
	tree, err := html.Parse(resp.Body)
	if err != nil {
		return nil, err
	}
	results := findSearchResults(tree)
	return results, nil
}

func findSearchResults(doc *html.Node) []api.PkgInfo {
	packageSnippets := findNodes(doc, func(node *html.Node) bool {
		if node.Type != html.ElementNode || node.Data != "a" {
			return false
		}
		for _, attr := range node.Attr {
			if attr.Key == "class" && attr.Val == "package-snippet" {
				return true
			}
		}
		return false
	})
	var results []api.PkgInfo
	for _, snippet := range packageSnippets {
		results = append(results, parsePackageSnippet(snippet))
	}
	return results
}

func parsePackageSnippet(node *html.Node) api.PkgInfo {
	spans := findNodes(node, func(node *html.Node) bool {
		return node.Type == html.ElementNode && node.Data == "span"
	})
	ps := findNodes(node, func(node *html.Node) bool {
		return node.Type == html.ElementNode && node.Data == "p"
	})
	var info api.PkgInfo
	// Be defensive just in case
	if len(spans) > 0 {
		info.Name = strings.Trim(collectText(spans[0]), WhiteSpaceChars)
	} else {
		info.Name = "(Unknown)"
	}
	if len(spans) > 1 {
		info.Version = strings.Trim(collectText(spans[1]), WhiteSpaceChars)
	}
	if len(ps) > 0 {
		info.Description = strings.Trim(collectText(ps[0]), WhiteSpaceChars)
	}
	return info
}

func collectText(node *html.Node) string {
	textNodes := findNodes(node, func(node *html.Node) bool {
		return node.Type == html.TextNode
	})
	text := ""
	for _, node := range textNodes {
		text += node.Data
	}
	return text
}

func findNodes(node *html.Node, want func(*html.Node) bool) []*html.Node {
	if want(node) {
		return []*html.Node{node}
	}
	var results []*html.Node
	for child := node.FirstChild; child != nil; child = child.NextSibling {
		subResults := findNodes(child, want)
		results = append(results, subResults...)
	}
	return results
}
