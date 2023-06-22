package api

import (
	"fmt"
	"net/http"
)

var HttpClient = &UpmHttpClient{}

type UpmHttpClient struct {
	http.Client
}

func (c *UpmHttpClient) Do(req *http.Request) (*http.Response, error) {
	req.Header.Set("User-Agent", "upm (+https://github.com/replit/upm)")
	resp, err := c.Client.Do(req)
	if resp == nil && err == nil {
		panic(fmt.Errorf("no response and no error %v", req))
	}

	return resp, err
}

func (c *UpmHttpClient) Get(url string) (*http.Response, error) {
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return &http.Response{}, err
	}
	return c.Do(req)
}
