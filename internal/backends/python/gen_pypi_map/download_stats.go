package main

import (
	"encoding/json"
	"fmt"
	"os"

	"cloud.google.com/go/bigquery"
	"golang.org/x/net/context"
	"google.golang.org/api/iterator"
)

func FetchBQDownloads(gcp string, filePath string) error {
	bqCache, err := GetPypiStats(gcp)
	if err != nil {
		return err
	}

	writer, err := os.Create(filePath)
	if err != nil {
		return err
	}

	cacheEncoder := json.NewEncoder(writer)
	cacheEncoder.SetIndent("", "  ")
	cacheEncoder.Encode(bqCache)

	writer.Close()
	return nil
}

func LoadDownloadStats(path string) (map[string]int, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("could not open cache: %v", err)
	}

	packages := map[string]int{}

	decoder := json.NewDecoder(file)
	decoder.Decode(&packages)

	return packages, nil
}

func SaveDownloadStats(path string, cache PackageCache) {
	writer, err := os.Create(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to open cache file for writing %v\n", err)
	}
	defer writer.Close()

	cacheEncoder := json.NewEncoder(writer)
	cacheEncoder.SetIndent("", "  ")
	cacheEncoder.Encode(cache)
}

func GetPypiStats(projectID string) (map[string]int, error) {
	ctx := context.Background()
	client, err := bigquery.NewClient(ctx, projectID)
	if err != nil {
		return map[string]int{}, fmt.Errorf("Failed to connect to bigquery: %v", err)
	}

	q := client.Query(
		`select project as name, count(*) as downloads
		from ` + "`bigquery-public-data.pypi.file_downloads`" + ` downloads
		where
		Date(timestamp) >= DATE_SUB(CURRENT_DATE(), INTERVAL 30 DAY)
		group by project
		order by downloads desc;`)

	it, err := q.Read(ctx)
	if err != nil {
		return map[string]int{}, fmt.Errorf("Failed to query downloads: %v", err)
	}

	packages := map[string]int{}
	for {
		var info PackageInfo
		err := it.Next(&info)
		if err == iterator.Done {
			break
		}
		if err != nil {
			return packages, err
		}
		packages[info.Name] = info.Downloads
	}

	return packages, nil
}
