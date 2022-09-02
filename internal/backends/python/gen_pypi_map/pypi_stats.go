package main

import (
	"fmt"

	"cloud.google.com/go/bigquery"
	"golang.org/x/net/context"
	"google.golang.org/api/iterator"
)

func GetPypiStats(projectID string) (PackageCache, error) {
	ctx := context.Background()
	client, err := bigquery.NewClient(ctx, projectID)
	if err != nil {
		return PackageCache{}, fmt.Errorf("Failed to connect to bigquery: %v", err)
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
		return PackageCache{}, fmt.Errorf("Failed to query downloads: %v", err)
	}

	packages := map[string]PackageInfo{}
	for {
		var info PackageInfo
		err := it.Next(&info)
		if err == iterator.Done {
			break
		}
		if err != nil {
			return packages, err
		}
		packages[info.Name] = info
	}

	return packages, nil
}
