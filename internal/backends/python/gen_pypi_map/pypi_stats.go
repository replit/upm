package main

import (
	"cloud.google.com/go/bigquery"
	"fmt"
	"golang.org/x/net/context"
	"google.golang.org/api/iterator"
)

func GetPypiStats(projectID string) (PackageCache, error) {
	ctx := context.Background()
	client, err := bigquery.NewClient(ctx, projectID)
	if err != nil {
		return PackageCache{}, fmt.Errorf("Failed to connect to bigquery: %v", err)
	}

	q := client.Query(`
			SELECT file.project AS name, COUNT(*) AS downloads
						FROM ` + "`the-psf.pypi.downloads*`" + `
						WHERE _TABLE_SUFFIX BETWEEN FORMAT_DATE( '%Y%m%d', DATE_SUB(CURRENT_DATE(), INTERVAL 30 DAY))
									AND FORMAT_DATE('%Y%m%d', CURRENT_DATE())
						GROUP BY file.project`)

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
