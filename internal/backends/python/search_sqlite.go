package python

import (
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

func SearchLocalSqlite(query string) ([]api.PkgInfo, error) {
	pypiMap, err := NewPypiMap()
	if err != nil {
		util.DieConsistency(err.Error())
	}
	defer pypiMap.Close()

	return pypiMap.QueryToResults(query)
}
