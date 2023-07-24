package python

import (
	"database/sql"
	"fmt"
	"os"
	"strings"

	_ "github.com/mattn/go-sqlite3"
)

type PypiMap struct {
	db *sql.DB
}

func NewPypiMap() (*PypiMap, error) {
	dbFilePath := os.Getenv("PYPI_MAP_DB")
	if dbFilePath == "" {
		return nil, fmt.Errorf("PYPI_MAP_DB not set. Set it to the path of the sqlite database.")
	}
	// Open db in read-only mode so it doesn't try to make any modifications
	// which would cause it to make a copy of the file in our layered fs
	db, err := sql.Open("sqlite3", dbFilePath+"?mode=ro")
	if err != nil {
		return nil, err
	}

	return &PypiMap{
		db: db,
	}, nil
}

func (p *PypiMap) Close() error {
	err := p.db.Close()
	fmt.Println("Closed pypi map db")
	return err
}

func (p *PypiMap) ModuleToPackage(moduleName string) (string, bool) {
	stmt, err := p.db.Prepare("select guess from module_to_pypi_package where module_name = ?")
	if err != nil {
		return "", false
	}
	defer stmt.Close()
	rows, err := stmt.Query(moduleName)
	if err != nil {
		return "", false
	}
	defer rows.Close()
	if !rows.Next() {
		return "", false
	}
	var guess string
	rows.Scan(&guess)
	return guess, true
}

func (p *PypiMap) PackageToModules(packageName string) ([]string, bool) {
	stmt, err := p.db.Prepare("select module_list from pypi_packages where package_name = ?")
	if err != nil {
		return nil, false
	}
	defer stmt.Close()
	rows, err := stmt.Query(packageName)
	if err != nil {
		return nil, false
	}
	defer rows.Close()
	if !rows.Next() {
		return nil, false
	}
	var moduleList string
	rows.Scan(&moduleList)
	return strings.Split(moduleList, ","), true
}

func (p *PypiMap) SearchModules(query string) []string {
	stmt, err := p.db.Prepare(`
	select package_name
	from pypi_packages
	where package_name like ?
	order by downloads desc limit 20
	`)
	if err != nil {
		return nil
	}
	defer stmt.Close()
	rows, err := stmt.Query(fmt.Sprintf("%%%s%%", query))
	if err != nil {
		return nil
	}
	defer rows.Close()
	var packages []string = nil
	for {
		if !rows.Next() {
			break
		}
		var pkg string
		rows.Scan(&pkg)
		packages = append(packages, pkg)
	}
	return packages
}
