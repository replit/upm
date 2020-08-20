package main

import (
	"archive/tar"
	"archive/zip"
	"compress/gzip"
	"io"
	"fmt"
	"os"
	"io/ioutil"
)

type ArchiveReader struct {
	Next   func() (string, error)
	Reader func() (io.Reader, error)
	Close  func()
}

func MakeZipReader(reader io.Reader, size int64) (ArchiveReader, error) {
	cacheFile, err := ioutil.TempFile(os.TempDir(), "pypi-")
	if err != nil {
		return ArchiveReader{}, fmt.Errorf("Cannot open cache for zip: %v", err)
	}

	// Copy the contents of the reader to disk
	io.Copy(cacheFile, reader)

	cacheFile.Close()

	zipFile, err := os.Open(cacheFile.Name())
	if err != nil {
		return ArchiveReader{}, fmt.Errorf("Cannot open cache for zip: %v", err)
	}

	zipReader, err := zip.NewReader(zipFile, size)
	if err != nil {
		return ArchiveReader{}, err
	}

	i := -1
	next := func() (string, error) {
		i++
		if i < len(zipReader.File) {
			return zipReader.File[i].Name, nil
		}
		return "", io.EOF
	}

	fileReader := func() (io.Reader, error) {
		return zipReader.File[i].Open()
	}

	archiveClose := func() {
		zipFile.Close()
		os.Remove(zipFile.Name())
	}

	return ArchiveReader{Next: next, Reader: fileReader, Close: archiveClose}, nil
}

func MakeTarballReader(reader io.Reader) (ArchiveReader, error) {
	gzipReader, err := gzip.NewReader(reader)
	if err != nil {
		return ArchiveReader{}, err
	}

	tarReader := tar.NewReader(gzipReader)

	next := func() (string, error) {
		header, err := tarReader.Next()
		if err != nil {
			return "", err
		}

		return header.Name, nil
	}

	fileReader := func() (io.Reader, error) {
		return tarReader, nil
	}

	closeGzip := func() () {
		gzipReader.Close()
	}

	return ArchiveReader{Next: next, Reader: fileReader, Close: closeGzip}, nil
}
