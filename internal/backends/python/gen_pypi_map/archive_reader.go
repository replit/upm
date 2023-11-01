package main

import (
	"archive/tar"
	"archive/zip"
	"compress/gzip"
	"fmt"
	"io"
	"os"
	"os/exec"
)

type ArchiveReader struct {
	Next   func() bool
	Error  func() error
	File   func() string
	Reader func() (io.Reader, error)
	Close  func()
	Dump   func(dir string) error
}

func MakeZipReader(reader io.Reader, size int64) (ArchiveReader, error) {
	cacheFile, err := os.CreateTemp(os.TempDir(), "pypi-")
	if err != nil {
		return ArchiveReader{}, fmt.Errorf("Cannot open cache for zip: %v", err)
	}

	// Copy the contents of the reader to disk
	_, err = io.Copy(cacheFile, reader)
	if err != nil {
		return ArchiveReader{}, err
	}

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
	next := func() bool {
		i++
		return i < len(zipReader.File)
	}

	archiveFile := func() string {
		return zipReader.File[i].Name
	}

	archiveError := func() error {
		if i >= len(zipReader.File) {
			return io.EOF
		}
		return nil
	}

	fileReader := func() (io.Reader, error) {
		return zipReader.File[i].Open()
	}

	archiveClose := func() {
		zipFile.Close()
		os.Remove(zipFile.Name())
	}

	archiveDump := func(dir string) error {
		cmd := exec.Command("unzip", "-d", dir, cacheFile.Name())
		return cmd.Run()
	}

	return ArchiveReader{Next: next, Error: archiveError, File: archiveFile, Reader: fileReader, Close: archiveClose, Dump: archiveDump}, nil
}

func MakeTarballReader(reader io.Reader) (ArchiveReader, error) {
	gzipReader, err := gzip.NewReader(reader)
	if err != nil {
		return ArchiveReader{}, err
	}

	tarReader := tar.NewReader(gzipReader)
	var header *tar.Header

	next := func() bool {
		header, err = tarReader.Next()
		return err != nil
	}

	tarballError := func() error {
		return err
	}

	tarballFile := func() string {
		return header.Name
	}

	fileReader := func() (io.Reader, error) {
		return tarReader, nil
	}

	closeGzip := func() {
		gzipReader.Close()
	}

	archiveDump := func(dir string) error {
		cmd := exec.Command("tar", "-x", "-C", dir)
		p, err := cmd.StdinPipe()
		if err != nil {
			return err
		}

		defer p.Close()

		err = cmd.Start()
		if err != nil {
			return err
		}

		_, err = io.Copy(p, gzipReader)
		if err != nil {
			return err
		}

		err = cmd.Wait()
		if err != nil {
			return err
		}

		return nil
	}

	return ArchiveReader{Next: next, Error: tarballError, File: tarballFile, Reader: fileReader, Close: closeGzip, Dump: archiveDump}, nil
}
