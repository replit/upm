package main

import (
  "bytes"
  "compress/gzip"
  "archive/zip"
  "archive/tar"
  "io"
)

type ArchiveReader struct {
  Next func() (string, error)
  Reader func() (io.Reader, error)
  //Read(p []byte) (n int, err error)
}

func MakeZipReader(reader io.Reader, size int64) (ArchiveReader, error) {
  // TODO Create an unbuffered ReaderAt to read zip from
  buff := bytes.NewBuffer([]byte{})
  size, err := io.Copy(buff, reader)
  if err != nil {
    return ArchiveReader{}, err
  }
  memReader := bytes.NewReader(buff.Bytes())

  zipReader, err := zip.NewReader(memReader, size)
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

  return ArchiveReader{Next: next, Reader: fileReader}, nil
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

  return ArchiveReader{Next: next, Reader: fileReader}, nil
}
