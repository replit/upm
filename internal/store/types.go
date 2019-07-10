package store

type hash string

type store struct {
	SpecfileHash hash `json:"specfileHash"`
	LockfileHash hash `json:"lockfileHash"`
}
