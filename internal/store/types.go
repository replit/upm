package store

type Hash string

type Store struct {
	SpecfileHash Hash `json:"specfileHash"`
	LockfileHash Hash `json:"lockfileHash"`
}
