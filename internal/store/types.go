package store

type Hash string

type Store struct {
	specfileHash Hash
	lockfileHash Hash
}
