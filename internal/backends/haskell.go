package backends

import (
	"github.com/replit/upm/internal/api"
)

var haskellBackend = api.LanguageBackend{
	Name:     "haskell-stack",
	Specfile: "package.yaml",
}
