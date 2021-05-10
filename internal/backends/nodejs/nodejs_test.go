package nodejs

import (
	"testing"
)

func TestGuessRegexpsFindBacktickRequire(t *testing.T) {
	contents := "require(`test`)"
	found := false
	for _, r := range NodejsNPMBackend.GuessRegexps {
		if r.FindString(contents) != "" {
			found = true
		}
	}
	if !found {
		t.Error("Guess regex search failed")
	}
}
