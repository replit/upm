package java

import (
	"testing"
)

func TestSearchMavenCentral(t *testing.T) {
	results, err := Search("junit")

	if err != nil {
		t.Errorf("Search failed with \n%q\n", err)
	}

	if len(results) != 10 {
		t.Errorf("Only %q junit results found", len(results))
	}

	for i := range results {
		pkg := results[i]
		if pkg.Artifact == "" {
			t.Errorf("pkg %q has no name", pkg)
		}
		if pkg.Version == "" {
			t.Errorf("pkg %q has no version", pkg)
		}
	}
}

func TestInfoMavenCentral(t *testing.T) {
	pkg := "org.apache.logging.log4j:log4j-core"
	info, err := Info(pkg)

	if err != nil {
		t.Errorf("Failed to find package with \n%q\n", err)
	}

	if info.CurrentVersion == "" {
		t.Errorf("Did not find info for %q", pkg)
	}
}

func TestInfoWithArtifactNameOnly(t *testing.T) {
	artifact := "log4j-core"
	info, err := Info(artifact)

	if err != nil {
		t.Errorf("Failed to find package with \n%q\n", err)
	}

	if info.CurrentVersion == "" {
		t.Errorf("Did not find infor for %q", artifact)
	}
}

func TestInfoWithUnknownArtifact(t *testing.T) {
	artifact := "yyy"
	info, err := Info(artifact)

	if err != nil {
		t.Errorf("Failed to find package with \n%q\n", err)
	}

	if &info == nil {
		t.Error("Got nil instead of zero record")
	}
}
