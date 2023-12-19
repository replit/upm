package python

import (
	"testing"

	"github.com/replit/upm/internal/api"
	assert "github.com/stretchr/testify/assert"
)

func TestRequirementsParser(t *testing.T) {
	flags, deps, err := ListRequirementsTxt("test_resources/requirements/requirements.txt")

	assert.Empty(t, flags)
	assert.Empty(t, err)

	assert.Equal(t, map[api.PkgName]api.PkgSpec{"foo":"", "triple-equals":"===1.3.5"}, deps)
}

func TestRecurseRequirementsParser(t *testing.T) {
	flags, deps, err := ListRequirementsTxt("test_resources/requirements/dependent-requirements.txt")

	assert.Empty(t, flags)
	assert.Empty(t, err)

	assert.Equal(t, map[api.PkgName]api.PkgSpec{"foo":"", "greater-than": ">0.3.6", "triple-equals":"===1.3.5"}, deps)
}

func TestSpecParser(t *testing.T) {
	flags, deps, err := ListRequirementsTxt("test_resources/requirements/spec-requirements.txt")

	assert.Empty(t, flags)
	assert.Empty(t, err)

	assert.Equal(t, map[api.PkgName]api.PkgSpec{
		"SomeProjectBare": "",
		"SomeProjectDoubleEquals": "== 1.3",
		"SomeProjectGEL": ">= 1.2, < 2.0",
		"SomeProjectExtras": "[foo, bar]",
		"SomeProjectCompatible": "~= 1.4.2",
		"SomeProjectExtrasRange": "[security] >= 2.8.1, == 2.8.*",
	}, deps)
}

func TestRequirementsNotFound(t *testing.T) {
	flags, deps, err := ListRequirementsTxt("test_resources/requirements/requirements-not-found.txt")

	assert.Empty(t, flags)
	assert.Empty(t, deps)

	assert.NotEmpty(t, err)
}
