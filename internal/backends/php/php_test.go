package php

import (
	"io/ioutil"
	"testing"

	"github.com/replit/upm/internal/api"
	"github.com/stretchr/testify/require"
)

func TestPackagistInfo(t *testing.T) {
	info := PhpComposerBackend.Info(api.PkgName("psr/log"))
	// We don't want to check too many fields since they can be changed externally and break this test.
	require.Equal(t, "psr/log", info.Name)
}

func TestPackagistSearch(t *testing.T) {
	results := PhpComposerBackend.Search("psr/log")
	// We don't want to check the results as they may change externally and break this test.
	require.NotEmpty(t, results)
}

func TestListSpecFile(t *testing.T) {
	testcases := []struct {
		testFilename   string
		expectedOutput map[api.PkgName]api.PkgSpec
	}{
		{
			testFilename: "composer1.json",
			expectedOutput: map[api.PkgName]api.PkgSpec{
				api.PkgName("aws/aws-sdk-php"):   api.PkgSpec("^3.209"),
				api.PkgName("guzzlehttp/guzzle"): api.PkgSpec("7.0"),
				api.PkgName("monolog/monolog"):   api.PkgSpec("^3.2"),
			},
		},
	}

	for _, test := range testcases {
		contents, err := ioutil.ReadFile("testdata/" + test.testFilename)
		require.NoError(t, err)
		require.Equal(t, test.expectedOutput, listSpecfileWithContents(contents))
	}
}

func TestListLockFile(t *testing.T) {
	testcases := []struct {
		testFilename   string
		expectedOutput map[api.PkgName]api.PkgVersion
	}{
		{
			testFilename: "composer1.lock",
			expectedOutput: map[api.PkgName]api.PkgVersion{
				api.PkgName("monolog/monolog"): api.PkgVersion("3.2.0"),
				api.PkgName("psr/log"):         api.PkgVersion("3.0.0"),
			},
		},
	}

	for _, test := range testcases {
		contents, err := ioutil.ReadFile("testdata/" + test.testFilename)
		require.NoError(t, err)
		require.Equal(t, test.expectedOutput, listLockfileWithContents(contents))
	}
}

func TestSearchParsing(t *testing.T) {
	testcases := []struct {
		testFilename   string
		errorMessage   string
		expectedOutput []api.PkgInfo
	}{
		{
			testFilename: "searchResponse.json",
			errorMessage: "",
			expectedOutput: []api.PkgInfo{
				{
					Name:          "monolog/monolog",
					Description:   "Sends your logs to files, sockets, inboxes, databases and various web services",
					SourceCodeURL: "https://github.com/Seldaek/monolog",
				},
				{
					Name:          "symfony/monolog-bundle",
					Description:   "Symfony MonologBundle",
					SourceCodeURL: "https://github.com/symfony/monolog-bundle",
				},
			},
		},
	}

	for _, test := range testcases {
		contents, err := ioutil.ReadFile("testdata/" + test.testFilename)
		require.NoError(t, err)

		resp, err := parseSearch(contents)

		if len(test.errorMessage) == 0 {
			require.NoError(t, err)
			require.Equal(t, test.expectedOutput, resp)
		} else {
			require.Equal(t, test.errorMessage, err.Error())
		}

	}
}

func TestInfoParsing(t *testing.T) {
	testcases := []struct {
		testFilename    string
		testPackageName api.PkgName
		errorMessage    string
		expectedOutput  api.PkgInfo
	}{
		{
			testFilename:    "infoResponse.json",
			testPackageName: api.PkgName("monolog/monolog"),
			errorMessage:    "",
			expectedOutput: api.PkgInfo{
				Name:          "monolog/monolog",
				Description:   "Sends your logs to files, sockets, inboxes, databases and various web services",
				Version:       "3.2.0",
				HomepageURL:   "https://github.com/Seldaek/monolog",
				License:       "MIT",
				Author:        "Jordi Boggiano",
				BugTrackerURL: "https://github.com/Seldaek/monolog/issues",
			},
		},
		{
			testFilename:    "failure.html",
			testPackageName: api.PkgName("testWhoCares"),
			errorMessage:    "Malformed Packagist response. Package may not exist",
			expectedOutput:  api.PkgInfo{},
		},
		{
			testFilename:    "infoResponse.json",
			testPackageName: api.PkgName("psr/log"),
			errorMessage:    "Requested package was not in packagist response",
			expectedOutput:  api.PkgInfo{},
		},
		{
			testFilename:    "emptyPackagistInfoResponse.json",
			testPackageName: api.PkgName("monolog/monolog"),
			errorMessage:    "Empty message from packagist",
			expectedOutput:  api.PkgInfo{},
		},
	}

	for _, test := range testcases {
		contents, err := ioutil.ReadFile("testdata/" + test.testFilename)
		require.NoError(t, err)
		resp, err := parseInfo(contents, test.testPackageName)

		if len(test.errorMessage) == 0 {
			require.Equal(t, test.expectedOutput, resp)
			require.NoError(t, err)
		} else {
			require.Equal(t, test.errorMessage, err.Error())
		}
	}
}
