Feature: nodejs-npm
	NPM support using nodejs

	Scenario: searching for a package
		Given a javascript project
		And npm is installed
		When I search for "express"
		Then I should see "express" in the results
