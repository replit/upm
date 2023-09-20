Feature: yarn
	Yarn support using NodeJS

	Background: yarn is installed
		Given yarn is installed

	Rule: basic functionality
		Background: empty project
			Given a javascript project without dependencies

		Scenario: upm which-language
			When I run "yarn"
			Then the detected language should be "nodejs-yarn"

		Scenario: upm search
			Then searching for "express" should include "express" in the results

		Scenario: upm info
			Then I should get info for "express"

		Scenario: upm show-specfile
			Then the specfile should be "package.json"

		Scenario: upm show-lockfile
			When I run "yarn"
			Then the lockfile should be "yarn.lock"

		Scenario: upm show-package-dir
			Then the package directory should be "node_modules"

	Rule: it works without any dependencies
		Background: yarn project without dependencies
			Given a javascript project without dependencies
			And the language is "nodejs-yarn"

		Scenario: upm add
			When I add "express"
			Then "express" should be a dependency
			And "express" should be locked

		Scenario: upm lock
			When I lock the specfile
			Then there should be no dependencies

		Scenario: upm install
			When I install dependencies
			Then there should be no dependencies

		Scenario: upm list
			Then there should be no dependencies

	Rule: it works with another dependency
		Background: yarn project with express
			Given a javascript project with express
			And the language is "nodejs-yarn"

		Scenario: upm add
			When I add "lodash"
			Then "lodash" should be a dependency
			And "lodash" should be locked
			And "express" should be a dependency
			And "express" should be locked

		Scenario: upm remove
			# TODO: yarn errors if the lockfile isn't present
			When I lock the specfile
			And I remove "express"
			Then there should be no dependencies

		Scenario: upm lock
			When I lock the specfile
			Then "express" should be a dependency
			And "express" should be locked

		Scenario: upm install
			When I lock the specfile
			# TODO: yarn does nothing if the lockfile isn't present - this step is a no-op
			And I install dependencies
			Then "express" should be a dependency
			And "express" should be locked

		Scenario: upm list
			Then "express" should be a dependency

	Rule: it works with several other dependencies
		Background: yarn project with dependencies
			Given a javascript project with dependencies
			And the language is "nodejs-yarn"
			And I lock the specfile

		@bug
		Scenario: upm add
			When I add "@replit/crosis"
			Then "express" should be a dependency
			And "lodash" should be a dependency
			And "react" should be a dependency
			And "@replit/crosis" should be a dependency
			And "@replit/crosis" should be locked

		Scenario: upm remove
			When I remove "react"
			Then "express" should be a dependency
			And "express" should be locked
			And "lodash" should be a dependency
			And "lodash" should be locked

		Scenario: upm install
			When I delete the package directory
			And I install dependencies
			Then "express" should be a dependency
			And "express" should be locked
			And "lodash" should be a dependency
			And "lodash" should be locked
			And "react" should be a dependency
			And "react" should be locked

		Scenario: upm lock
			Then "express" should be a dependency
			And "express" should be locked
			And "lodash" should be a dependency
			And "lodash" should be locked
			And "react" should be a dependency
			And "react" should be locked
