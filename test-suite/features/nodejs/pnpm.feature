Feature: pnpm
	PNPM support using NodeJS

	Background: pnpm is installed
		Given pnpm is installed
		And a bug with "list -a"

	Rule: basic functionality
		Background: empty project
			Given a javascript project without dependencies
			When I run "pnpm install"

		Scenario: upm which-language
			Then the detected language should be "nodejs-pnpm"

		Scenario: upm search
			Then searching for "express" should include "express" in the results

		Scenario: upm info
			Then I should get info for "express"

		Scenario: upm show-specfile
			Then the specfile should be "package.json"

		Scenario: upm show-lockfile
			Then the lockfile should be "pnpm-lock.yaml"

		Scenario: upm show-package-dir
			Then the package directory should be "node_modules"

	Rule: it works without any dependencies
		Background: pnpm project without dependencies
			Given a javascript project without dependencies
			And the language is "nodejs-pnpm"

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
		Background: pnpm project with express
			Given a javascript project with express
			And the language is "nodejs-pnpm"

		Scenario: upm add
			When I add "lodash"
			Then "lodash" should be a dependency
			And "lodash" should be locked
			And "express" should be a dependency
			And "express" should be locked

		Scenario: upm remove
			When I remove "express"
			Then there should be no dependencies

		Scenario: upm lock
			When I lock the specfile
			Then "express" should be a dependency
			And "express" should be locked

		Scenario: upm install
			When I install dependencies
			Then "express" should be a dependency
			And "express" should be locked

		Scenario: upm list
			Then "express" should be a dependency

	Rule: it works with other dependencies
		Background: pnpm project with dependencies
			Given a javascript project with dependencies
			And the language is "nodejs-pnpm"

		Scenario: upm add
			When I add "chalk"
			Then "chalk" should be a dependency
			And "chalk" should be locked
			And "express" should be a dependency
			And "lodash" should be a dependency
			And "react" should be a dependency

		Scenario: upm remove
			When I remove "react"
			Then "express" should be a dependency
			And "express" should be locked
			And "lodash" should be a dependency
			And "lodash" should be locked

		Scenario: upm lock
			When I lock the specfile
			Then "express" should be a dependency
			And "express" should be locked
			And "lodash" should be a dependency
			And "lodash" should be locked
			And "react" should be a dependency
			And "react" should be locked

		Scenario: upm install
			When I delete the package directory
			And I install dependencies
			Then "express" should be a dependency
			And "express" should be locked
			And "lodash" should be a dependency
			And "lodash" should be locked
			And "react" should be a dependency
			And "react" should be locked

		Scenario: upm list
			Then "express" should be a dependency
			And "lodash" should be a dependency
			And "react" should be a dependency
