Feature: npm
  NPM support using NodeJS

  Background: it's a nodejs-npm project
    Given a javascript project
    And npm is installed

  Scenario: detecting npm as the package manager
    When I run "npm install"
    Then the detected language should be "nodejs-npm"

  Scenario: getting node_modules
    When I run "npm install"
    Then the package directory should be "node_modules"

  Scenario: searching for a package
    When I search for "express"
    Then I should see "express" in the search results

  Scenario: getting info for a package
    Then I should get info for "express"

  Scenario: adding a package
    Given a file named "package.json" with:
    """
    {
      "package": "adding-a-package",
      "version": "70.531.415",
      "license": "MIT"
    }
    """
    When I run "npm install"
    And I add the "express" dependency
    Then the "package.json" file should be:
    """
    {
      "package": "adding-a-package",
      "version": "70.531.415",
      "license": "MIT",
      "dependencies": {
        "express": "^4.18.2"
      }
    }

    """

  Scenario: removing a package

  Scenario: locking a package.json

  Scenario: installing a package.json

  Scenario: listing installed packages

  Scenario: guessing a package

  Scenario: listing packages from package.json

  Scenario: listing packages from package-lock.json
