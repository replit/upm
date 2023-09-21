import { After, Before, Given, Then, When } from "@cucumber/cucumber";
import UPMWorld from "./world";
import assert from "assert";
import { rimraf } from "rimraf";
import path from "path";

Before<UPMWorld>(
	{ name: 'Install UPM' },
	async function () {
		await this.install('upm');
	}
)

/** pre-set the language */
Given<UPMWorld>(
	"the language is {string}",
	async function (language: string) {
		const supportedLanguages = (await this.upm('list-languages')).split('\n');
		assert(
			supportedLanguages.includes(language),
			`language ${language} is not supported`,
		);

		this.upmLanguage = language;
	}
)

/** upm which-language */
Then<UPMWorld>(
	"the detected language should be {string}",
	async function (expectLanguage: string) {
		const output = await this.upm('which-language');
		assert.equal(
			output,
			expectLanguage,
			`upm did not print ${expectLanguage}`,
		);
	}
)

/** upm search */
Then<UPMWorld>(
	"searching for {string} should include {string} in the results",
	async function (query: string, packageName: string) {
		const output = await this.upm(`search -f json ${query}`);
		const results: { name: string; }[] = JSON.parse(output);
		assert(
			results.some((result) => result.name === packageName),
			`${packageName} is not in the search results`,
		);
	}
)

/** upm info */
Then<UPMWorld>(
	"I should get info for {string}",
	async function (packageName: string) {
		const output = await this.upm(`info -f json ${packageName}`);
		const outputJson = JSON.parse(output);

		assert(
			outputJson.name === packageName,
			`expected ${packageName} but got ${outputJson.name}`,
		);
	}
)

/** upm add */
When<UPMWorld>(
	"I add {string}",
	async function (packageName: string) {
		await this.upm(`add ${packageName}`);
	}
)

/** upm remove */
When<UPMWorld>(
	"I remove {string}",
	async function (packageName: string) {
		await this.upm(`remove ${packageName}`);
	}
)

/** upm lock */
When<UPMWorld>(
	"I lock the specfile",
	async function () {
		await this.upm('lock');
	}
)

/** upm install */
When<UPMWorld>(
	"I install dependencies",
	async function () {
		await this.upm('install');
	}
)

When<UPMWorld>(
	"I force-install dependencies",
	async function () {
		await this.upm('install --force');
	}
)

/** upm list */
Then<UPMWorld>(
	"{string} should be a dependency",
	async function (packageName: string) {
		const output = await this.upm('list -f json');
		const dependencies: { name: string; }[] = JSON.parse(output);
		assert(
			dependencies.some((dep) => dep.name === packageName),
			`${packageName} is not a dependency`,
		);
	}
)

Then<UPMWorld>(
	"{string} should be locked",
	async function (packageName: string) {
		if (this.knownBugs.includes('list -a')) {
			return 'skipped';
		}

		const output = await this.upm('list -f json -a');
		const installedPackages: { name: string; }[] = JSON.parse(output);
		assert(
			installedPackages.some((pkg) => pkg.name === packageName),
			`${packageName} is not in the lockfile`,
		);
	}
)

Then<UPMWorld>(
	"there should be no dependencies",
	async function () {
		const output = await this.upm('list -f json');
		const dependencies: unknown[] = JSON.parse(output);
		assert(
			dependencies.length === 0,
			`there are still dependencies`,
		);
	}
)

/** upm guess */
When<UPMWorld>(
	"I guess the dependencies",
	async function () {
		await this.upm('guess');
	}
)

/** upm show-specfile */
Then<UPMWorld>(
	"the specfile should be {string}",
	async function (specfile: string) {
		const output = await this.upm('show-specfile');
		assert.equal(
			output,
			specfile,
			`upm did not detect ${specfile}`,
		);
	}
)

/** upm show-lockfile */
Then<UPMWorld>(
	"the lockfile should be {string}",
	async function (lockfile: string) {
		const output = await this.upm('show-lockfile');
		assert.equal(
			output,
			lockfile,
			`upm did not detect ${lockfile}`,
		);
	}
)

/** upm show-package-dir */
Then<UPMWorld>(
	"the package directory should be {string}",
	async function (dir: string) {
		const output = await this.upm('show-package-dir');
		assert.equal(
			output,
			dir,
			`upm did not detect ${dir}`,
		);
	}
)

When<UPMWorld>(
	"I delete the package directory",
	async function () {
		const packageDir = await this.upm('show-package-dir');
		await rimraf(path.join(this.projectDirectory, packageDir));
	}
)
