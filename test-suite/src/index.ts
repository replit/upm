import { After, AfterStep, Before, BeforeAll, Given, IWorldOptions, Then, When, World, setWorldConstructor } from "@cucumber/cucumber";
import assert from "assert";
import { mkdir, readFile, readdir, symlink, writeFile } from "fs/promises";
import * as javascript from './javascript';
import { execSync } from "child_process";
import { existsSync, mkdirSync, mkdtempSync } from "fs";
import { rimraf } from 'rimraf';
import { tmpdir } from "os";
import path from "path";

const RESULTS_TO_KEEP_FOR_DEBUGGING = [
	'FAILED',
];

export interface UPMParameters {
	bins: Record<string, string>;
}

class UPMWorld extends World<UPMParameters> {
	/** The directory containing the PATH (/bin) and the project (/project) for each test */
	testRoot: string;

	/** The path to the package manager bin that's being tested  */
	packageManager: string | null = null;
	/**
	 * Search results that have been fetched
	 *
	 * TODO: generate the type for this
	 */
	searchResults: Record<string, string>[] | null = null;
	failed: boolean = false;

	get binDirectory(): string {
		return path.join(this.testRoot, 'bin');
	}

	get projectDirectory(): string {
		return path.join(this.testRoot, 'project');
	}

	constructor(options: IWorldOptions<UPMParameters>) {
		super(options);

		this.testRoot = mkdtempSync(path.join(tmpdir(), 'upm-test-'));
		assert(existsSync(this.testRoot), 'expected mkdtemp to make the testRoot');

		mkdirSync(this.binDirectory)
		mkdirSync(this.projectDirectory)
	}

	async install(command: string): Promise<void> {
		assert(command in this.parameters.bins, `${command} is not configured`)

		const bin = this.parameters.bins[command];
		const installedBin = path.join(this.binDirectory, command);
		if (existsSync(installedBin)) {
			throw new Error(`${command} is already installed`);
		}

		await symlink(bin, installedBin);
	}

	exec(command: string, options?: Parameters<typeof execSync>[1]): ReturnType<typeof execSync> {
		const extendEnv = options?.env ?? {};
		delete options?.env;

		const env = {
			PATH: this.binDirectory,
			...extendEnv
		};

		const output = execSync(command, {
			cwd: this.projectDirectory,
			env,
			stdio: ['ignore', 'pipe', 'inherit'],
			...options,
		});

		const envString = Object.entries(env).map(([key, value]) => `${key}=${value}`).join(' ');
		this.attach(`$ ${envString} ${command}\n${output}`);

		return output;
	}
}

setWorldConstructor(UPMWorld);

Before<UPMWorld>({ name: 'Install UPM' }, async function () {
	this.attach(`Test directory is ${this.testRoot}`);
	await this.install('upm');
});

AfterStep<UPMWorld>(async function ({ result }) {
	if (['FAILED'].includes(result.status)) {
		this.failed ||= true;
	}
});

After<UPMWorld>(async function ({ result }) {
	const installedBins = await readdir(this.binDirectory);
	this.attach(`Installed bins: ${installedBins.join(', ')}`);
	// TODO: attach the project directory as a zip or something even more helpful for debugging

	if (!RESULTS_TO_KEEP_FOR_DEBUGGING.includes(result?.status ?? 'UNKNOWN')) {
		await rimraf(this.testRoot);
	}
});

Given<UPMWorld>("a {word} project", async function (language: string) {
	if (language === 'javascript') {
		await javascript.initProjectDirectory({
			projectDirectory: this.projectDirectory
		});
	} else {
		throw new Error(`Unknown language: ${language}`);
	}
});

Given<UPMWorld>("{word} is installed", function (bin: string) {
	this.install(bin);
});

Given<UPMWorld>("a file named {string} with:", async function (filename: string, content: string) {
	await writeFile(path.join(this.projectDirectory, filename), content);
});

When<UPMWorld>("I add the {string} dependency", function (packageName: string) {
	this.exec(`upm add ${packageName}`);
});

When<UPMWorld>("I run {string}", function (command: string) {
	this.exec(command);
});

When<UPMWorld>("I search for {string}", function (query: string) {
	assert(this.searchResults === null);
	const output = this.exec(`upm search -f json ${query}`).toString().trim();
	this.searchResults = JSON.parse(output);
});

Then<UPMWorld>("the detected language should be {string}", async function (expectLanguage: string) {
	const output = this.exec('upm which-language').toString().trim();
	assert.equal(output, expectLanguage);
});

Then<UPMWorld>("the package directory should be {string}", async function (dir: string) {
	const output = this.exec('upm show-package-dir').toString().trim();
	assert.equal(output, dir);
});

Then<UPMWorld>("I should get info for {string}", async function (packageName: string) {
	const output = this.exec(`upm info -f json ${packageName}`).toString().trim();
	const outputJson = JSON.parse(output);

	assert(outputJson.name === packageName, `expected ${packageName} but got ${outputJson.name}`);
});

Then<UPMWorld>("I should see {string} in the search results", function (packageName: string) {
	assert(this.searchResults !== null, "expected a search to be executed");
	assert(this.searchResults.some((result) => (result['name'] as string) === packageName));
});

Then<UPMWorld>("the {string} file should be:", async function (filename: string, content: string) {
	assert(existsSync(path.join(this.projectDirectory, filename)), `expected ${filename} to exist`);
	const file = await readFile(path.join(this.projectDirectory, filename), { encoding: 'utf-8' });
	this.attach("File contents:\n" + file);
	assert.equal(file, content);
});
