import { After, AfterStep, Before, BeforeAll, Given, IWorldOptions, Then, When, World, setWorldConstructor } from "@cucumber/cucumber";
import assert from "assert";
import { mkdir, symlink, writeFile } from "fs/promises";
import * as javascript from './javascript';
import { execSync } from "child_process";
import { existsSync, mkdirSync, mkdtempSync } from "fs";
import { rimraf } from 'rimraf';
import { tmpdir } from "os";
import path from "path";

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

		return execSync(command, {
			env: {
				PATH: this.binDirectory,
				...extendEnv
			},
			stdio: ['ignore', 'pipe', 'inherit'],
			...options,
		});
	}
}

setWorldConstructor(UPMWorld);

Before<UPMWorld>(async function () {
	this.install('upm');
});

AfterStep<UPMWorld>(async function ({ result }) {
	if (['FAILED'].includes(result.status)) {
		this.failed ||= true;
	}
});

After<UPMWorld>(async function () {
	if (!this.failed) {
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
	const filePath = `${this.projectDirectory}/${filename}`;

	if (existsSync(filePath)) {
		throw new Error(`File ${filename} already exists`);
	}

	await writeFile(filePath, content);
});

Then<UPMWorld>("the detected language should be {string}", async function (expectLanguage: string) {
	const output = this.exec('upm detect-language').toString().trim();
	assert.equal(output, expectLanguage);
});

When<UPMWorld>("I search for {string}", async function (query: string) {
	assert(this.searchResults === null);

	const output = this.exec(`upm search -f json ${query}`).toString().trim();

	this.searchResults = JSON.parse(output);
});

Then<UPMWorld>("I should see {string} in the results", function (packageName: string) {
	assert(this.searchResults !== null);

	assert(this.searchResults.some((result) => (result['name'] as string) === packageName));
});
