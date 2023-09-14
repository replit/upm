import { After, AfterStep, Before, BeforeAll, Given, IWorldOptions, Then, When, World, setWorldConstructor } from "@cucumber/cucumber";
import assert from "assert";
import { link, mkdir, mkdtemp, writeFile } from "fs/promises";
import { promisify } from 'node:util';
import * as javascript from './javascript';
import { execSync } from "child_process";
import { exists, existsSync, mkdirSync, mkdtempSync } from "fs";
import { rimraf } from 'rimraf';
import { tmpdir } from "os";
import path from "path";
import { Err, Result } from "./types";

export interface UPMParameters {
	bins: Record<string, string>;
}

class UPMWorld extends World<UPMParameters> {
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

	constructor(options: IWorldOptions<UPMParameters>) {
		super(options);

		this.testRoot = mkdtempSync(path.join(tmpdir(), 'upm-test-'));
		assert(existsSync(this.testRoot), 'expected mkdtemp to make the testRoot');
	}

	async install(command: string): Promise<void> {
		if (!(command in this.parameters.bins)) {
			throw new Error(`${command} is not configured`);
		}

		const bin = this.parameters.bins[command];
		const binPath = path.join(this.testRoot, 'bin');
		if (!existsSync(binPath)) {
			await mkdir(binPath);
		}

		const installedBin = path.join(binPath, command);
		if (existsSync(installedBin)) {
			throw new Error(`${command} is already installed`);
		}

		await link(bin, installedBin);
	}

	exec(command: string, options?: Parameters<typeof execSync>[1]): ReturnType<typeof execSync> {
		const extendEnv = options?.env ?? {};
		delete options?.env;

		return execSync(command, {
			env: {
				PATH: `${this.testRoot}/bin`,
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
	if (result.status === 'FAILED') {
		this.failed = true;
	}
});

After<UPMWorld>(async function () {
	if (!this.failed && this.projectDirectory !== null) {
		await rimraf(this.projectDirectory);
	}
});

Given<UPMWorld>("a {word} project", async function (language: string) {
	assert(this.projectDirectory === null);
	const projectDirectory = await mkdtemp(`upm-test-${language}-`);
	this.projectDirectory = projectDirectory;

	if (language === 'javascript') {
		await javascript.initProjectDirectory({
			projectDirectory,
		});
	} else {
		throw new Error(`Unknown language: ${language}`);
	}
});

Given<UPMWorld>("{word} is installed", function (packageManager: string) {
	assert(this.packageManager === null);
	assert(packageManager in this.parameters.bins);

	const bin = this.parameters.bins[packageManager];
	console.log({ bin })
	try {
		execSync(`${bin} --version`, {
			stdio: 'ignore',
		});
	} catch (e) {
		throw new Error(`${packageManager} is not installed: ${e}`)
	}

	this.path.push(bin);
});

Given<UPMWorld>("a file named {string} with:", async function (filename: string, content: string) {
	if (existsSync(`${this.projectDirectory}/${filename}`)) {
		throw new Error(`File ${filename} already exists`);
	}

	await writeFile(`${this.projectDirectory}/${filename}`, content);
});

Then<UPMWorld>("the detected language should be {string}", async function (expectLanguage: string) {
	assert(this.projectDirectory !== null);

	const output = execSync(`${this.upm} detect-language`, {
		cwd: this.projectDirectory,
	}).toString().trim();

	assert.strictEqual(output, expectLanguage);
});

When<UPMWorld>("I search for {string}", async function (query: string) {
	assert(this.projectDirectory !== null);
	assert(this.packageManager !== null);
	assert(this.searchResults === null);

	const output = execSync(`${this.upm} search -f json ${query}`, {
		cwd: this.projectDirectory,
	});

	this.searchResults = JSON.parse(output.toString().trim());
});

Then<UPMWorld>("I should see {string} in the results", function (packageName: string) {
	assert(this.searchResults !== null);

	assert(this.searchResults.some((result) => (result['name'] as string) === packageName));
});
