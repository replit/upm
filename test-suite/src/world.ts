import { IWorldOptions, World, setWorldConstructor } from "@cucumber/cucumber";
import assert from "assert";
import { exec as unpromisifiedExecForSomeReason } from "child_process";
import { existsSync, mkdirSync, mkdtempSync } from "fs";
import { symlink } from "fs/promises";
import { tmpdir } from "os";
import path from "path";
import { promisify } from "util";

const exec = promisify(unpromisifiedExecForSomeReason);

export interface UPMParameters {
	bins: Record<string, string>;
}

export default class UPMWorld extends World<UPMParameters> {
	/** The directory containing the PATH (/bin) and the project (/project) for each test */
	testRoot: string;

	/** explicitly-set language */
	upmLanguage?: string;

	/** bugs */
	knownBugs: string[] = [];

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

	async exec(command: string, options?: Parameters<typeof exec>[1]): Promise<string> {
		const extendEnv = options?.env ?? {};
		delete options?.env;

		const env = {
			PATH: this.binDirectory,
			...extendEnv
		};

		const envString = Object.entries(env).map(([key, value]) => `${key}=${value}`).join(' ');
		this.attach(`$ ${envString} ${command}`);

		let stderr: string;
		let stdout: string;
		try {
			const { stderr: err, stdout: out } = await exec(command, {
				cwd: this.projectDirectory,
				env,
				...options,
			});
			stderr = err.toString().trim();
			stdout = out.toString().trim();
		} catch (execError) {
			this.attach(`error: ${execError}`);
			throw new Error('error while running command')
		}

		this.attach(`stdout: ${stdout}`);
		if (stdout.length > 0) {
			this.attach(`stderr: ${stderr}`);
		}

		return stdout;
	}

	upm(command: string, options?: Parameters<typeof exec>[1]): Promise<string> {
		return this.exec(`upm ${this.upmLanguage ? '--lang ' + this.upmLanguage : ''} ${command}`, options);
	}
}

setWorldConstructor(UPMWorld);
