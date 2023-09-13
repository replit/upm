import { mkdir, writeFile } from "fs/promises";
import { Directory, Err, Ok, Result } from "./types";

const projectStructurePackageJson = {
	name: 'upm-test-project',
	version: '0.0.1',
	license: 'UNLICENSED',
};

const projectStructure: Directory['contents'] = {
	'index.ts': {
		type: 'file',
		contents: `
console.log('Hello, world!');
`,
	},
	'package.json': {
		type: 'file',
		contents: JSON.stringify(projectStructurePackageJson, null, 2),
	}
};

export async function initProjectDirectory({
	projectDirectory,
}: {
	projectDirectory: string;
}): Promise<Result<true, 'unexpected project structure'>> {
	for (const path in projectStructure) {
		const { type, contents } = projectStructure[path];

		switch (type) {
			case 'file':
				await writeFile(`${projectDirectory}/${path}`, contents);
				break;

			case 'directory':
				await mkdir(`${projectDirectory}/${path}`);
				break;

			default:
				return Err('unexpected project structure');
		}
	}

	return Ok(true);
}
