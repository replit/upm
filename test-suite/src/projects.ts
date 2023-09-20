import { Given, defineParameterType } from "@cucumber/cucumber";
import UPMWorld from "./world";
import path from "path";
import { existsSync } from "fs";
import copy from "recursive-copy";
import { rm } from "fs/promises";

defineParameterType({
	name: 'upm-language',
	regexp: /javascript/,
	transformer: (s) => s,
});

Given<UPMWorld>(
	"a {word} project {}",
	async function (language: string, template: string) {
		const languageTemplates = path.join(path.dirname(__dirname), 'projects', language);
		if (!existsSync(languageTemplates)) {
			throw new Error(`No templates for language ${language}`);
		}

		const templatePath = path.join(languageTemplates, template);
		if (!existsSync(templatePath)) {
			throw new Error(`No template ${template} for language ${language}`);
		}

		await copy(templatePath, this.projectDirectory);

		this.attach(`initialized project directory ${this.projectDirectory}`)
	},
);

Given<UPMWorld>(
	"no lockfile",
	async function () {
		const lockfile = this.upm('show-lockfile').toString().trim();
		await rm(path.join(this.projectDirectory, lockfile));
	}
)
