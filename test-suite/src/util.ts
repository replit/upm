import { Given, When } from "@cucumber/cucumber";
import UPMWorld from "./world";
import { writeFile } from "fs/promises";
import path from "path";

Given<UPMWorld>(
	"{word} is installed",
	function (bin: string) {
		this.install(bin);
	}
)

Given<UPMWorld>(
	"a file named {string} with:",
	async function (filename: string, content: string) {
		await writeFile(path.join(this.projectDirectory, filename), content);
	}
)

When<UPMWorld>(
	"I run {string}",
	async function (command: string) {
		await this.exec(command);
	}
)
