import { execSync } from "child_process";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const upmRepo = path.resolve(__dirname, "..");

const getNixpkg = (pkg) => {
  const res = execSync(
    `nix build --inputs-from ${upmRepo} --print-out-paths --no-link --no-warn-dirty nixpkgs#${pkg}`
  );

  return res.toString("utf-8").trim();
};

const upm = execSync(
  `nix build --print-out-paths --no-link --allow-dirty --no-warn-dirty ${upmRepo}#upm`
)
  .toString("utf-8")
  .trim();

const npm = `${getNixpkg("nodejs")}/bin/npm`;

/** @type {import('./src').UPMParameters}  */
const worldParameters = {
  bins: {
    upm,
    npm,
  },
};

/** @type {import("@cucumber/cucumber/lib/configuration").IConfiguration} */
const config = {
  worldParameters,
  require: ["dist/**/*.js"],
};

export default config;
