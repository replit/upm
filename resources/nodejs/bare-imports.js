// This script is called with two command-line arguments: a directory
// to search for module requires, and a comma-delimited list of names
// to ignore in the filesystem traversal. It prints to stdout a list
// of modules that were required, excluding builtins, local modules,
// etc.

const builtinModules = require("module").builtinModules;
const fs = require("fs");
const path = require("path");
const process = require("process");
const util = require("util");

const babelParser = require("./babel-parser");

// Walk filesystem tree rooted at directory in parallel. For each file
// in the tree, invoke callback. Resolve once all callbacks resolve.
// Ignore files and directories whose basenames are keys in ignored.
async function walkTree({ directory, callback, ignored }) {
  const dirents = (await util.promisify(fs.readdir)(directory, {
    withFileTypes: true
  })).filter(d => !ignored[d.name]);

  const fromFilesP = Promise.all(
    dirents
      .filter(d => d.isFile() && d.name.match(/\.[jt]sx?$/))
      // Does not follow symlinks.
      .map(async d => callback(path.join(directory, d.name)))
  );
  const fromDirectoriesP = Promise.all(
    dirents
      .filter(d => d.isDirectory())
      .map(d =>
        walkTree({ directory: path.join(directory, d.name), callback, ignored })
      )
  );

  const fromFiles = await fromFilesP;
  const fromDirectories = await fromDirectoriesP;

  fromDirectories.push(fromFiles);
  return [].concat.apply([], fromDirectories);
}

// Return list of modules required by code in the given filename, or
// null if there was a parse error.
async function parseFile(filename) {
  const code = await util.promisify(fs.readFile)(filename, {
    encoding: "utf-8"
  });
  let ast;

  try {
    ast = babelParser.parse(code, {
      // Maximum permissibility because the bare imports search is
      // just to try to help out the user; we don't need the code to
      // be totally valid. See
      // <https://babeljs.io/docs/en/next/babel-parser.html>.
      allowImportExportEverywhere: true,
      allowAwaitOutsideFunction: true,
      allowReturnOutsideFunction: true,
      allowSuperOutsideMethod: true,
      allowUndeclaredExports: true,
      sourceType: "unambiguous",
      plugins: [
        "asyncGenerators",
        "bigInt",
        "classProperties",
        "classPrivateProperties",
        "classPrivateMethods",
        "doExpressions",
        "dynamicImport",
        "exportDefaultFrom",
        "exportNamespaceFrom",
        "functionBind",
        "functionSent",
        "importMeta",
        "logicalAssignment",
        "nullishCoalescingOperator",
        "numericSeparator",
        "objectRestSpread",
        "optionalCatchBinding",
        "optionalChaining",
        "partialApplication",
        "throwExpressions"
      ]
    });
  } catch (err) {
    return null;
  }

  return parseNode(ast);
}

// Return list of modules required by code in the given Babel AST
// node, array, or other object.
function parseNode(node) {
  if (Array.isArray(node)) {
    return [].concat.apply([], node.map(parseNode));
  }

  if (typeof node !== "object" || node === null) {
    return [];
  }

  if (
    node.type === "ImportDeclaration" &&
    node.source.type === "StringLiteral"
  ) {
    return [node.source.value];
  }

  if (
    node.type === "CallExpression" &&
    (node.callee.type === "Import" ||
      (node.callee.type === "Identifier" && node.callee.name === "require")) &&
    node.arguments.length === 1 &&
    node.arguments[0].type === "StringLiteral"
  ) {
    return [node.arguments[0].value];
  }

  return [].concat.apply([], Object.values(node).map(parseNode));
}

// Given list of arguments, do top-level logic and return or throw
// error.
async function commandLine(args) {
  if (args.length != 2) {
    throw new Error("usage: node bare-imports.js DIRECTORY IGNORED");
  }

  const [directory, ignoredList] = args;

  const ignored = {};
  for (const name of ignoredList.split(",")) {
    ignored[name] = true;
  }

  let hadErrors = false;
  const modules = {};
  await walkTree({
    directory,
    callback: async filename => {
      const foundModules = await parseFile(filename);
      if (foundModules === null) {
        hadErrors = true;
      } else {
        for (const module of foundModules) {
          modules[module] = true;
        }
      }
    },
    ignored
  });

  const filteredModules = {};
  for (let module in modules) {
    // Skip modules loaded via plugins; see
    // <https://stackoverflow.com/a/34930235/3538165>.
    if (module.indexOf("!") != -1) {
      continue;
    }

    // Prevent our code from crashing.
    if (module === "") {
      continue;
    }

    // Skip local modules.
    if (module[0] === ".") {
      continue;
    }

    // Resolve "express/router" into just "express", but
    // "@types/express/router" into "@types/express".
    if (module[0] === "@") {
      module = module
        .split("/")
        .slice(0, 2)
        .join("/");
    } else {
      module = module.split("/")[0];
    }

    filteredModules[module] = true;
  }

  for (const module of builtinModules) {
    delete filteredModules[module];
  }

  console.log(
    JSON.stringify({
      modules: Object.keys(filteredModules),
      success: !hadErrors
    })
  );
}

// process.argv has only one element in the case that this module is
// loaded via 'require', and at least two if it is run as a script.
if (process.argv.length >= 2) {
  commandLine(process.argv.slice(2))
    .then(_ => process.exit(0))
    .catch(err => {
      console.error(err);
      process.exit(1);
    });
}

module.exports = { parseFile, parseNode };
