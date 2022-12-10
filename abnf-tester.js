const { program } = require("commander");
const { apgLib: ApgLib, apgApi: ApgApi, apg } = require("apg-js");
const heketClient = require("heket");

const fs = require("fs");

class RuntimeError extends Error {
  constructor(message) {
    super(message);
    this.name = this.constructor.name;
  }
}

const _loadAbnf = (file_path) => {
  try {
    return fs.readFileSync(file_path, "utf8");
  } catch (e) {
    const errorMessage = "Could not load the file!";
    console.warn(errorMessage);
    console.error(e);
    throw new RuntimeError(errorMessage);
  }
};

const __makeResult = (message, success, result) => ({
  message,
  success,
  result,
});

const heketAction = (path, testInput) => {
  const _checkAbnf = (abnf_source, test_input) => {
    try {
      const parser = heketClient.createParser(abnf_source);
      const result = parser.parse("foo");
      const success = !!result.getString();
      return !!success
        ? __makeResult("Parsed okay!", success, result)
        : __makeResult("Parsing failed!", success, result);
    } catch (e) {
      const errorMessage = "Could not process ABNF input!";
      console.warn(errorMessage);
      console.error(e);
      throw new RuntimeError(errorMessage);
    }
  };

  const abnf_source = _loadAbnf(path);
  return _checkAbnf(abnf_source, testInput);
};

const apgAction = (path, testInput) => {
  const _makeGrammarObject = (abnf_source) => {
    const grammar = new ApgApi(abnf_source);
    grammar.generate();
    if (!!grammar.errors.length) {
      console.log("GRAMMAR ERRORS:");
      console.log(grammar.errorsToAscii());
      console.log("\nORIGINAL GRAMMAR:");
      console.log(grammar.linesToAscii());
      throw new RuntimeError(`Provided grammar has errors`);
    }
    return grammar.toObject();
  };

  const _checkAbnf = (abnf_grammar, test_input) => {
    try {
      const parser = new ApgLib.parser();
      const result = parser.parse(abnf_grammar, 0, test_input);
      return !!result.success
        ? __makeResult("Parsed okay!", result.success, result)
        : __makeResult("Parsing failed!", result.success, result);
    } catch (e) {
      const errorMessage = "Could not process ABNF input!";
      console.warn(errorMessage);
      console.error(e);
      throw new RuntimeError(errorMessage);
    }
  };

  const abnf_source = _loadAbnf(path);
  const grammar = _makeGrammarObject(abnf_source);
  return _checkAbnf(grammar, testInput);
};

const actionMap = {
  apg: apgAction,
  heket: heketAction,
};

program
  .description("Test the provided ABNF grammar file.")
  .argument("<path>", "path to ABNF file to check")
  .argument("<string>", "string to check against the grammar")
  .option(
    "-s, --strategy <string>",
    "which strategy to apply, supported: apg, heket"
  )
  .action((path, testInput, options) => {
    const strategy = options.strategy || "apg";
    console.info(
      `~~\nProcessing:\n--\n${strategy}\n--\n${path}\n--\n${testInput}\n~~`
    );
    const result = actionMap[strategy](path, testInput || "");
    console.info(result);
  });

program.parse();
