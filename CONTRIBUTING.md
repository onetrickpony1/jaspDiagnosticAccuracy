# Contributing

Thank you for your interest in the Diagnostic Accuracy module. It is a community module, not made or endorsed by the JASP team, and a work in progress, so reports and suggestions are welcome.

There are two ways to help. The first is for people who know R and the statistics of ROC analysis. The second is for people who would like to help with an AI coding agent (Claude Code, Codex, Cursor or similar) and are willing to spend their own tokens on it. Both are useful.

## Reporting a problem

Anyone can do this. Open an issue in this repository. Please include:

- your JASP version (Help > About) and operating system;
- what you did, what you expected and what happened instead;
- if you can, a small data set or a `.jasp` file that shows the problem. Do not share data that is private or identifies people.

For a wrong number, say which value you expected and where it comes from, for example another program or a paper.

## 1. For developers and statisticians

1. Fork the repository and create a branch for your change.
2. Set up the module as described in the README ("Installation", "As a development module").
3. Make the change. Follow the rules in [AGENTS.md](AGENTS.md): option and output names follow `dev/interface.md`, the default output stays the AUC table only, and tests compare against independently computed values, never against the module's own output.
4. Run the tests, as described in [AGENTS.md](AGENTS.md) ("Running the tests and the validation"). If you change a statistic, also run the validation script.
5. Add a line to `CHANGELOG.md` under "## Unreleased" for any change users will notice.
6. Open a pull request that says what the change does and how you checked it.

Reviews of the statistics are as welcome as code. If you think a method, a default or a statement on the help page is wrong, open an issue with your source.

## 2. With an AI coding agent

An agent can do careful, repetitive checking that takes a person hours. The module's rules are in [AGENTS.md](AGENTS.md), which most agents read on their own. Point your agent at this repository and give it one of the tasks below. Each one stands on its own.

**Tasks that help most**

- **Test the macOS file.** The macOS (Apple silicon) `.JASPModule` on the releases page has not been tested in JASP. Install it as the README describes, open the example files in `examples/`, and report what works and what does not, with screenshots.
- **Compare with other software.** Run the example data, or the data sets in `tests/testthat/`, through another program you have (for example SPSS, Stata, SAS, MedCalc or R's pROC). Compare the AUC, its interval, the cut-offs and the paired test with the module's output. Report each difference with the program, its version and the settings used.
- **Try to break it.** Make unusual data sets: very small or very large, all ties, one class missing, text values, many missing values, extreme numbers, long or non-English column names. Check that the analysis gives a clear message instead of an error or a wrong number. Report each case with its data.
- **Check the help page against the code.** Every default, range and statement in `inst/help/rocCurve.md` should match `R/` and `inst/qml/RocCurve.qml`. Report any mismatch with the file and line.

**Rules for agent work**

- Open an issue with your findings first. Open a pull request only for a change you have checked: the tests pass (AGENTS.md says how), and the pull request says how the result was verified.
- Every number needs a source you name: another program, a paper, or a calculation written out independently of the module. A value taken from the module's own output is not a check.
- Keep each issue or pull request to one topic.
- Read what your agent wrote before you post it, and remove anything private (names, local paths, your own data).
- Say in the issue or pull request that an agent did the work, and which one.

## Licence

The module is licensed under the GNU General Public License, version 3 or later (see [LICENSE](LICENSE)). By contributing, you agree that your contribution is licensed in the same way.
