# Contributing

Thank you for your interest in the Diagnostic Accuracy module. The module is a work in progress, so reports and suggestions are welcome.

## Reporting a problem

Open an issue in this repository. Please include:

- your JASP version (Help > About) and operating system;
- what you did, what you expected and what happened instead;
- if you can, a small data set or a `.jasp` file that shows the problem. Do not share data that is private or identifies people.

For a wrong number, say which value you expected and where it comes from, for example another program or a paper.

## Suggesting a change

1. Fork the repository and create a branch for your change.
2. Set up the module as described in the README ("Installation").
3. Make the change. Follow the rules in [AGENTS.md](AGENTS.md): option and output names follow `dev/interface.md`, the default output stays the AUC table only, and tests compare against independently computed values.
4. Run the tests, as described in [AGENTS.md](AGENTS.md) ("Running the tests and the validation"). If you change a statistic, also run the validation script.
5. Add a line to `CHANGELOG.md` under "## Unreleased" for any change users will notice.
6. Open a pull request that says what the change does and how you checked it.

## Licence

The module is licensed under the GNU General Public License, version 3 or later (see [LICENSE](LICENSE)). By contributing, you agree that your contribution is licensed in the same way.
