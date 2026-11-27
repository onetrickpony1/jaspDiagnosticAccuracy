# Security

## Reporting a vulnerability

Please report security problems privately, not in a public issue. Use GitHub's private vulnerability reporting: on this repository's **Security** tab, choose **Report a vulnerability**. Only the maintainer sees the report.

Please include what you found, the steps to reproduce it, the affected version or commit, and what an attacker could do with it. A small proof of concept helps.

The maintainer works on this module in their own time, so there are no fixed response times. Please allow a reasonable time for a fix before you publish anything, and agree a date with the maintainer.

## Scope

In scope:

- the code in this repository (`R/`, `inst/`, `tests/`, `dev/`, the workflows in `.github/`);
- the `.JASPModule` and `.jasp` files on this repository's releases page.

Out of scope, please report these to their owners instead:

- JASP itself, its module installer and its other modules: the JASP team (https://jasp-stats.org);
- R packages the module uses, such as pROC, jaspBase and jaspGraphs: their own maintainers;
- GitHub's services: GitHub.

## Safe harbour

Security research on this module is welcome. If you act in good faith and follow this policy, the maintainer:

- considers your research authorised, and will not take or support legal action against you for it;
- will not report you to anyone for it;
- will work with you to understand and fix the problem, and credit you if you wish.

Good faith means that you:

- test only on your own computer, with your own copy of JASP, this module and your own data;
- do not access, change or delete anyone else's data or systems, and stop and report as soon as you could;
- do not test against other people's machines, the JASP team's services or GitHub's infrastructure;
- do not run denial-of-service tests, spam or social engineering;
- keep the details private until a fix is out or the agreed date has passed.

This safe harbour covers only the maintainer of this repository and only this module. It cannot give you permission on behalf of the JASP team, GitHub or anyone else. If you are unsure whether something is allowed, ask first through a private report.
