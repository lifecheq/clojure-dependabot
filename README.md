# Dependabot for Clojure projects

Dependabot doesn't directly support Clojure projects, but it is possible to send the dependencies list to Dependabot through GitHub [Submission API](https://docs.github.com/en/code-security/supply-chain-security/understanding-your-software-supply-chain/using-the-dependency-submission-api).

This GitHub Action looks for all `project.clj` and `deps.edn` in the repository and sends the list of dependencies (both primary dependencies and transitive dependencies) to Dependabot. If enabled, it can open pull-requests to update packages.

**Maintainers:** ☁️ Pitch Cloud Engineering Team (https://www.pitch.com)

This is a fork of [pitch-io/clojure-dependabot](https://github.com/pitch-io/clojure-dependabot).
It tracks upstream, with two behavioural differences described under
[How `deps.edn` dependencies are resolved](#how-depsedn-dependencies-are-resolved).

## How `deps.edn` dependencies are resolved

Upstream generates the pom with `clojure -X:deps mvn-pom`, which writes out the
project's top-level `:deps` and lets Maven work out everything underneath.
That reports Maven's answer rather than the one the project actually builds
with, and the two differ in both directions:

- **Alias dependencies go missing.** `mvn-pom` reads only top-level `:deps`, so
  anything under an alias's `:extra-deps` never reaches the dependency graph and
  its vulnerabilities are never reported.
- **Transitive versions disagree.** Maven picks the nearest declaration;
  tools.deps runs its own resolution. Where they differ, the graph records a
  version the project does not ship - which either hides a vulnerability or
  raises an alert that can never be closed, because the fix never shows up.

So for `deps.edn` projects this fork resolves with tools.deps instead, via
`clojure -Strace`, and writes every artifact that resolution actually selected
into a flat, fully pinned pom. Each entry excludes its own children, leaving
Maven nothing to re-resolve. `project.clj` projects still use `lein pom`.

Aliases are detected automatically: every alias declaring `:extra-deps` is
activated. Nothing in a `deps.edn` says which aliases matter, so the choice is
to over- or under-report, and over-reporting is the safer failure - you may see
an alert for a dev or test dependency that never ships.

If `deps.edn` refers to private repositories over SSH, resolution has to fetch
them, so set `SSH_PRIVATE_KEY` (see below). Without it the scan fails outright
rather than quietly skipping those dependencies.

## Required Tokens

The Action requires the following environment variables to run the [maven-dependency-submission-action](https://github.com/advanced-security/maven-dependency-submission-action) CLI and GitHub CLI to list GitHub Security Alerts (GHSA) and to create auto-pull-requests for dependencies updates. Both a Personal Access Token (PAT) and GitHub Token (`github.token`) are required: the Action needs a PAT because GitHub Token cannot be used to list security alerts for security reasons ([_"Granting access to security alerts"_](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/enabling-features-for-your-repository/managing-security-and-analysis-settings-for-your-repository#granting-access-to-security-alerts)), and the Action cannot open pull-requests as `github-actions (bot)` if it doesn't use the GitHub Token.

- GitHub Personal Access Token to run GitHub CLI (recommended privileges: `repo:all`)
- `github.token`
- `github.repository`
- `github.ref`
- `github.sha`
- `github.workspace`

Optional:

- `SSH_PRIVATE_KEY` - a private key with read access to any private repositories
  referenced as git dependencies. Only needed if `deps.edn` has any.

## Example

```
name: Dependabot for Clojure

on:
  workflow_dispatch:

env:
  GITHUB_PAT: ${{ secrets.PAT }}
  GITHUB_TOKEN: ${{ github.token }}
  GITHUB_REPOSITORY: ${{ github.repository }}
  GITHUB_REF: ${{ github.ref }}
  GITHUB_SHA: ${{ github.sha }}
  GITHUB_WORKSPACE: ${{ github.workspace }}
  # only needed if deps.edn refers to private repositories over SSH
  SSH_PRIVATE_KEY: ${{ secrets.SSH_PRIVATE_KEY }}

jobs:
  dependabot:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4
      - name: Dependabot for Clojure
        uses: pitch-io/clojure-dependabot@main
        with:
          labels: "third-party,bug"
          reviewers: "luigigubello"
          directory: "foo/bar"
```

See [`action.yml`](./action.yml) for more details on each option.

## Development

This project is configured to use [`asdf`](https://asdf-vm.com/) to manage its environment.

You can run the code in the production Docker container with `./scripts/docker.sh` or locally with `./scripts/local.sh`.
You will need to set environment variables for these scripts to run.
You can look at the source code, or run them and they will raise errors and tell you what variables to set.

**NOTE:** Running the these scripts will communicate with the GitHub API and will manage tickets and PRs.
If this is not desirable, you will need to set up a dummy repo to test against.

Test your changes by running:

```sh
bb test:bb
```

## Security

If you find a security vulnerability, please report it privately at [security@pitch.com](mailto:security@pitch.com).
