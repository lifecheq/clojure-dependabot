# Debian 11 (bullseye) reached end of life, so its security suite stopped
# serving a valid Release file and the pool packages were rotated out. From
# 2026-09-05 every build of the old clojure:lein-slim-bullseye base died at
# "apt update", which silently froze the snapshots we submit to GitHub.
# Bookworm is supported until 2028; JDK 21 matches what our projects build with.
FROM clojure:temurin-21-lein-bookworm-slim

LABEL com.github.actions.name="Dependabot for Clojure projects" \
      com.github.actions.description="Run Dependabot as GitHub Action workflow in your Clojure project."

# pom_generator.clj parses the EDN written by "clojure -Strace", so the CLI
# stays pinned alongside the org.clojure/tools.deps version in deps.edn.
ARG CLOJURE_CLI_VERSION=1.11.1.1165
ARG MAVEN_DEPENDENCY_SUBMISSION_REF=2ecce44ccb44fd4b52f43468d3644e2d3e2b3cf2

# The lein base image has no Clojure CLI, so install it alongside maven (to
# generate and inspect the pom), openssh-client (entrypoint.sh runs
# ssh-keyscan), and gh (antq.sh opens the pull requests).
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        git \
        jq \
        libmaven-dependency-plugin-java \
        maven \
        openssh-client && \
    curl -fsSL -O "https://download.clojure.org/install/linux-install-${CLOJURE_CLI_VERSION}.sh" && \
    chmod +x "linux-install-${CLOJURE_CLI_VERSION}.sh" && \
    "./linux-install-${CLOJURE_CLI_VERSION}.sh" && \
    rm "linux-install-${CLOJURE_CLI_VERSION}.sh" && \
    curl -fsSL --retry 5 --retry-max-time 120 \
        -o /usr/bin/maven-dependency-submission-linux-x64 \
        "https://github.com/advanced-security/maven-dependency-submission-action/raw/${MAVEN_DEPENDENCY_SUBMISSION_REF}/cli/maven-dependency-submission-linux-x64" && \
    chmod +x /usr/bin/maven-dependency-submission-linux-x64 && \
    clojure -Ttools install-latest :lib com.github.liquidz/antq :as antq && \
    curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
        -o /usr/share/keyrings/githubcli-archive-keyring.gpg && \
    chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg && \
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
        > /etc/apt/sources.list.d/github-cli.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends gh && \
    rm -rf /var/lib/apt/lists/*

COPY local_dependency.sh /local_dependency.sh

COPY scanner.sh /scanner.sh

COPY dependabot_alerts.sh /dependabot_alerts.sh

COPY alerts_summary.sh /alerts_summary.sh

COPY antq.sh /antq.sh

COPY entrypoint.sh /entrypoint.sh

COPY deps.edn pom_generator.clj /

RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
