FROM clojure:lein-slim-bullseye

LABEL com.github.actions.name="Dependabot for Clojure projects" \
      com.github.actions.description="Run Dependabot as GitHub Action workflow in your Clojure project."

# Install maven, antq, maven-dependency-submission cli, clojure, and gh cli
RUN apt update && \
    apt install maven libmaven-dependency-plugin-java curl jq git build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libsqlite3-dev libreadline-dev libffi-dev libbz2-dev -y && \
    rm -rf /var/lib/apt/lists/* && \
    curl -O https://download.clojure.org/install/linux-install-1.11.1.1165.sh && \
    chmod +x linux-install-1.11.1.1165.sh && \
    ./linux-install-1.11.1.1165.sh && \
    clojure -Ttools install-latest :lib com.github.liquidz/antq :as antq && \
    curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg && \
    chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg && \
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null && \
    apt update && \
    apt install gh -y

# maven-dependency-submission-linux-x64 2.0.1
# commit: 2ecce44ccb44fd4b52f43468d3644e2d3e2b3cf2
COPY maven-dependency-submission-linux /maven-dependency-submission-linux

COPY scanner.sh /scanner.sh

COPY dependabot_alerts.sh /dependabot_alerts.sh

COPY alerts_summary.sh /alerts_summary.sh

COPY antq.sh /antq.sh

COPY entrypoint.sh /entrypoint.sh

RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
