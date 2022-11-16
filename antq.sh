#!/bin/bash

update_package () {
    clojure -Sdeps '{:deps {com.github.liquidz/antq {:mvn/version "RELEASE"}}}' -M -m antq.core --upgrade --force --directory "$1" --focus="$2"
    git checkout -b "dependabot/clojure${1/$GITHUB_WORKSPACE}/$4-$5-$6"
    git add "$3"
    git commit -m "Bump $2"
    git push --set-upstream origin "dependabot/clojure${1/$GITHUB_WORKSPACE}/$4-$5-$6"
    echo "Bump $2 $7 to $5"
    if [[ $8 != "null" ]]; then
        gh pr create -B "$INPUT_MAIN_BRANCH" --title "Bump $2 $7 to $5" -b "Bumps **$2** from $7 to $5.</br>*Changelog:* $8.</br></br>---</br></br>Pull request generated by Github Action \"Dependabot for Clojure projects\". Auto-rebase is currently not supported, so it is recommended to rebase before merging to prevent conflicts." -l "$INPUT_LABELS" -r "$INPUT_REVIEWERS"
    else
        gh pr create -B "$INPUT_MAIN_BRANCH" --title "Bump $2 $7 to $5" -b "Bumps **$2** from $7 to $5.</br></br>---</br></br>Pull request generated by Github Action \"Dependabot for Clojure projects\". Auto-rebase is currently not supported, so it is recommended to rebase before merging to prevent conflicts." -l "$INPUT_LABELS" -r "$INPUT_REVIEWERS"
    fi
    git checkout "$INPUT_MAIN_BRANCH"
}

# $1 - "project.clj" or "deps.edn"
if [[ $INPUT_AUTO_PULL_REQUEST == true ]]; then
    if [[ -n $INPUT_DIRECTORY ]]; then
        cd "$GITHUB_WORKSPACE$INPUT_DIRECTORY" || exit
    fi
    mapfile -t array < <(find . -name "$1")
    for i in "${array[@]}"
    do
        i=${i/.}
        cljdir=$GITHUB_WORKSPACE$INPUT_DIRECTORY${i//\/$1}
        cd "$cljdir" || exit
        clojure -Sdeps '{:deps {com.github.liquidz/antq {:mvn/version "RELEASE"}}}' -M -m antq.core --reporter="json" > /tmp/antq-report.json || true
        length=$(jq '. | length' /tmp/antq-report.json)
        length=$((length-1))
        for j in $(seq 0 $length);
        do
            fileType=$(jq -r ".[$j] .file" /tmp/antq-report.json)
            if  [[ $fileType == "$1" ]]; then
                name=$(jq -r ".[$j] .name" /tmp/antq-report.json)
                version=$(jq -r ".[$j] .version" /tmp/antq-report.json)
                latestVersion=$(jq -r ".[$j] .\"latest-version\"" /tmp/antq-report.json)
                changesUrl=$(jq -r ".[$j] .\"changes-url\"" /tmp/antq-report.json)
                time=$(date +%s)
                escapedName=$(echo "$name" | tr "/" "-")
                prefix="dependabot/clojure${cljdir/$GITHUB_WORKSPACE}/$escapedName-$latestVersion-"
                git fetch
                mapfile -t branches < <(git branch -r | grep "$prefix")
                if [[ ${branches[*]} ]]; then
                    prTime=()
                    for k in "${branches[@]}"
                    do
                        prTime+=("${k//origin\/$prefix/}")
                    done
                    IFS=" " read -r -a lastBranch <<< "$(echo "${prTime[*]}" | xargs -n1 | sort -nr | xargs)"
                    statusPr=$(gh pr list --head "$prefix${lastBranch[0]}" --state open --json title | jq ". | length")
                    if [[ $statusPr -lt 1 ]]; then
                        update_package "$cljdir" "$name" "$1" "$escapedName" "$latestVersion" "$time" "$version" "$changesUrl"
                    else
                        git checkout "$INPUT_MAIN_BRANCH"
                    fi
                else
                    update_package "$cljdir" "$name" "$1" "$escapedName" "$latestVersion" "$time" "$version" "$changesUrl"
                fi
            fi
        done
    done
fi
