#!/bin/bash

set -euxo pipefail

# The step summary is reporting only, but entrypoint.sh runs under "set -e",
# so a failure here used to abort the whole action before antq.sh ever ran -
# and the snapshot submitted by scanner.sh still went through, which is how a
# permanently red workflow went unnoticed. Degrade instead of aborting.
dependency_tree_summary () {
    if ! mvn -ntp dependency:tree -Dverbose=true -DoutputFile="dependency-tree.txt"; then
        echo "WARNING: mvn dependency:tree failed for $INPUT_DIRECTORY$1, omitting the tree from the summary" >&2
        {
            echo "### $INPUT_DIRECTORY$1"
            echo ""
            echo "> :warning: \`mvn dependency:tree\` failed, dependency tree omitted."
            echo ""
        } >> "$GITHUB_STEP_SUMMARY"
        return 0
    fi
    if [[ "$INPUT_VERBOSE" == true ]]; then
        cat dependency-tree.txt
    fi
    {
        echo "### $INPUT_DIRECTORY$1"
        echo "<details>"
        echo ""
        echo "\`\`\`"
        cat dependency-tree.txt
        echo "\`\`\`"
        echo "</details>"
        echo ""
    } >> "$GITHUB_STEP_SUMMARY"
}

vulnerabilities_summary () {
    mapfile -t info_pack < <(jq -r --arg MANIFEST "$1" '.[] | select(.dependency.manifest_path == $MANIFEST and .state == "open") | (.number|tostring) + "|" + .security_vulnerability.package.name + "|" + .security_vulnerability.severity + "|" + .security_advisory.ghsa_id + "|" + .security_advisory.cve_id + "|" + .security_vulnerability.first_patched_version.identifier + "|"' <<< "$2")
    for i in "${info_pack[@]}"
    do
        IFS='|' read -r -a array_i <<< "$i" 
        cd "/${1/'pom.xml'/''}" || exit
        # An alert for a package that no longer shows up in the tree leaves grep
        # with no match, and under "set -o pipefail" that would abort the run.
        dep_level=$(mvn -ntp dependency:tree -DoutputType=dot -Dincludes="${array_i[1]}" | grep -e "->" | cut -d ">" -f 2 | cut -d '"' -f 2 | cut -d ":" -f 1-2 || true)
        IFS=' ' read -r -a dependency_level <<< "$dep_level"
        array_i+=("${dependency_level[0]}")
        table_row="| "
        counter=0
        for j in "${array_i[@]}"
        do
            if [[ $counter == 0 ]]; then
                table_row+="[$j](https://github.com/$GITHUB_REPOSITORY/security/dependabot/$j) | "
                counter=$((counter+1))
            elif [[ $counter == 1 ]]; then
                table_row+="$j | "
                counter=$((counter+1))
            elif [[ $counter == 2 ]]; then
                if [[ $j == "critical" ]] || [[ $j == "high" ]]; then
                    table_row+="‼️ $j | "
                else
                    table_row+="$j | "
                fi
                counter=$((counter+1))
            elif [[ $counter == 3 ]]; then
                table_row+="$j | "
                counter=$((counter+1))
            elif [[ $counter == 4 ]]; then
                if [[ $j = "null" ]]; then
                    table_row+="  | "
                else
                    table_row+="$j | "
                fi
                counter=$((counter+1))
            elif [[ $counter == 5 ]]; then
                table_row+="$j | "
                counter=$((counter+1))
            elif [[ $counter == 6 ]]; then
                table_row+="$j | "
                counter=$((counter+1))
            else
                continue
            fi
        done
        echo "$table_row" >> "$GITHUB_STEP_SUMMARY"
    done
}


# $1 - "project.clj" or "deps.edn"
if [[ -n $INPUT_DIRECTORY ]]; then
    if [[ "$INPUT_VERBOSE" == true ]]; then
        echo "Moving to $GITHUB_WORKSPACE$INPUT_DIRECTORY"
    fi
    cd "$GITHUB_WORKSPACE$INPUT_DIRECTORY" || exit
fi
if [[ "$INPUT_VERBOSE" == true ]]; then
        echo "Finding all $1 files"
fi
mapfile -t array < <(find . -name "$1")
if [[ $1 == "project.clj" ]]; then
  echo "## Dependency Tree" >> "$GITHUB_STEP_SUMMARY"
fi
if [[ $INPUT_INCLUDE_SUBDIRECTORIES != true ]]; then
    if [[ $1 == "project.clj" ]] && [[ "${array[*]}" == *"./project.clj"* ]]; then
        array=("./project.clj")
    elif [[ $1 == "deps.edn" ]] && [[ "${array[*]}" == *"./deps.edn"* ]]; then
        array=("./deps.edn")
    else
        array=()
    fi
fi
vul_page=$(cat /tmp/dependabot_alerts.json)
for i in "${array[@]}"
do
    if [[ "$INPUT_VERBOSE" == true ]]; then
        echo "Creating the dependency tree for $i"
    fi
    i=${i/.}
    cljdir=$GITHUB_WORKSPACE$INPUT_DIRECTORY${i//\/$1}
    if  [[ $1 == "project.clj" ]]; then
        cd "${cljdir}/projectclj" || exit
        dependency_tree_summary "$i"
        db_path="${cljdir}/projectclj/pom.xml"
        db_path=${db_path:1}
        {
            echo "| Number | Package | Severity | GHSA | CVE | Patched in | Dependency level |"
            echo "| --- | --- | --- | --- | --- | --- | --- |"
        } >> "$GITHUB_STEP_SUMMARY"
        vulnerabilities_summary "$db_path" "$vul_page"
        echo "" >> "$GITHUB_STEP_SUMMARY"
    else
        cd "${cljdir}/depsedn" || exit
        dependency_tree_summary "$i"
        db_path="${cljdir}/depsedn/pom.xml"
        db_path=${db_path:1}
        {
            echo "| Number | Package | Severity | GHSA | CVE | Patched in | Dependency level |"
            echo "| --- | --- | --- | --- | --- | --- | --- |"
        } >> "$GITHUB_STEP_SUMMARY"
        vulnerabilities_summary "$db_path" "$vul_page"
        echo "" >> "$GITHUB_STEP_SUMMARY"
    fi
done