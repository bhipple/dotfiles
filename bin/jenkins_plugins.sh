#!/bin/bash
# Script to get all of the installed plugins and versions of those plugins on Jenkins,
# from the cmdline.

jenkins_plugins=/var/lib/jenkins/plugins
for plugin in $jenkins_plugins/*.jpi; do
    plugin=$(basename "$plugin" .jpi)
    manifest="$jenkins_plugins/$plugin/META-INF/MANIFEST.MF"
    echo "$plugin" "$(grep 'Plugin-Version' "$manifest" | awk '{print $2}')"
done
