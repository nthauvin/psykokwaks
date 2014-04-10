#!/bin/bash

[ -z "$1" ] && echo "Entrer un répertoire" && exit 1

Subdirs="$1/*/Visages $1/*/Crossmodal"

erlc mk_stats.erl || exit 1

for sub in $Subdirs; do
    echo "$sub :"
    out=$(echo $sub | sed -e 's;/;_;g')
    erl -run mk_stats run $sub/*tsv -noinput -run erlang halt >$out.tsv
done
rm -f results.zip
zip results.zip *tsv

