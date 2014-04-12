#!/bin/bash

[ -z "$1" ] && echo "Fichier resultat ?" && exit 1
[ -z "$2" ] && echo "Répertoire metrics ?" && exit 1

Results=$1
Metrics=$2

headers=$(head -n 1 $Metrics)
echo "$headers"
while read Line; do
    Participant=$(echo "$Line" | cut -f 1 -d '	')
    Rejected=$(echo "$Line" | cut -f 5 -d '	')
    indices=""
    for r in $Rejected; do
	media=${r//visagestest/}
	i=0;
	IFS='	'
	for h in $headers; do
	    echo "$h" | grep "$media" >/dev/null && indices="$indices $i"
	    i=$(($i+1))
	done
    done
    i=0
    l=`grep "^$Participant" $Metrics`
    l2=${l// /_}
    line=()
    for c in $l2; do
	line[$i]=$c
	i=$i+1
    done
    IFS=' '
    for i in $indices; do
	line[$i]=''
    done
    for ((i=0; i<${#line[@]}; i++ )); do
	echo -ne "${line[$i]}\t"
    done
    echo
done < $Results

