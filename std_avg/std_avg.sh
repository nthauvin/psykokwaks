#!/bin/bash

Files=$@

[ -z "$Files" ] && echo "Fichiers .f0 ?" && exit 1
for f in $Files; do
    echo -n "$f" |sed 's;/;\t;g'
    cat "$f" | grep -v ' 0$' |\
     awk -v CONVFMT=%.17g '{sum+=$2; array[NR]=$2} END {\
      avg=sum/NR;\
      for(x=1;x<=NR;x++){\
       sumsq+=(array[x]-avg)^2;\
      }\
      print "\t" NR "\t" avg "\t" sqrt(sumsq/(NR-0))}'
done
