#!/bin/bash

# looping through log files to get jump times with collect_times script
# applying 40000000 burnin steps to each, over 10% for each analysis

perl=~/Downloads/Phylogeo_1stwave/TH-plots/collect_times2.pl

for file in $(ls ~/Downloads/Phylogeo_1stwave/DTA/*jumpHistory.log);
	do
	stem=$(sed 's/\.jump.*//1' <<< ${file})
	echo "${stem}"
	$perl 40 < ${file} > ${stem}jumpTimes.txt
done


#for file in $(ls ~/Downloads/Phylogeo_1stwave/TH-plots/*.jumpHistory.log);
#        do
#        stem=$(sed 's/\.jump.*//1' <<< ${file})
#        echo "${stem}"
#        $perl 40 < ${file} > ${stem}jumpTimes.txt
#done
