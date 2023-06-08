#!/bin/bash

# looping through log files to get jump times with collect_times script
# applying 40000000 burnin steps to each, over 10% for each analysis

perl="./collect_times2.pl"

for file in $(ls log/*jumpHistory.log);
	do
	stem=$(sed 's/\.jump.*//1' <<< ${file})
	echo "${stem}"
	$perl 40 < ${file} > ${stem}jumpTimes.txt
done