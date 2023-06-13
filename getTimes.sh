#!/bin/bash

###########################################################################
##### This is a wrapper for a .pl script that pulls jump times from   #####
##### log files from DTA* output in BEAST v1. Can be found at:        #####
##### Accessed 13-06-23: https://beast.community/markov_jumps_rewards #####
###########################################################################

# looping through log files to get jump times with collect_times script
# applying 40000000 burnin steps to each, over 10% for each analysis

perl="./collect_times2.pl"

for file in $(ls log/*jumpHistory.log);
	do
	stem=$(sed 's/\.jump.*//1' <<< ${file})
	echo "${stem}"
	$perl 40 < ${file} > ${stem}jumpTimes.txt
done