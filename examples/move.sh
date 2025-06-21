#! /bin/bash

ls examples/ | awk -v range=$1 -v delta=$2 '
/\.co$/ {
	match($1, /^[0-9]+/);
	name = substr($1, RLENGTH + 2);
	idx = substr($1, RSTART, RLENGTH)
	gsub(/^0*/, "", idx);
	idx = strtonum(idx); 

	min_idx = 1000;
	max_idx = 1000;
	if (match(range, /^[0-9]+/) != 0)
		min_idx = strtonum(substr(range, RSTART, RLENGTH)); 
	if (match(range, /[0-9]+$/) != 0) 
		max_idx = strtonum(substr(range, RSTART, RLENGTH)); 

	if (min_idx <= idx && max_idx > idx) {
		new_idx = idx + strtonum(delta)
		new_name = sprintf("%03d-%s", new_idx, name);
		cmd = sprintf("mv examples/%s examples/%s", $1, new_name);
		moves[$1] = cmd;
	}
}
END {
	asort(moves);
	for (idx in moves) print moves[idx];
	print "Accept changes? y/n";
	getline acc < "/dev/tty";
	if (acc == "y") {
		for (idx in moves) system(moves[idx]);
	}
}
'
