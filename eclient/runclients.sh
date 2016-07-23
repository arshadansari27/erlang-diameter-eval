#!/bin/bash

# This script opens 4 terminal windows.

i="0"

while [ $i -lt 4 ]
do
	echo 'Something'
	i=$[$i+1]
done

erl -pa ebin -run runner2 run &
