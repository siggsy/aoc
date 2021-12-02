#!/usr/bin/bash

echo -e "Publishing level $1 with $2"
echo -e ""

curr_day=$(pwd | rev | cut -d/ -f 1-2 | rev)
day=$(echo "$curr_day" | cut -d/ -f2 | tr -dc '[0-9]')
year=$(echo "$curr_day" | cut -d/ -f1)

curl --silent 'https://adventofcode.com/'$year'/day/'$day'/answer' \
    -H 'cookie: session='$AOC_COOKIE'' \
    --data-raw 'level='$1'&answer='$2'' | htmlq --text "main article"
