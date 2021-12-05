#!/usr/bin/bash

echo -e "+----------------------------+"
echo -e "| * * * Advent Of Code * * * |"
echo -e "+----------------------------+"

day=$1
year=$2

day_name=day$day
full_path=$year/$day_name
echo -e "> Copying template to $full_path"
mkdir -p $full_path
cp template.hs $full_path/$day_name.hs

# Find inputs
echo -e "> Downoading input file"
curl --silent 'https://adventofcode.com/'$year'/day/'$day'/input' -H 'cookie: session='$AOC_COOKIE'' > $full_path/$day_name.txt

echo -e "> Creating aliases: test, run, publish1, publish2"
echo -e "
alias run=\"ghc $day_name.hs && ./$day_name\"
" > $full_path/aliases
echo -e "--------------------"
echo -e "> DONE!"
echo -e "Note: source aliases $full_path directory ($full_path/aliases)"
