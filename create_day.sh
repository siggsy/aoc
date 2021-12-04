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
echo -e "> Downoading input files"
echo -e ">> Example input"
example_test=$(curl --silent 'https://adventofcode.com/'$year'/day/'$day'' -H 'coockie: session='$AOC_COOKIE'' | htmlq "pre code")
START=$(echo "$example_test" | grep -n -m1 "<code" | cut -d ':' -f1)
END=$(echo "$example_test" | grep -n -m1 "</code" | cut -d ':' -f1)
echo "$example_test" | sed -n -e "$START,$END p" | htmlq --text | head -n -1 > $full_path/$day_name.test.txt

echo -e ">> Actual input"
curl --silent 'https://adventofcode.com/'$year'/day/'$day'/input' -H 'cookie: session='$AOC_COOKIE'' > $full_path/$day_name.txt

echo -e "> Creating aliases: test, run, publish1, publish2"
echo -e "
alias test=\"ghc $day_name.hs && ./$day_name < $day_name.test.txt\"
alias run=\"ghc $day_name.hs && ./$day_name < $day_name.txt\"
alias publish1=\"../../publish.sh 1\"
alias publish2=\"../../publish.sh 2\"
" > $full_path/aliases
echo -e "--------------------"
echo -e "> DONE!"
echo -e "Note: source aliases $full_path directory ($full_path/aliases)"
