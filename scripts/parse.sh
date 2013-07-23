#!/bin/bash

echo Contents of $1:
cat $1
echo ----------------------------
echo Output of parser:
perl -Ilib scripts/parse.pl -i $1 -t $2 $3 $4 $5 $6 $7 $8
echo ----------------------------
echo Contents of $2:
cat $2
echo ----------------------------
