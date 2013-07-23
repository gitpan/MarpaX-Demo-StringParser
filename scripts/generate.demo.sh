#!/bin/bash

perl -Ilib scripts/ge2tokens.pl
perl -Ilib ~/bin/ge2svg.pl
perl -Ilib scripts/generate.index.pl

cp html/*.svg html/*.html $DR/Perl-modules/html/marpax.demo.stringparser
