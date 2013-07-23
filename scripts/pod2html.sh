#!/bin/bash

DEST=$DR/Perl-modules/html/MarpaX/Demo

mkdir -p $DEST/StringParser

pod2html.pl -i lib/MarpaX/Demo/StringParser.pm         -o $DEST/StringParser.html
pod2html.pl -i lib/MarpaX/Demo/StringParser/Actions.pm -o $DEST/StringParser/Actions.html
pod2html.pl -i lib/MarpaX/Demo/StringParser/Config.pm  -o $DEST/StringParser/Config.html
pod2html.pl -i lib/MarpaX/Demo/StringParser/Utils.pm   -o $DEST/StringParser/Utils.html
