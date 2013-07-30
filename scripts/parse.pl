#!/usr/bin/env perl

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.
use open      qw(:std :utf8);    # Undeclared streams in UTF-8.
use charnames qw(:full :short);  # Unneeded in v5.16.

use MarpaX::Demo::StringParser;

use Getopt::Long;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'description=s',
	'format=s',
	'help',
	'input_file=s',
	'report_tokens=i',
	'token_file=s',
	'verbose=i',
) )
{
	pod2usage(1) if ($option{'help'});

	# Return 0 for success and 1 for failure.

	exit MarpaX::Demo::StringParser -> new(%option) -> run;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

parse.pl - Run MarpaX::Demo::StringParser::Parser.

=head1 SYNOPSIS

parse.pl [options]

	Options:
	-description graphDescription
	-help
	-input_file aGEFileName
	-report_tokens 0 or 1
	-token_file aTokenFileName
	-verbose Integer

Exit value: 0 for success, 1 for failure. Die upon error.

Typical usage:

	perl -Ilib scripts/parse.pl -d '[node]{color:blue; label: "Node name"}' -r 1 -v 1 -t output.tokens

	perl -Ilib scripts/parse.pl -i data/node.04.ge -r 1 -t node.04.tokens
	diffdata/node.04.tokens node.04.tokens

You can use scripts/parse.sh to simplify this process:

	scripts/parse.sh data/node.04.ge node.04.tokens

=head1 OPTIONS

=over 4

=item o -description graphDescription

Specify a graph description string to parse.

You are strongly encouraged to surround this string with '...' to protect it from your shell.

See also the -input_file option to read the description from a file.

The -description option takes precedence over the -input_file option.

Default: ''.

=item o -help

Print help and exit.

=item o -input_file aGEFileName

Read the graph description string from a file.

See also the -description option to read the graph description from the command line.

The whole file is slurped in as 1 graph.

The first lines of the file can start with /\s*#/, and will be discarded as comments.

The -description option takes precedence over the -input_file option.

Default: ''.

=item o -report_tokens 0 or 1

Report the tokens recognised by the parser.

This is a neat list of what is optionally written if a -token_file is specified.

Default: 0.

=item o -token_file aTokenFileName

The name of a CSV file of parsed tokens to write.

This is a permanent copy of what is reported if the -report_tokens option is set to 1.

If '', no output file will be written.

Default: ''.

=item o -verbose Integer

Print more (1, 2) or less (0) progress messages.

Default: 0.

=back

=cut
