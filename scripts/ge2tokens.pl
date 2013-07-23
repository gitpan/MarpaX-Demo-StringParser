#!/usr/bin/env perl

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.
use open      qw(:std :utf8);    # Undeclared streams in UTF-8.
use charnames qw(:full :short);  # Unneeded in v5.16.

use Capture::Tiny 'capture';

use MarpaX::Demo::StringParser::Utils;

use Path::Tiny; # For path().

use Try::Tiny;

# ------------------------------------------------

my($data_dir_name) = 'data';
my($out_dir_name)  = $data_dir_name;
my(%ge_files)      = MarpaX::Demo::StringParser::Utils -> new -> get_files($data_dir_name, 'ge');
my($script)        = path('scripts', 'parse.pl');

my($stdout, $stderr);
my($token_name);

for my $ge_name (sort values %ge_files)
{
	$token_name = File::Spec -> catfile($out_dir_name, $ge_name);
	$token_name =~ s/ge$/tokens/;
	$ge_name    = File::Spec -> catfile($data_dir_name, $ge_name);

	print "$ge_name => $token_name \n";

	try
	{
		($stdout, $stderr) = capture{system $^X, '-Ilib', $script, '-i', $ge_name, '-t', $token_name};

		if ($stderr)
		{
			die "STDERR: $stderr\n";
		}
		else
		{
			#print "Result: $stdout\n";
		}
	}
	catch
	{
		print "$script died: $_. \n";
	};

	if (! -e $token_name)
	{
		die "Missing tokens file $token_name. ";
	}
}
