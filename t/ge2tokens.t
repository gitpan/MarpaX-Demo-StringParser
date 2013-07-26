#!/usr/bin/env perl

use strict;
use warnings;

use Algorithm::Diff 'diff';

use Capture::Tiny 'capture';

use File::Spec;
use File::Temp;

use MarpaX::Demo::StringParser::Filer;

use Path::Tiny; # For path().

use Perl6::Slurp; # For slurp().

use Test::More;

use Try::Tiny;

# -----------

my($data_dir_name) = 'data';
my($in_suffix)     = 'ge';
my($out_suffix)    = 'tokens';
my($test_count)    = 0;

# The EXLOCK option is for BSD-based systems.

my($temp_dir)      = File::Temp -> newdir('temp.XXXX', CLEANUP => 1, EXLOCK => 0, TMPDIR => 1);
my($temp_dir_name) = $temp_dir -> dirname;
my($script)        = path('scripts', 'parse.pl');
my(%ge_name)       = MarpaX::Demo::StringParser::Filer -> new -> get_files($data_dir_name, $in_suffix);

my($command);
my(@diff, $diff_count);
my($old_name, @old_content);
my($new_name, @new_content);
my($stdout, $stderr);
my($token_name);

# Ignore known failures.

for my $ge_name (sort keys %ge_name)
{
	$test_count++;

	$new_name = File::Spec -> catfile($temp_dir_name, $ge_name);
	$new_name .= '.tokens';
	$old_name = File::Spec -> catfile($data_dir_name, $ge_name);
	$old_name .= '.tokens';
	$ge_name  = File::Spec -> catfile($data_dir_name, $ge_name);
	$ge_name  .= '.ge';

	`$^X -Ilib $script -i $ge_name -t $new_name`;

	if (! -e $new_name)
	{
		die "Missing tokens file $new_name. ";
	}

	@old_content = slurp $old_name;
	@new_content = slurp $new_name;
	@diff        = diff(\@old_content, \@new_content);
	$diff_count  = scalar @diff;

	ok($diff_count == 0, "Compare shipped and generated: $old_name");
}

done_testing($test_count);
