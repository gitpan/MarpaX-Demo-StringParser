package MarpaX::Demo::StringParser::Config;

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.
use open      qw(:std :utf8);    # Undeclared streams in UTF-8.
use charnames qw(:full :short);  # Unneeded in v5.16.

use Config::Tiny;

use File::HomeDir;

use Moo;

use Path::Tiny; # For path().

has config =>
(
	default  => sub{return {} },
	is       => 'rw',
#	isa      => 'HashRef',
	required => 0,
);

has config_file_path =>
(
	default  => sub{return ''},
	is       => 'rw',
#	isa      => 'Str',
	required => 0,
);

has section =>
(
	default  => sub{return ''},
	is       => 'rw',
#	isa      => 'Str',
	required => 0,
);

our $VERSION = '1.01';

# -----------------------------------------------

sub BUILD
{
	my($self) = @_;
	my($path) = path(File::HomeDir -> my_dist_config('MarpaX-Demo-StringParser'), '.htmarpax.demo.stringparser.conf');

	$self -> read($path);

} # End of BUILD.

# -----------------------------------------------

sub read
{
	my($self, $path) = @_;

	$self -> config_file_path($path);

	# Check [global].

	$self -> config(Config::Tiny -> read($path) );

	if (Config::Tiny -> errstr)
	{
		die Config::Tiny -> errstr;
	}

	$self -> section('global');

	if (! ${$self -> config}{$self -> section})
	{
		die "Config file '$path' does not contain the section [@{[$self -> section]}]\n";
	}

	# Check [x] where x is host=x within [global].

	$self -> section(${$self -> config}{$self -> section}{'host'});

	if (! ${$self -> config}{$self -> section})
	{
		die "Config file '$path' does not contain the section [@{[$self -> section]}]\n";
	}

	# Move desired section into config, so caller can just use $self -> config to get a hashref.

	$self -> config(${$self -> config}{$self -> section});

}	# End of read.

# --------------------------------------------------

1;

=pod

=head1 NAME

MarpaX::Demo::StringParser::Config - A config manager for use by MarpaX::Demo::StringParser

=head1 Synopsis

End-users do not need to call the methods in this module.

See L<MarpaX::Demo::StringParser>.

=head1 Description

L<MarpaX::Demo::StringParser::Config> provides a config manager for use by L<MarpaX::Demo::StringParser>.

=head1 Installation

Install L<MarpaX::Demo::StringParser> as you would for any C<Perl> module:

Run:

	cpanm MarpaX::Demo::StringParser

or run:

	sudo cpan MarpaX::Demo::StringParser

or unpack the distro, and then either:

	perl Build.PL
	./Build
	./Build test
	sudo ./Build install

or:

	perl Makefile.PL
	make (or dmake or nmake)
	make test
	make install

=head1 Methods

=head2 read()

read() is called automatically by new(). It does the actual reading of the config file.

If the file can't be read, C<die $string> is called.

The path to the config file is determined by:

	Path::Class::file(File::HomeDir -> my_dist_config('MarpaX-Demo-StringParser'), '.htmarpax.demo.stringparser.conf');

During installation, you should have run scripts/copy.config.pl, which uses the same code, to move the config file
from the config/ directory in the disto into an OS-dependent directory.

The run-time code uses this module to look in the same directory as used by scripts/copy.config.pl.

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=MarpaX::Demo::StringParser>.

=head1 Author

L<MarpaX::Demo::StringParser> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2013.

Home page: L<http://savage.net.au/index.html>.

=head1 Copyright

Australian copyright (c) 2013, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License, a copy of which is available at:
	http://www.opensource.org/licenses/index.html

=cut
