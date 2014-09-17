package MarpaX::Demo::StringParser::Actions;

use strict;
use utf8;
use warnings;
use warnings qw(FATAL utf8); # Fatalize encoding glitches.

use Set::Array;

# Warning: Do not use Moo or anything similar.
# This class needs a sub new() due to the way
# Marpa::R2 calls the constructor.

# This is a flag initialized in MarpaX::Demo::StringParser.

our $verbose;

our $VERSION = '1.08';

# --------------------------------------------------

sub graph
{
	my($stash, $graph) = @_;

	return $graph;

} # End of graph.

# ------------------------------------------------

sub new
{
	my($class) = @_;

	return bless {}, $class;

} # End of new.

# ------------------------------------------------

1;

=pod

=head1 NAME

C<MarpaX::Demo::StringParser::Actions> - A namespace for MarpaX::Demo::StringParser, called via Marpa

=head1 Synopsis

End-users do not need to call the methods in this module.

Only L<Marpa::R2> does that, under certain conditions as specified in the grammar declared in
L<MarpaX::Demo::StringParser>.

=head1 Description

C<MarpaX::Demo::StringParser::Actions> provides a namespace for action methods used by L<MarpaX::Demo::StringParser>.

See L<MarpaX::Demo::StringParser> for details.

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

=head1 Constructor and Initialization

End-users do not call the methods in the module.

=head1 Methods

=head2 graph($graph)

Returns $graph.

Called as appropriate by L<Marpa::R2>.

=head2 new()

Returns a hashref, currently empty.

Called as appropriate by L<Marpa::R2>.

=head1 Machine-Readable Change Log

The file Changes was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

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
