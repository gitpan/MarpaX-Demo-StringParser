package MarpaX::Demo::StringParser::Actions;

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.
use open      qw(:std :utf8);    # Undeclared streams in UTF-8.
use charnames qw(:full :short);  # Unneeded in v5.16.

use Set::Array;

# Warning: Do not use Moo or anything similar.
# This class needs a sub new() due to the way
# Marpa::R2 calls the constructor.

# This is a stack initialized in MarpaX::Demo::StringParser.

our $items;

# This is a flag initialized in MarpaX::Demo::StringParser.

our $verbose;

our $VERSION = '1.03';

# ------------------------------------------------

sub edge
{
	my($stash, $edge_name) = @_;

	print "Edge: $edge_name\n" if ($verbose);

	$items -> push
	({
		name  => $edge_name,
		type  => 'edge',
		value => '',
	});

	return $edge_name;

} # End of edge.

# --------------------------------------------------

sub graph
{
	my($stash, $graph) = @_;

	return $graph;

} # End of graph.

# ------------------------------------------------

sub new
{
	my($class)   = @_;
	my($hashref) =
	{
		attribute_name => '',
		node_name      => '',
	};

	return bless $hashref, $class;

} # End of new.

# ------------------------------------------------

1;

=pod

=head1 NAME

L<MarpaX::Demo::StringParser::Actions> - Actions called from MarpaX::Demo::StringParser

=head1 Synopsis

End-users do not need to call the methods in this module.

Only L<Marpa::R2> does that, under certain conditions as specified in the grammar declared in
L<MarpaX::Demo::StringParser>.

=head1 Description

L<MarpaX::Demo::StringParser::Actions> provides a namespace for action methods used by L<MarpaX::Demo::StringParser>.

This module is a cut-down version of L<Graph::Easy::Marpa> V 2.00's Actions.pm module.

The language parsed is a cut-down version of the C<DOT> language used by AT&T's C<dot> program.
See L<http://graphviz.org>.

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

=head2 edge($edge_name)

Returns a string being the name of the edge detected by L<Marpa::R2> in the input stream.

Pushes this edge onto a stack declared in L<MarpaX::Demo::StringParser>.

The stack's elements are documented in L<MarpaX::Demo::StringParser/How is the parsed graph stored in RAM?>.

=head2 graph($graph)

Returns the result of L<Marpa::R2>'s parse.

The result is a data structure of arrayrefs of arrayrefs nested very deeply, with the depth of nesting
depending on which rules in the grammar were triggered for a given input.

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
