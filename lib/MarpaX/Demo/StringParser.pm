package MarpaX::Demo::StringParser;

use strict;
use utf8;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.
use open      qw(:std :utf8);    # Undeclared streams in UTF-8.
use charnames qw(:full :short);  # Unneeded in v5.16.

# The next line is mandatory, else
# the action names cannot be resolved.

use MarpaX::Demo::StringParser::Actions;

use Marpa::R2;

use Moo;

use Set::Array;

use Text::CSV;

use Try::Tiny;

has description =>
(
	default  => sub{return ''},
	is       => 'rw',
#	isa      => 'Str',
	required => 0,
);

has grammar =>
(
	default  => sub{return ''},
	is       => 'rw',
#	isa      => 'Marpa::R2::Scanless::G',
	required => 0,
);

has graph_text =>
(
	default  => sub{return ''},
	is       => 'rw',
#	isa      => 'Str',
	required => 0,
);

has input_file =>
(
	default  => sub{return ''},
	is       => 'rw',
#	isa      => 'Str',
	required => 0,
);

has items =>
(
	default  => sub{return ''},
	is       => 'rw',
#	isa      => 'Set::Array',
	required => 0,
);

has recce =>
(
	default  => sub{return ''},
	is       => 'rw',
#	isa      => 'Marpa::R2::Scanless::R',
	required => 0,
);

has report_tokens =>
(
	default  => sub{return 0},
	is       => 'rw',
#	isa      => 'Int',
	required => 0,
);

has token_file =>
(
	default  => sub{return ''},
	is       => 'rw',
#	isa      => 'Str',
	required => 0,
);

has verbose =>
(
	default  => sub{return 0},
	is       => 'rw',
#	isa      => 'Int',
	required => 0,
);

our $VERSION = '1.05';

# ------------------------------------------------

sub attribute_list
{
	my($self, $attribute_list) = @_;
	my(@char)          = split(//, $attribute_list);
	my($inside_name)   = 1;
	my($inside_value)  = 0;
	my($quote)         = '';
	my($name)          = '';
	my($previous_char) = '';

	my($char);
	my(%attribute);
	my($key);
	my($value);

	for my $i (0 .. $#char)
	{
		$char = $char[$i];

		# Name matches /^[a-zA-Z_]+$/.

		if ($inside_name)
		{
			next if ($char =~ /\s/);

			if ($char eq ':')
			{
				print "Attribute name: $name\n" if ($self -> verbose > 1);

				$inside_name = 0;
				$key         = $name;
				$name        = '';
			}
			elsif ($char =~ /[a-zA-Z_]/)
			{
				$name .= $char;
			}
			else
			{
				die "The char '$char' is not allowed in the names of attributes\n";
			}
		}
		elsif ($inside_value)
		{
			if ($char eq $quote)
			{
				# Get out of quotes if matching one found.
				# But, ignore an escaped quote.
				# The first 2 backslashes are just to fix syntax highlighting in UltraEdit.

				if ($char =~ /[\"\']/)
				{
					if ($previous_char ne '\\')
					{
						$quote = '';
					}
				}
				else
				{
					if ( (substr($value, 0, 2) eq '<<') && ($i > 0) && ($char[$i - 1]) eq '>')
					{
						$quote = '';
					}
					elsif ( (substr($value, 0, 1) eq '<') && (substr($value, 1, 1) ne '<') && ($previous_char ne '\\') )
					{
						$quote = '';
					}
				}

				$value .= $char;
			}
			elsif ( ($char eq ';') && ($quote eq '') )
			{
				if ($previous_char eq '\\')
				{
					$value .= $char;
				}
				else
				{
					$attribute{$key} = $value;

					print "Attribute value: $value\n" if ($self -> verbose > 1);

					$inside_name  = 1;
					$inside_value = 0;
					$quote        = '';
					$key          = '';
					$value        = '';
				}
			}
			else
			{
				$value .= $char;
			}
		}
		else # After name and ':' but before label.
		{
			next if ($char =~ /\s/);

			$inside_value = 1;
			$value        = $char;

			# Look out for quotes, amd make '<' match '>'.
			# The backslashes are just to fix syntax highlighting in UltraEdit.
			# Also, this being the 1st char in the value, there can't be a '\' before it.

			if ($char =~ /[\"\'<]/)
			{
				$quote = $char eq '<' ? '>' : $char;
			}
		}

		$previous_char = $char;
	}

	# Beware {a:b;}. In this case, the ';' leaves $key eq ''.

	if (length $key)
	{
		$attribute{$key} = $value;

		print "Attribute value: $value\n" if ($self -> verbose > 1);
	}

	for $key(sort keys %attribute)
	{
		$value = $attribute{$key};
		$value =~ s/\s+$//;

		# The first 2 backslashes are just to fix syntax highlighting in UltraEdit.

		$value =~ s/^([\"\'])(.*)\1$/$2/;

		print "Attribute: $key => $value\n" if ($self -> verbose);

		$self -> items -> push
		({
			name  => $key,
			type  => 'attribute',
			value => $value,
		});
	}

} # End of attribute_list.

# --------------------------------------------------
# References from an email from Jeffrey to the Marpa Google Groups list:
# Check out the SLIF grammar:
# <https://github.com/jeffreykegler/Marpa--R2/blob/master/cpan/lib/Marpa/R2/meta/metag.bnf>.
# It's full of stuff you can steal, including rules for quoted strings.
# The basic idea is that strings must be G0 lexemes, not assembled in G1 as your (Paul Bennett) gist has it.
# Jean-Damien's C language BNF:
# <https://github.com/jddurand/MarpaX-Languages-C-AST/blob/master/lib/MarpaX/Languages/C/AST/Grammar/ISO_ANSI_C_2011.pm>
# is also full of stuff to do all the C syntax, including strings and C-style comments. -- jeffrey

sub BUILD
{
	my($self) = @_;

	$self -> items(Set::Array -> new);

	# Ensure we can report from the action_object.

	$MarpaX::Demo::StringParser::Actions::verbose = $self -> verbose;

	$self -> grammar
	(
		Marpa::R2::Scanless::G -> new
		({
action_object			=> 'MarpaX::Demo::StringParser::Actions',
source					=> \(<<'END_OF_SOURCE'),

:default				::= action => [values]

# Overall stuff.

:start 					::= graph_grammar

graph_grammar			::= graph_definition	action => graph

# Graph stuff.

graph_definition		::= node_definition
							| edge_definition
# Node stuff

node_definition			::= node_statement
							| node_statement graph_definition

node_statement			::= node_name
							| node_name attribute_definition
							| node_statement (',') node_statement

node_name				::= start_node end_node

:lexeme					~ start_node		pause => before		event => start_node
start_node				~ '['

:lexeme					~ end_node
end_node				~ ']'

# Edge stuff

edge_definition			::= edge_statement
							| edge_statement graph_definition

edge_statement			::= edge_name
							| edge_name attribute_definition
							| edge_statement (',') edge_statement

edge_name				::= directed_edge
							| undirected_edge

:lexeme					~ directed_edge		pause => before		event => directed_edge
directed_edge			~ '->'

:lexeme					~ undirected_edge	pause => before		event => undirected_edge
undirected_edge			~ '--'

# Attribute stuff.

attribute_definition	::= attribute_statement*

attribute_statement		::= start_attributes end_attributes

:lexeme					~ start_attributes	pause => before		event => start_attributes
start_attributes		~ '{'

:lexeme					~ end_attributes
end_attributes			~ '}'

# Boilerplate.

:discard				~ whitespace
whitespace				~ [\s]+

END_OF_SOURCE
		})
	);

	$self -> recce
	(
		Marpa::R2::Scanless::R -> new
		({
			grammar => $self -> grammar
		})
	);

} # End of BUILD.

# ------------------------------------------------

sub edge
{
	my($self, $edge_name) = @_;

	print "Edge: $edge_name\n" if ($self -> verbose);

	$self -> items -> push
	({
		name  => $edge_name,
		type  => 'edge',
		value => '',
	});

} # End of edge.

# -----------------------------------------------
# $target is either qr/]/ or qr/}/, and allows us to handle
# both node names and either edge or node attributes.
# The special case is <<...>>, as used in attributes.

sub find_terminator
{
	my($self, $stringref, $target, $start) = @_;
	my(@char)   = split(//, substr($$stringref, $start) );
	my($offset) = 0;
	my($quote)  = '';
	my($angle)  = 0; # Set to 1 if inside <<...>>.

	my($char);

	for my $i (0 .. $#char)
	{
		$char   = $char[$i];
		$offset = $i;

		if ($quote)
		{
			# Ignore an escaped quote.
			# The first 2 backslashes are just to fix syntax highlighting in UltraEdit.

			next if ( ($char =~ /[\]\"\'>]/) && ($i > 0) && ($char[$i - 1] eq '\\') );

			# Get out of quotes if matching one found.

			if ($char eq $quote)
			{
				if ($quote eq '>')
				{
					$quote = '' if (! $angle || ($char[$i - 1] eq '>') );

					next;
				}

				$quote = '';

				next;
			}
		}
		else
		{
			# Look for quotes.
			# 1: Skip escaped chars.

			next if ( ($i > 0) && ($char[$i - 1] eq '\\') );

			# 2: " and '.
			# The backslashes are just to fix syntax highlighting in UltraEdit.

			if ($char =~ /[\"\']/)
			{
				$quote = $char;

				next;
			}

			# 3: <.
			# In the case of attributes ($target eq '}') but not nodes names,
			# quotes can be <...> or <<...>>.

			if ( ($target =~ '}') && ($char =~ '<') )
			{
				$quote = '>';
				$angle = 1 if ( ($i < $#char) && ($char[$i + 1] eq '<') );

				next;
			}

			last if ($char =~ $target);
		}
	}

	return $start + $offset;

} # End of find_terminator.

# -----------------------------------------------

sub format_token
{
	my($self, $item) = @_;
	my($format) = '%4s  %-13s  %-s';
	my($value)  = $$item{name};
	$value      = "$value => $$item{value}" if (length($$item{value}) > 0);

	return sprintf($format, $$item{count}, $$item{type}, $value);

} # End of format_token.

# --------------------------------------------------

sub generate_token_file
{
	my($self, $file_name) = @_;
	my($csv) = Text::CSV -> new
	({
		always_quote => 1,
		binary       => 1,
	});

	open(OUT, '>', $file_name) || die "Can't open(> $file_name): $!";

	# Don't call binmode here, because we're already using it.

	$csv -> print(\*OUT, ['key', 'name', 'value']);
	print OUT "\n";

	for my $item ($self -> items -> print)
	{
		$csv -> print(\*OUT, [$$item{type}, $$item{name}, $$item{value}]);
		print OUT "\n";
	}

	close OUT;

} # End of generate_token_file.

# --------------------------------------------------

sub get_graph_from_command_line
{
	my($self) = @_;

	$self -> graph_text($self -> description);

} # End of get_graph_from_command_line.

# --------------------------------------------------

sub get_graph_from_file
{
	my($self) = @_;

	# This code accepts utf8 data, due to the standard preamble above.

	open(INX, $self -> input_file) || die "Can't open input file(" . $self -> input_file . "): $!\n";
	my(@line) = <INX>;
	close INX;
	chomp @line;

	shift(@line) while ( ($#line >= 0) && ($line[0] =~ /^\s*#/) );

	$self -> graph_text(join(' ', @line) );

} # End of get_graph_from_file.

# ------------------------------------------------

sub node
{
	my($self, $node_name) = @_;
	$node_name =~ s/^\s+//;
	$node_name =~ s/\s+$//;

	# The first 2 backslashes are just to fix syntax highlighting in UltraEdit.

	$node_name =~ s/^([\"\'])(.*)\1$/$2/;

	print "Node: $node_name\n" if ($self -> verbose);

	$self -> items -> push
	({
		name  => $node_name,
		type  => 'node',
		value => '',
	});

	if ($node_name eq '')
	{
		$self -> items -> push
		({
			name  => 'color',
			type  => 'attribute',
			value => 'invis',
		});
	}

} # End of node.

# --------------------------------------------------

sub process
{
	my($self)   = @_;
	my($string) = $self -> graph_text;
	my($length) = length $string;

	# We use read()/lexeme_read()/resume() because we pause at each lexeme.

	my($attribute_list);
	my($do_lexeme_read);
	my(@event, $event_name);
	my($lexeme_name, $lexeme);
	my($node_name);
	my($span, $start);

	for
	(
		my $pos = $self -> recce -> read(\$string);
		$pos < $length;
		$pos = $self -> recce -> resume($pos)
	)
	{
		print "read() => pos: $pos\n" if ($self -> verbose > 1);

		$do_lexeme_read = 1;
		@event          = @{$self -> recce -> events};
		$event_name     = ${$event[0]}[0];
		($start, $span) = $self -> recce -> pause_span;
		$lexeme_name    = $self -> recce -> pause_lexeme;
		$lexeme         = $self -> recce -> literal($start, $span);

		print "pause_span($lexeme_name) => start: $start. span: $span. " .
			"lexeme: $lexeme. event: $event_name\n" if ($self -> verbose > 1);

		if ($event_name eq 'start_attributes')
		{
			# Read the attribute_start lexeme, but don't do lexeme_read()
			# at the bottom of the for loop, because we're just about
			# to fiddle $pos to skip the attributes.

			$pos            = $self -> recce -> lexeme_read($lexeme_name);
			$pos            = $self -> find_terminator(\$string, qr/}/, $start);
			$attribute_list = substr($string, $start + 1, $pos - $start - 1);
			$do_lexeme_read = 0;

			print "index() => attribute list: $attribute_list\n" if ($self -> verbose > 1);

			$self -> attribute_list($attribute_list);
		}
		elsif ($event_name eq 'start_node')
		{
			# Read the node_start lexeme, but don't do lexeme_read()
			# at the bottom of the for loop, because we're just about
			# to fiddle $pos to skip the node's name.

			$pos            = $self -> recce -> lexeme_read($lexeme_name);
			$pos            = $self -> find_terminator(\$string, qr/]/, $start);
			$node_name      = substr($string, $start + 1, $pos - $start - 1);
			$do_lexeme_read = 0;

			print "index() => node name: $node_name\n" if ($self -> verbose > 1);

			$self -> node($node_name);
		}
		elsif ($event_name eq 'directed_edge')
		{
			$self -> edge($lexeme);
		}
		elsif ($event_name eq 'undirected_edge')
		{
			$self -> edge($lexeme);
		}
		else
		{
			die "Unexpected lexeme '$lexeme_name' with a pause\n";
		}

		$pos = $self -> recce -> lexeme_read($lexeme_name) if ($do_lexeme_read);

		print "lexeme_read($lexeme_name) => $pos\n" if ($self -> verbose > 1);
    }

	# Return a defined value for success and undef for failure.

	return $self -> recce -> value;

} # End of process.

# -----------------------------------------------

sub renumber_items
{
	my($self)  = @_;
	my(@item)  = @{$self -> items};
	my($count) = 0;

	my(@new);

	for my $item (@item)
	{
		$$item{count} = ++$count;

		push @new, $item;
	}

	$self -> items(Set::Array -> new(@new) );

} # End of renumber_items.

# -----------------------------------------------

sub report
{
	my($self) = @_;

	print $self -> format_token
	({
		count => 'Item',
		name  => 'Name',
		type  => 'Type',
		value => '',
	}), "\n";

	for my $item ($self -> items -> print)
	{
		print $self -> format_token($item), "\n";
	}

} # End of report.

# --------------------------------------------------

sub run
{
	my($self) = @_;

	if ($self -> description)
	{
		$self -> get_graph_from_command_line;
	}
	elsif ($self -> input_file)
	{
		$self -> get_graph_from_file;
	}
	else
	{
		die "Error: You must provide a graph using one of -input_file or -description\n";
	}

	# Return 0 for success and 1 for failure.

	my($result) = 0;

	try
	{
		if (defined $self -> process)
		{
			$self -> renumber_items;
			$self -> report if ($self -> report_tokens);

			my($file_name) = $self -> token_file;

			$self -> generate_token_file($file_name) if ($file_name);
		}
		else
		{
			$result = 1;

			print "Parse failed\n";
		}
	}
	catch
	{
		$result = 1;

		print "Parse failed. Error: $_\n";
	};

	# Return 0 for success and 1 for failure.

	print "Parse result: $result (0 is success)\n" if ($self -> verbose);

	return $result;

} # End of run.

# --------------------------------------------------

1;

=pod

=head1 NAME

L<MarpaX::Demo::StringParser> - Conditional preservation of whitespace while parsing

=head1 Synopsis

Typical usage:

	perl -Ilib scripts/parse.pl -d '[node]{color:blue; label: "Node name"}' -r 1 -v 1 -t output.tokens

The following refer to data shipped with the distro:

	perl -Ilib scripts/parse.pl -i data/node.04.ge -r 1 -t node.04.tokens
	diff data/node.04.tokens node.04.tokens

You can use scripts/parse.sh to simplify this process:

	scripts/parse.sh data/node.04.ge node.04.tokens -r 1

See L<the demo page|http://savage.net.au/Perl-modules/html/marpax.demo.stringparser/> for sample output.

Also, there is L<an article|http://savage.net.au/Ron/html/Conditional.preservation.of.whitespace.html> based on
this module.

=head1 Description

This module demonstrations how to use L<Marpa::R2>'s capabilities to have Marpa repeatedly pass control back to code
in your own module, during the parse, to handle certain cases where you don't want Marpa's default processing to occur.

Specifically, it deals with the classic case of when you wish to preserve whitespace in some contexts, but also
want Marpa to discard whitespace in all other contexts.

Note that this module's usage of Marpa's adverbs I<event> and I<pause> should be regarded as an intermediate/advanced
technique. For people just beginning to use Marpa, use of the I<action> adverb is the recommended technique.

The article mentioned above discusses important issues regarding the timing sequence of I<pauses> and I<actions>.

All this assumes a relatively recent version of Marpa, one in which its Scanless interface (SLIF) is implemented.
All my development was done using L<Marpa::R2> V 2.064000.

Lastly, L<MarpaX::Demo::StringParser> is a cut-down version of L<Graph::Easy::Marpa> V 2.00, and (the former)
provides a Marpa-based parser for parts of L<Graph::Easy::Marpa>-style graph definitions. The latter module handles
the whole Graph::Easy::Marpa language.

See L<Graph::Easy::Marpa::Parser/What is the Graph::Easy::Marpa language?> for details.
And see below, L</What is the grammar parsed by this module?>, for details of the parts supported by this module.

In pragmatic terms, the code in the current module was developed for inclusion in L<Graph::Easy::Marpa>, which in
turn is a pre-processor for the L<DOT|http://graphviz.org/content/dot-language> language.

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

=head1 Scripts Shipped with this Module

All scripts are shipped in the scripts/ directory.

=over 4

=item o copy.config.pl

This is for use by the author. It just copies the config file out of the distro, so the script generate.index.pl
(which uses HTML template stuff) can find it.

=item o find.config.pl

This cross-checks the output of copy.config.pl.

=item o ge2tokens.pl

This transforms all data/*.ge files into their corresponding data/*.tokens files.

=item o generate.demo.sh

This runs:

=over 4

=item o perl -Ilib scripts/ge2tokens.pl

=item o perl -Ilib ~/bin/ge2svg.pl

See the article mentioned in the Synopsis for details on this script. Briefly, it is not included in the distro
because it has Graph::Easy::Marpa::Renderer::GraphViz2 as a pre-req.

=item o perl -Ilib scripts/generate.index.pl

=back

And then generate.demo.sh copies the demo output to my dev web server's doc root, where I can cross-check it.

=item o generate.index.pl

This constructs a web page containing all the html/*.svg files.

=item o parse.pl

This runs a parse on a single input file. Run .parse.pl -h' for details.

=item o parse.sh

This simplifies running parse.pl.

=item o pod2html.sh

This converts all lib/*.pm files into their corresponding *.html versions, for proof-reading and uploading
to my real web site.

=back

=head1 Constructor and Initialization

C<new()> is called as C<< my($parser) = MarpaX::Demo::StringParser -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<MarpaX::Demo::StringParser>.

Key-value pairs accepted in the parameter list (see corresponding methods for details
[e.g. description($graph)]):

=over 4

=item o description => '[node.1]->[node.2]'

Specify a string for the graph definition.

You are strongly encouraged to surround this string with '...' to protect it from your shell if using
this module directly from the command line.

See also the I<input_file> key which reads the graph from a file.

The I<description> key takes precedence over the I<input_file> key.

Default: ''.

=item o input_file => $graph_file_name

Read the graph definition from this file.

See also the I<description> key to read the graph from the command line.

The whole file is slurped in as a single graph.

The first lines of the file can start with /^\s*#/, and will be discarded as comments.

The I<description> key takes precedence over the I<input_file> key.

Default: ''.

=item o report_tokens => $Boolean

When set to 1, calls L</report()> to print the items recognized by the parser.

Default: 0.

=item o token_file => $file_name

The name of the CSV file in which parsed tokens are to be saved.

If '', the file is not written.

Default: ''.

=item o verbose => $integer

Prints more (1, 2) or less (0) progress messages.

Default: 0.

=back

=head1 Methods

=head2 attribute_list($attribute_list)

Returns nothing.

Processes the attribute string found when Marpa pauses during the processing of a set of attributes.

Then, pushes these attributes onto a stack.

The stack's elements are documented below in L</FAQ> under L</How is the parsed graph stored in RAM?>.

=head2 description([$graph])

Here, the [] indicate an optional parameter.

Gets or sets the graph string to be parsed.

See also the L</input_file([$graph_file_name])> method.

The value supplied to the description() method takes precedence over the value read from the input file.

Also, I<description> is an option to new().

=head2 edge($edge_name)

Returns nothing.

Processes the edge name string returned by L<Marpa::R2> when it pauses during the processing of '->' or '--'.

Pushes this edge name onto a stack.

The stack's elements are documented below in the L</FAQ> under L</How is the parsed graph stored in RAM?>.

=head2 find_terminator($stringref, $target, $start)

Returns the offset into $stringref at which the $target is found.

$stringref is a refererence to the input string (stream).

$target is a regexp specifying the closing delimiter to search for.

For attributes, it is qr/}/, and for nodes, $target is qr/]/.

$start is the offset into $stringref at which to start searching. It's assumed to be pointed to the opening
delimiter when this method is called, since the value is $start is set by Marpa when it pauses based on the
C<< 'pause => before' >> construct in the grammar.

The return value allows the calling code to extract the substring between the opening and closing delimiters,
and to process it in either L</attribute_list($attribute_list)> or L</node($node_name)>.

=head2 format_token($item)

Returns a string containing a nicely formatted version of the keys and values of the hashref $item.

$item must be an element of the stack of tokens output by the parse.

The stack's elements are documented below in L</FAQ> under L</How is the parsed graph stored in RAM?>.

=head2 generate_token_file($file_name)

Returns nothing.

Writes a CSV file of tokens output by the parse if new() was called with the C<token_file> option.

=head2 get_graph_from_command_line()

If the caller has requested a graph be parsed from the command line, with the I<description> option to new(),
get it now.

Called as appropriate by run().

=head2 get_graph_from_file()

If the caller has requested a graph be parsed from a file, with the I<input_file> option to new(), get it now.

Called as appropriate by run().

=head2 grammar()

Returns an object of type L<Marpa::R2::Scanless::G>.

=head2 graph_text([$graph])

Here, the [] indicate an optional parameter.

Returns the value of the graph definition string, from either the command line or a file.

=head2 input_file([$graph_file_name])

Here, the [] indicate an optional parameter.

Gets or sets the name of the file to read the graph definition from.

See also the L</description([$graph])> method.

The whole file is slurped in as a single graph.

The first few lines of the file can start with /^\s*#/, and will be discarded as comments.

The value supplied to the description() method takes precedence over the value read from the input file.

Also, I<input_file> is an option to new().

=head2 node()

Returns nothing.

Processes the node name string returned by L<Marpa::R2> when it pauses during the processing of '[' ... ']'.

Then, pushes this node name onto a stack.

The stack's elements are documented below in the L</FAQ> under L</How is the parsed graph stored in RAM?>.

=head2 process()

Returns the result of calling Marpa's value() method.

Does the real work. Called by run() after processing the user's options.

=head2 recce()

Returns an object of type L<Marpa::R2::Scanless::R>.

=head2 renumber_items()

Ensures each item in the stack as a sequential number 1 .. N.

=head2 report()

Reports (prints) the list of items recognized by the parser.

=head2 report_tokens([0 or 1])

Here, the [] indicate an optional parameter.

Gets or sets the value which determines whether or not to report the items recognised by the parser.

Also, I<report_tokens> is an option to new().

=head2 run()

This is the only method the caller needs to call. All parameters are supplied to new().

Returns 0 for success and 1 for failure.

=head2 verbose([0 .. 2])

Here, the [] indicate an optional parameter.

Gets or sets the value which determines how many progress reports are printed.

Also, I<verbose> is an option to new().

=head1 FAQ

=head2 What is the grammar parsed by this module?

It's a cut-down version of the L<Graph::Easy::Marpa> language.
See L<Graph::Easy::Marpa::Parser/What is the Graph::Easy::Marpa language?>.

Firstly, a summary:

	Element        Syntax
	---------------------
	Edge names     Either '->' or '--'
	---------------------
	Node names     1: Delimited by '[' and ']'.
	               2: May be quoted with " or '.
	               3: Escaped characters, using '\', are allowed.
	               4: Internal spaces in node names are preserved even if not quoted.
	---------------------
	Attributes     1: Delimited by '{' and '}'.
	               2: Within that, any number of "key : value" pairs separated by ';'.
	               3: Values may be quoted with " or ' or '<...>' or '<<table>...</table>>'.
	               4: Escaped characters, using '\', are allowed.
	               5: Internal spaces in attribute values are preserved even if not quoted.
	---------------------

Note: Both edges and nodes can have attributes.

Note: HTML-like labels trigger special-case processing in Graphviz.
See L</Why doesn't the parser handle my HTML-style labels?> below.

Demo page:

	L<http://savage.net.au/Perl-modules/html/marpax.demo.stringparser/>
	L<Graph::Easy::Marpa|http://savage.net.au/Perl-modules/html/graph.easy.marpa/>

The latter page utilizes the entire  L<Graph::Easy::Marpa> language.
See L<Graph::Easy::Marpa::Parser/What is the Graph::Easy::Marpa language?>.

And now the details:

=over 4

=item o Attributes

Both nodes and edges can have any number of attributes.

Attributes are delimited by '{' and '}'.

These attributes are listed immdiately after their owing node or edge.

Each attribute consists of a key:value pair, where ':' must appear literally.

These key:value pairs must be separated by the ';' character. A trailing ';' is optional.

The values for 'key' are reserved words used by Graphviz's L<attributes|http://graphviz.org/content/attrs>.
These keys match the regexp /^[a-zA-Z_]+$/.

For the 'value', any printable character can be used.

Some escape sequences are a special meaning within L<Graphviz|http://www.graphviz.org/content/attrs>.

E.g. if you use [node name] {label: \N}, then if that graph is input to Graphviz's I<dot>, \N will be replaced
by the name of the node.

Some literals - ';', '}', '<', '>', '"', "'" - can be used in the attribute's value, but they must satisfy one
of these conditions. They must be:

=over 4

=item o Escaped using '\'.

Eg: \;, \}, etc.

=item o Placed inside " ... "

=item o Placed inside ' ... '

=item o Placed inside <...>

This does I<not> mean you can use <<Some text>>. See the next point.

=item o Placed inside <<table> ... </table>>

Using this construct allows you to use HTML entities such as &amp;, &lt;, &gt; and &quot;.

=back

Internal spaces are preserved within an attribute's value, but leading and trailing spaces are not (unless quoted).

Samples:

	[node.1] {color: red; label: Green node}
	-> {penwidth: 5; label: From Here to There}
	[node.2]
	-> {label: "A literal semicolon '\;' in a label"}

Note: That '\;' does not actually need those single-quote characters, since it is within a set of double-quotes.

Note: Attribute values quoted with a balanced pair or single- or double-quotes will have those quotes stripped.

=item o Comments

The first few lines of the input file can start with /^\s*#/, and will be discarded as comments.

=item o Daisy-chains

See L<Wikipedia|https://en.wikipedia.org/wiki/Daisy_chain> for the origin of this term.

=over 4

=item o Edges

Edges can be daisy-chained by juxtaposition, or by using a comma (','), newline, space, or attributes ('{...}')
to separate them.

Hence both of these are valid: '->,->{color:green}' and '->{color:red}->{color:green}'.

See data/edge.02.ge and data/edge.06.ge.

=item o Groups

Groups can be daisy chained by juxtaposition, or by using a newline or space to separate them.

=item o Nodes

Nodes can be daisy-chained by juxtaposition, or by using a comma (','), newline, space, or attributes ('{...}')
to separate them.

Hence all of these are valid: '[node.1][node.2]' and '[node.1],[node.2]' and '[node.1]{color:red}[node.2]'.

=back

=item o Edges

Edge names are either '->' or '--'.

No other edge names are accepted.

Note: The syntax for edges is just a visual clue for the user. The I<directed> 'v' I<undirected> nature of the
graph depends on the value of the 'directed' attribute present (explicitly or implicitly) in the input stream.
Nevertheless, usage of '->' or '--' must match the nature of the graph, or Graphviz will issue a syntax error.

Samples:

	->
	--

=item o Graphs

Graphs are sequences of nodes and edges, in any order.

The sample given just above for attributes is in fact a single graph.

A sample:

	[node]
	[node] ->
	-> {label: Start} -> {color: red} [node.1] {color: green] -> [node.2]
	[node.1] [node.2] [node.3]

=back

For more samples, see the data/*.ge files shipped with the distro.

=item o Line-breaks

These are converted into a single space.

=item o Nodes

Nodes are delimited by '[' and ']'.

Within those, any printable character can be used for a node's name.

Some literals - ']', '"', "'" - can be used in the node's value, but they must satisfy one of these
conditions. They must be:

=over 4

=item o Escaped using '\'

Eg: \].

=item o Placed inside " ... "

=item o Placed inside ' ... '

=back

Internal spaces are preserved within a node's name, but leading and trailing spaces are not (unless quoted).

Lastly, the node's name can be empty. I.e.: You use '[]' in the input stream to create an anonymous node.

Samples:

	[]
	[node.1]
	[node 1]
	[[node\]]
	["[node]"]
	[     From here     ] -> [     To there     ]

Note: Node names quoted with a balanced pair or single- or double-quotes will have those quotes stripped.

=head2 Does this module handle utf8?

Yes. See the last sample on L<the demo page|http://savage.net.au/Perl-modules/html/marpax.demo.stringparser/>.

=head2 How is the parsed graph stored in RAM?

Items are stored in an arrayref managed by L<Set::Array>. This arrayref is available via the L</items()> method.

Each element in the array is a hashref, listed here in alphabetical order by type.

Note: Items are numbered from 1 up.

=over 4

=item o Attributes

An attribute can belong to a node or an edge. An attribute definition of
'{color: red;}' would produce a hashref of:

	{
		count => $n,
		name  => 'color',
		type  => 'attribute',
		value => 'red',
	}

An attribute definition of '{color: red; shape: circle}' will produce 2 hashrefs,
i.e. 2 sequential elements in the arrayref:

	{
		count => $n,
		name  => 'color',
		type  => 'attribute',
		value => 'red',
	}

	{
		count => $n + 1,
		name  => 'shape',
		type  => 'attribute',
		value => 'circle',
	}

Attribute hashrefs appear in the arrayref immediately after the item (edge or node) to which they belong.

=item o Edges

An edge definition of '->' would produce a hashref of:

	{
		count => $n,
		name  => '->',
		type  => 'edge',
		value => '',
	}

=item o Nodes

A node definition of '[Name]' would produce a hashref of:

	{
		count => $n,
		name  => 'Name',
		type  => 'node',
		value => '',
	}

A node can have a definition of '[]', which means it has no name. Such nodes are called anonymous (or
invisible) because while they take up space in the output stream, they have no printable or visible
characters if the output stream is turned into a graph by Graphviz's
L<dot|http://www.graphviz.org/Documentation.php> program.

Each anonymous node will have at least these 2 attributes:

	{
		count => $n,
		name  => '',
		type  => 'node',
		value => '',
	}

	{
		count => $n + 1,
		name  => 'color',
		type  => 'attribute',
		value => 'invis',
	}

You can of course give your anonymous nodes any attributes, but they will be forced to have
these attributes.

E.g. If you give it a color, that would become element $n + 2 in the arrayref, and hence that color would override
the default color 'invis'. See the output for data/node.03.ge on
L<the demo page|http://savage.net.au/Perl-modules/html/marpax.demo.stringparser/>.

Node names are case-sensitive in C<dot>, but that does not matter within the context of this module.

=back

=head2 Why doesn't the parser handle my HTML-style labels?

Traps for young players:

=over 4

=item o The <br /> component must include the '/'

=item o If any tag's attributes use double-quotes, they will be doubled in the CSV output file

That is, just like double-quotes everywhere else.

=back

See L<http://www.graphviz.org/content/dot-language> for details of Graphviz's HTML-like syntax.

See data/table.*.ge for a set of examples.

=head2 Why do I get error messages like the following?

	Error: <stdin>:1: syntax error near line 1
	context: digraph >>>  Graph <<<  {

Graphviz reserves some words as keywords, meaning they can't be used as an ID, e.g. for the name of the graph.
So, don't do this:

	strict graph graph{...}
	strict graph Graph{...}
	strict graph strict{...}
	etc...

Likewise for non-strict graphs, and digraphs. You can however add double-quotes around such reserved words:

	strict graph "graph"{...}

Even better, use a more meaningful name for your graph...

The keywords are: node, edge, graph, digraph, subgraph and strict. Compass points are not keywords.

See L<keywords|http://www.graphviz.org/content/dot-language> in the discussion of the syntax of DOT
for details.

=head2 Where are the action subs named in the grammar?

In L<MarpaX::Demo::StringParser::Actions>.

=head2 What is the homepage of Marpa?

L<http://jeffreykegler.github.io/Ocean-of-Awareness-blog/>.

=head2 How do I reconcile Marpa's approach with classic lexing and parsing?

I've included in a recent article a section called
L<Constructing a Mental Picture of Lexing and Parsing|http://savage.net.au/Ron/html/Conditional.preservation.of.whitespace.html#Constructing_a_Mental_Picture_of_Lexing_and_Parsing>
which is aimed at helping us think about this issue.

=head2 How did you generate the html/*.svg files?

With a private script which uses L<Graph::Easy::Marpa::Renderer::GraphViz2> V 2.00. This script is not shipped
in order to avoid a dependency on that module. Also, another private script which validates Build.PL and
Makefile.PL would complain about the missing dependency.

See L<the demo page|http://savage.net.au/Perl-modules/html/marpax.demo.stringparser/> for details.

=head1 Machine-Readable Change Log

The file Changes was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=MarpaX::Demo::StringParser>.

=head1 Author

L<MarpaX::Demo::StringParser> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2013.

Home page: L<http://savage.net.au/>.

=head1 Copyright

Australian copyright (c) 2013, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License, a copy of which is available at:
	http://www.opensource.org/licenses/index.html

=cut
