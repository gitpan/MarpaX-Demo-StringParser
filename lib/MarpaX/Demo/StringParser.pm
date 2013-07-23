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

use Regexp::Common qw/balanced delimited/;

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

has parser =>
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

our $VERSION = '1.00';

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

	# Ensure we can stack from the action_object.

	$MarpaX::Demo::StringParser::Actions::items = $self -> items;

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

graph_grammar			::= graph_definition		action => graph

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

:lexeme					~ end_node			pause => before		event => end_node
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

:lexeme					~ end_attributes	pause => before		event => end_attributes
end_attributes			~ '}'

# Boilerplate.

:discard				~ whitespace
whitespace				~ [\s]+

END_OF_SOURCE
		})
	);

	$self -> parser
	(
		Marpa::R2::Scanless::R -> new
		({
			grammar => $self -> grammar
		})
	);

} # End of BUILD.

# -----------------------------------------------
# $target is either qr/]/ or qr/}/, and allows us to handle
# both node names and either edge or node attributes.
# The special case is <<...>>, as used in attributes.

sub find_terminator
{
	my($self, $string, $target, $start) = @_;
	my(@char)   = split(//, substr($$string, $start) );
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
	my($value);

	for
	(
		my $pos = $self -> parser -> read(\$string);
		$pos < $length;
		$pos = $self -> parser -> resume($pos)
	)
	{
		print "read() => pos: $pos\n" if ($self -> verbose > 1);

		$do_lexeme_read = 1;
		@event          = @{$self -> parser -> events};
		$event_name     = ${$event[0]}[0];
		($start, $span) = $self -> parser -> pause_span;
		$lexeme_name    = $self -> parser -> pause_lexeme;
		$lexeme         = $self -> parser -> literal($start, $span);

		print "pause_span($lexeme_name) => start: $start. span: $span. lexeme: $lexeme. event: $event_name\n" if ($self -> verbose > 1);

		if ($event_name eq 'start_attributes')
		{
			# Read the attribute_start lexeme, but don't do lexeme_read()
			# at the bottom of the for loop, because we're just about
			# to fiddle $pos to skip the attributes themselves.
			# And that means we'll end up at the end_attributes lexeme.

			$pos            = $self -> parser -> lexeme_read($lexeme_name);
			$do_lexeme_read = 0;
			$lexeme_name    = 'end_attributes';
			$pos            = $self -> find_terminator(\$string, qr/}/, $start);
			$attribute_list = substr($string, $start + 1, $pos - $start - 1);

			print "index() => attribute list: $attribute_list\n" if ($self -> verbose > 1);

			$self -> attribute_list($attribute_list);
		}
		elsif ($event_name eq 'end_attributes')
		{
		}
		elsif ($event_name eq 'start_node')
		{
			# Read the node_start lexeme, but don't do lexeme_read()
			# at the bottom of the for loop, because we're just about
			# to fiddle $pos to skip the node's name itselvf.
			# And that means we'll end up at the node_end lexeme.

			$pos            = $self -> parser -> lexeme_read($lexeme_name);
			$do_lexeme_read = 0;
			$lexeme_name    = 'node_end';
			$pos            = $self -> find_terminator(\$string, qr/]/, $start);
			$node_name      = substr($string, $start + 1, $pos - $start - 1);

			print "index() => node name: $node_name\n" if ($self -> verbose > 1);

			$self -> node($node_name);
		}
		elsif ($event_name eq 'end_node')
		{
		}
		elsif ($event_name eq 'directed_edge')
		{
			MarpaX::Demo::StringParser::Actions::edge({}, $lexeme);
		}
		elsif ($event_name eq 'undirected_edge')
		{
			MarpaX::Demo::StringParser::Actions::edge({}, $lexeme);
		}
		else
		{
			die "Unexpected lexeme '$event_name' without a pause\n";
		}

		$pos = $self -> parser -> lexeme_read($lexeme_name) if ($do_lexeme_read);

		print "lexeme_read($lexeme_name) => $pos\n" if ($self -> verbose > 1);
    }

	# Return a defined value for success and undef for failure.

	return $self -> parser -> value;

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

	perl -Ilib scripts/parse.pl -d '[noddy]{color:blue}' -r 1 -v 1 -t output.tokens

Complex graphs work too: Try -d '[node.1]{a:b;c:d}->{e:f;}->{g:h}[node.2]{i:j}->[node.3]{k:l}'

The following refer to data shipped with the distro:

	perl -Ilib scripts/parse.pl -i data/node.04.ge -r 1 -t node.04.tokens
	diff data/node.04.tokens node.04.tokens

You can use scripts/parse.sh to simplify this process:

	scripts/parse.sh data/node.04.ge node.04.tokens -r 1

See L<the demo page|http://savage.net.au/Perl-modules/html/marpax.demo.stringparser/> for sample output.

Also, there is L<an article|http://savage.net.au/Ron/html/Conditional.preservation.of.whitespace.html> based on this module.

=head1 Description

This module a demonstration of how to use L<Marpa::R2>'s capabilities to have the parser within Marpa call back to code
in your own module, to handle certain cases where you don't want Marpa's default processing to occur.

A classic case of this is when you wish to preserve whitespace in some contexts, but also want Marpa to discard
whitespace in all other contexts.

Specifically, L<MarpaX::Demo::StringParser> is a cut-down version of L<Graph::Easy::Marpa> V 2.00, and (the former)
provides a Marpa-based parser for parts of L<Graph::Easy>-style graph definitions. The latter module handles the whole
language.

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

C<new()> is called as C<< my($parser) = MarpaX::Demo::StringParser -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<MarpaX::Demo::StringParser>.

Key-value pairs accepted in the parameter list (see corresponding methods for details
[e.g. description($graph)]):

=over 4

=item o description => '[node.1]->[node.2]'

Specify a string for the graph definition.

You are strongly encouraged to surround this string with '...' to protect it from your shell if using
this module directly from the command line.

See also the 'input_file' key which reads the graph from a file.

The 'description' key takes precedence over the 'input_file' key.

=item o input_file => $graph_file_name

Read the graph definition from this file.

See also the 'graph' key to read the graph from the command line.

The whole file is slurped in as 1 graph.

The first lines of the file can start with /^\s*#/, and will be discarded as comments.

The 'description' key takes precedence over the 'input_file' key.

=item o report_tokens => $Boolean

Calls L</report()> to report, via the log, the items recognized by the parser.

=item o token_file => $file_name

The name of the CSV file in which parsed tokens are to be saved.

If '', the file is not written.

Default: ''.

=item o verbose => $integer

Prints more (1, 2) or less (0) progress messages.

=back

=head1 Methods

=head2 attribute_list($attribute_string)

Returns nothing.

Processes the attribute string returned by L<Marpa::R2> when it pauses during the processing of a set of
attributes.

Then, pushes this set of attributes onto a stack.

The stack's elements are documented below in L</FAQ> under L</How is the parsed graph stored in RAM?>.

=head2 description([$graph])

Here, the [] indicate an optional parameter.

Gets or sets the graph string to be parsed.

The value supplied to the description() method takes precedence over the value read from the input file.

Also, C<description> is an option to new().

=head2 format($item)

Returns a string containing a nicely formatted version of the keys and values of the hashref $item.

$item must be an element of the stack of tokens output by the parse.

The stack's elements are documented below in L</FAQ> under L</How is the parsed graph stored in RAM?>.

=head2 generate_token_file($file_name)

Returns nothing.

Writes a CSV file of tokens output by the parse if new() was called with the C<token_file> option.

=head2 get_graph_from_command_line()

If the caller has requested a graph be parsed from the command line, with the description option to new(),
get it now.

Called as appropriate by run().

=head2 get_graph_from_file()

If the caller has requested a graph be parsed from a file, with the input_file option to new(), get it now.

Called as appropriate by run().

=head2 grammar()

Returns an object of type L<Marpa::R2::Scanless::G>.

=head2 graph([$graph])

Here, the [] indicate an optional parameter.

Gets or sets the value of the graph definition string.

=head2 graph_text([$graph])

Here, the [] indicate an optional parameter.

Returns the value of the graph definition string, from either the command line or a file.

=head2 input_file([$graph_file_name])

Here, the [] indicate an optional parameter.

Gets or sets the name of the file to read the graph definition from.

See also the description() method.

The whole file is slurped in as 1 graph.

The first lines of the file can start with /^\s*#/, and will be discarded as comments.

The value supplied to the description() method takes precedence over the value read from the input file.

Also, C<input_file> is an option to new().

=head2 node()

Returns nothing.

Processes the node name string returned by L<Marpa::R2> when it pauses during the processing of '[' ... ']'.

Then, pushes this node name onto a stack.

The stack's elements are documented below in the L</FAQ> under L</How is the parsed graph stored in RAM?>.

=head2 parser()

Returns an object of type L<Marpa::R2::Scanless::R>.

=head2 process()

Does the real work. Called by run() after processing the user's options.

=head2 renumber_items()

Ensures each item in the stack as a sequential number 1 .. N.

=head2 report()

Reports (prints) the list of items recognized by the parser.

=head2 report_tokens([0 or 1])

The [] indicate an optional parameter.

Gets or sets the value which determines whether or not to report the items recognised by the parser.

Also, C<report_tokens> is an option to new().

=head2 run()

This is the only method the caller needs to call. All parameters are supplied to new().

Returns 0 for success and 1 for failure.

=head2 verbose([0 .. 2])

The [] indicate an optional parameter.

Gets or sets the value which determines how many progress reports are printed.

Also, C<verbose> is an option to new().

=head1 FAQ

=head2 Does this module handle utf8?

Yes. See the last sample on L<the demo page|http://savage.net.au/Perl-modules/html/marpax.demo.stringparser/>.

=head2 In simple terms, what is the grammar you parse?

It's a cut-down version of the L<DOT|http://www.graphviz.org/content/dot-language> language used by AT&T's C<dot> program. See L<http://graphviz.org>.

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

See L<the demo page|http://savage.net.au/Perl-modules/html/marpax.demo.stringparser/> for many samples.

And now the details:

=over 4

=item o Comments

The first lines of the input file can start with /^\s*#/, and will be discarded as comments.

=item o Line-breaks

These are converted into a single space.

=item o Nodes

Nodes are delimited by the quote characters '[' and ']'.

Within the quotes, any printable character can be used for a node's name.

Some literals - ']', '"', "'" - can be used in the node's value, but they must satisfy one of these
conditions:

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

=item o Edges

Edge names are either '->' or '--'.

No other edge names are accepted.

Samples:

	->
	--

=item o Attributes

Both nodes and edges can have any number of attributes.

Attributes are delimited by the quote characters '{' and '}'.

These attributes are listed immdiately after their owing node or edge.

Each attribute consists of a key:value pair, where ':' must appear literally.

These key:value pairs must be separated by the ';' character.

The values for 'key' are reserved words used by Graphviz's L<attributes|http://graphviz.org/content/attrs>.
These keys satisy the regexp /^[a-zA-Z_]+$/.

For the 'value', any printable character can be used.

Some escape sequences are reserved by L<Graphviz|http://www.graphviz.org/content/attrs>.

Some literals - ';', '}', '<', '>', '"', "'" - can be used in the attribute's value, but they must satisfy one of these
conditions:

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

=head2 How is the parsed graph stored in RAM?

Items are stored in an arrayref managed by L<Set::Array>.

This arrayref is available via the L</items()> method.

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

An attribute definition of '{color: red; shape: circle;}' will produce 2 hashrefs,
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

Node names are case-sensitive in C<dot>, but that does not matter within the context of this module.

=back

=head2 Where are the actions named in the grammar?

In L<MarpaX::Demo::StringParser::Actions>.

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

Home page: L<http://savage.net.au/index.html>.

=head1 Copyright

Australian copyright (c) 2013, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License, a copy of which is available at:
	http://www.opensource.org/licenses/index.html

=cut
