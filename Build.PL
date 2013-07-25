use Module::Build;

my $class = Module::Build->subclass(code => <<'EOF');

sub ACTION_authortest
{
	my($self) = @_;

	$self->depends_on('build');
	$self->depends_on('manifest');
	$self->depends_on('distmeta');

	$self->test_files( qw< t xt/author > );
	$self->recursive_test_files(1);

	$self->depends_on('test');

	return;
}
EOF

$class -> new
(
	module_name    => 'MarpaX::Demo::StringParser',
	license        => 'artistic_2',
	dist_abstract  => 'Conditional preservation of whitespace while parsing',
	dist_author    => 'Ron Savage <ron@savage.net.au>',
	build_requires =>
	{
		Test::More=> 0.94,
#		Test::Pod  => 1.45, # Make it optional. See t/pod.t
	},
	configure_requires =>
	{
		Module::Build => 0.3800,
	},
	requires =>
	{
		Algorithm::Diff  => 1.1902,
		Capture::Tiny    => 0.11,
		charnames        => 0,
		Config           => undef,
		Config::Tiny     => 2.14,
		Date::Simple     => 3.03,
		File::Basename   => 2.78,
		File::Copy       => 2.21,
		File::HomeDir    => 0.99,
		File::Spec       => 3.33,
		File::Temp       => 0.2301,
		Getopt::Long     => 2.38,
		HTML::Entities::Interpolate => 1.04,
		Marpa::R2        => 2.064000,
		Moo              => 1.002000,
		open             => 0,
		Path::Tiny       => 0.025,
		Perl6::Slurp     => 0.03,
		Pod::Usage       => 1.36,
		Set::Array       => 0.23,
		strict           => 0,
		Text::CSV        => 1.32,
		Text::CSV::Slurp => 1.01,
		Text::Xslate     => 1.4001,
		Try::Tiny        => 0.09,
		utf8             => 0,
		warnings         => 0,
	},
) -> create_build_script();
