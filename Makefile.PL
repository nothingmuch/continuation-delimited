use inc::Module::Install 0.91;

use 5.008001;

use ExtUtils::Depends;

name 'Continuation-Delimited';
all_from 'lib/Continuation/Delimited.pm';


my %c_deps = (
	'B::Hooks::XSUB::CallAsOp' => 0.01,
	'Magical::Hooker::Decorate' => 0.03,
	'XS::Object::Magic' => 0.02,
);

foreach my $dep ( keys %c_deps ) {
	my $version = $c_deps{$dep};

	requires           $dep => $version;
	build_requires     $dep => $version;
	configure_requires $dep => $version;
}

# minimum version that works on Win32+gcc
configure_requires 'ExtUtils::Depends' => 0.302;

test_requires 'Test::More' => '0.88';
test_requires 'Test::use::ok' => 0;
test_requires 'Scope::Guard' => 0;

repository 'git://github.com/nothingmuch/continuation-delimited.git';

ppport;

my $pkg = ExtUtils::Depends->new('Continuation::Delimited', keys %c_deps);

WriteMakefile(
  $pkg->get_makefile_vars,
);

