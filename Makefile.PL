use inc::Module::Install 0.91;

use 5.008001;

use ExtUtils::Depends;

name 'Continuation-Delimited';
all_from 'lib/Continuation/Delimited.pm';

requires 'B::Hooks::XSUB::CallAsOp' => '0.01';
build_requires 'B::Hooks::XSUB::CallAsOp' => '0.01';
configure_requires 'B::Hooks::XSUB::CallAsOp' => '0.01';

# minimum version that works on Win32+gcc
configure_requires 'ExtUtils::Depends' => 0.302;

test_requires 'Test::More' => '0.88';

repository 'git://github.com/nothingmuch/continuation-delimited.git';

my $pkg = ExtUtils::Depends->new('Continuation::Delimited', 'B::Hooks::XSUB::CallAsOp');

WriteMakefile(
  $pkg->get_makefile_vars,
);

