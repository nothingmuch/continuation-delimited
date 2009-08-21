use 5.010000;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME              => 'Continuation::Delimited',
    VERSION_FROM      => 'lib/Continuation/Delimited.pm',
    PREREQ_PM         => {
	'Test::use::ok' => '0.0.2'
    },
    SIGN              => 1,
);
