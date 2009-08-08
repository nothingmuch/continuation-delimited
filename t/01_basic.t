#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 11;

use ok 'Continuation::Delimited' => qw(cont_reset);

my $x = cont_reset {
	pass("reset called");
	return 42;
};

is( $x, 42, "value preserved" );

sub bar { return "foo" }

sub foo {
	is_deeply( [ @_ ], [ "blah" ], "args" );
	cont_reset {
		is_deeply( [ @_ ], [], "no args" );
		bar();
	};
}

is( cont_reset { cont_reset { 22 } }, 22, "two levels" );
is( cont_reset { cont_reset { cont_reset { cont_reset { 44 } } } }, 44, "four levels" );
is( cont_reset { foo("blah") }, "foo", "sub calls" );

is( cont_reset { !wantarray }, 1, "scalar context" );
is_deeply( [ cont_reset { wantarray } ], [ 1 ], "list context" );
cont_reset { is( wantarray, undef, "void context") };

