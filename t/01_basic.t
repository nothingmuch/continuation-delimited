#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 16;

use ok 'Continuation::Delimited' => qw(delimit suspend);

my $x = delimit {
	pass("reset called");
	return 42;
};

is( $x, 42, "value preserved" );

sub bar { return "foo" }

sub foo {
	is_deeply( [ @_ ], [ "blah" ], "args" );
	delimit {
		is_deeply( [ @_ ], [], "no args" );
		bar();
	};
}

is( delimit { delimit { 22 } }, 22, "two levels" );
is( delimit { delimit { delimit { delimit { 44 } } } }, 44, "four levels" );
is( delimit { foo("blah") }, "foo", "sub calls" );

is( delimit { !wantarray }, 1, "scalar context" );
is_deeply( [ delimit { wantarray } ], [ 1 ], "list context" );
delimit { is( wantarray, undef, "void context") };


is(
	delimit {
		suspend { 42 };
		fail("not reached");
		return "foo";
	},
	42,
	"escape continuation",
);


my $after = 0;

my $value = delimit {
	pass("reset called");
    my $x = suspend {
		pass("shift called");
		my $k = shift;
		$k->(7);
    };

	is($after, 0, "resumed immediately");

	return 3 + $x;
};

$after++;

is($value, 10, "noop continuation");

