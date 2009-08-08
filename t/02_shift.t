#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';

use Devel::Peek qw(DumpArray Dump);

use ok 'Continuation::Delimited' => qw(cont_reset cont_shift);

# FIXME breaks if these are lexicals
my $after = 0;
my $invoked = 0;
my $created = 0;

my $add = cont_reset {
	pass("inside delimited scope");

	my $value = cont_shift {
		my $k = shift;

		pass("created cont");
		is(ref($k), 'CODE', "looks like a coderef");

		is( $created++, 0, "first shift" );

		return $k; # FIXME test fallthrough as well
	};

	pass("invoked cont");
	$invoked++;

	is($after, 1, "was deferred");

	ok($value, "got a value ($value)");

	return $value + 7;
};

pass("escaped");

ok( $add, "got cont" );
is( ref($add), "CODE", "looks like a coderef" );

is( $invoked, 0, "not yet invoked" );

$after++;

is( $add->(4), 11, "cont works" );
is( $invoked, 1, "invoked once" );

is( $add->(42), 49, "cont works repeatedly" );
is( $invoked, 2, "invoked twice" );
is( $add->(7), 14, "and again" );
is( $invoked, 3, "invoked three times" );

is( $created, 1, "shift not reinvoked" );
is( $after, 1, "reset retop not reinvoked" );
