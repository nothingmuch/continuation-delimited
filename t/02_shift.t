#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';

use Devel::Peek qw(DumpArray Dump);

use ok 'Continuation::Delimited' => qw(cont_reset cont_shift);

my $after = 0;
my $invoked = 0;
my $created = 0;


Continuation::Delimited::stk();
#$^D="DXstvRL";

my $add = cont_reset {
	#pass("inside delimited scope");

	my $value = cont_shift {
		$^D="";
		my $k = shift;

		pass("created cont");
		is(ref($k), 'CODE', "looks like a coderef");

		is( $created++, 0, "first shift" );

#$^D="DXstvRL";
		return $k; # FIXME test fallthrough as well
	};
	$^D="";

	pass("invoked cont");
	$invoked++;

	is($after, 1, "was deferred");

	ok($value, "got a value ($value)");

#$^D="DXstvRl";
	return $value + 7;
};

$^D="";

pass("escaped");

ok( $add, "got cont" );
is( ref($add), "CODE", "looks like a coderef" );

is( $invoked, 0, "not yet invoked" );

$after++;

#$^D="DXstvRl";
Continuation::Delimited::stk();
my $x = $add->(4);
Continuation::Delimited::stk();
$^D="";
is( $x, 11, "cont works" );
is( $invoked, 1, "invoked once" );

is( $add->(42), 49, "cont works repeatedly" );
is( $invoked, 2, "invoked twice" );

is( $add->(7), 14, "and again" );
is( $invoked, 3, "invoked three times" );

is( $created, 1, "shift not reinvoked" );
is( $after, 1, "reset retop not reinvoked" );
