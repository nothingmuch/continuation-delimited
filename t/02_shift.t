#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 27;

use ok 'Continuation::Delimited' => qw(cont_reset cont_shift);

my $after = 0;
my $invoked = 0;
my $created = 0;
my $count = 7;

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

	return $value + $count++;
};

pass("escaped");

ok( $add, "got cont" );
is( ref($add), "CODE", "looks like a coderef" );

is( $invoked, 0, "not yet invoked" );

$after++;

my $x = $add->(4);
is( $x, 11, "cont works" );
is( $invoked, 1, "invoked once" );

is( $add->(42), 50, "cont works repeatedly" );
is( $invoked, 2, "invoked twice" );

is( $add->(7), 16, "and again" );
is( $invoked, 3, "invoked three times" );

is( $created, 1, "shift not reinvoked" );
is( $after, 1, "reset retop not reinvoked" );
is( $count, 10, "count var updated" );

sub foo { 3 + cont_shift { return $_[0] } }

my $add_foo = cont_reset { foo() };

is( $add_foo->(7), 10, "Add with a sub" );
is( $add_foo->(4), 8, "Add with a sub" );

sub bar { $_[0] + cont_shift { return $_[0] } }

my $add_bar = cont_reset { bar(4) };

is( $add_bar->(7), 11, "Add with a sub using \@_" );
is( $add_bar->(4), 9, "Add with a sub using \@_" );

sub baz { my $add = shift; $add + cont_shift { return $_[0] } }

my $add_baz = cont_reset { baz(2) };

is( $add_baz->(7), 9, "Add with a sub using lexical" );
is( $add_baz->(4), 6, "Add with a sub using lexical" );

