#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 58;

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
#$^D="";

	pass("invoked cont");
	$invoked++;

	is($after, 1, "was deferred");

	ok($value, "got a value ($value)");

#$^D="DXstvRlA";
	return $value + $count++;
};

pass("escaped");

ok( $add, "got cont" );
is( ref($add), "CODE", "looks like a coderef" );

is( $invoked, 0, "not yet invoked" );

$after++;

#$^D="DXstvRlA";
my $x = $add->(4);
#$^D="";

is( $x, 11, "cont works" );
is( $invoked, 1, "invoked once" );

is( $add->(42), 50, "cont works repeatedly" );
is( $invoked, 2, "invoked twice" );

is( $add->(7), 16, "and again" );
is( $invoked, 3, "invoked three times" );

is( $created, 1, "shift not reinvoked" );
is( $after, 1, "reset retop not reinvoked" );
is( $count, 10, "count var updated" );

sub foo {
	my $value = cont_shift { return $_[0] };
	return 3 + $value;
}

my $add_foo = cont_reset { foo() };

is( ref $add_foo, "CODE", "captured cont" );

is( $add_foo->(7), 10, "Add with a sub" );
is( $add_foo->(5), 8, "Add with a sub" );

sub quxx {
	return 1 + cont_shift { return $_[0] };
}

my $add_quxx = cont_reset { quxx() };

is( ref $add_quxx, "CODE", "captured cont" );

my $y = $add_quxx->(1);

is( $y, 2, "Add with a sub (no temp var)" );
is( $add_quxx->(3), 4, "Add with a sub (no temp var)" );

sub bar { $_[0] + cont_shift { return $_[0] } }

my $add_bar = cont_reset { bar(4) };

is( ref $add_bar, "CODE", "captured cont" );

is( $add_bar->(7), 11, "Add with a sub using \@_" );
is( $add_bar->(5), 9, "Add with a sub using \@_" );

sub baz { my $add = shift; $add + cont_shift { return $_[0] } }

my $add_baz = cont_reset { baz(7) };

is( ref $add_baz, "CODE", "captured cont" );

is( $add_baz->(7), 14, "Add with a sub using lexical" );
is( $add_baz->(10), 17, "Add with a sub using lexical" );

sub my_shift { cont_shift { return $_[0] } }
sub zot { my $x = $_[0]; $x += my_shift(); return $x } # tests that $x is cloned on the stack
sub make_zot { cont_reset { zot(13) } };

my $add_zot = make_zot();

is( ref $add_zot, "CODE", "captured cont" );

#$^D="DXstvRlA";
my $z = $add_zot->(7);
#$^D="";

is( $z, 20, "Add with a nested sub" );
is( $add_zot->(5), 18, "Add with a nested sub" );
is( $add_zot->(20), 33, "Add with a nested sub" );
is( $add_zot->(42), 55, "Add with a nested sub" );
is( $add_zot->(1), 14, "Add with a nested sub" );

{
	my ( @log, $end );

	sub steps {
		cont_reset {
			push @log, my_shift(); # sub { cont_shift { return $_[0] } }->(); # the second form works, the first fails (pad screwups?)
			push @log, my_shift();
			push @log, my_shift();
			$end++;
			return "end";
		}
	}

	ok( !$end, "didn't end yet" );
	is( ref(my $k = steps()), "CODE", "got cont" );
	is_deeply( \@log, [], "log" );

	ok( !$end, "didn't end yet" );
	is( ref($k = $k->("one")), "CODE", "next cont" );
	is_deeply( \@log, [qw(one)], "log" );

	ok( !$end, "didn't end yet" );
	is( ref($k = $k->("two")), "CODE", "next cont" );
	is_deeply( \@log, [qw(one two)], "log" );

	ok( !$end, "didn't end yet" );
	is( $k->("three"), "end", "end" );
	is_deeply( \@log, [qw(one two three)], "log" );
	ok( $end, "end set" );

}

# FIXME
# reset { reset { shift { shift { } } } } + composition of reversed stack frames
# recursive continuations
# corecursive continuations
