#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 11;
use Scalar::Util qw(refaddr);

use ok 'Continuation::Delimited' => qw(delimit suspend);

# based on http://coach.cs.uchicago.edu/package-source/murphy/amb.plt/1/0/planet-docs/amb/index.html

my $fail = \"oh noes";
sub amb_is_failure ($) { ref $_[0] and refaddr($_[0]) == refaddr($fail) }

sub amb_call {
	my @items = @_;

	# this could be optimized when the operation *is* deterministic:
	# return $items[0] if @items == 1;

	suspend {
		my $k = shift;
		# $k is the continuation for the remainder of the amb_find, delimited
		# by amb_find or the last amb_call.

		# we try to execute $k with every param in @items, causing amb_call to
		# return these values
		foreach my $item ( @items ) {
			my $result = delimit {
				# this expression is delimited for backtracking to return
				# $fail in $result
				$k->($item);
			};

			unless ( amb_is_failure($result) ) {
				return $result; # return from the shift, not from amb_call
			}
		}

		# none of the items seemed to work (or none were provided), we need to
		# backtrack further
		return $fail;
	}
}

# ambiguous evaluation context
sub amb_find (&) {
	my $block = shift;

	my $result = delimit { $block->() };

	if ( amb_is_failure($result) ) {
		die "No solution";
	} else {
		return $result;
	}
}


# convenience functions

# backtracks unless its arguments are true
sub amb_assert ($) {
	my $condition = shift;

	amb() unless $condition; # amb() with no arguments causes backtracking

	return $condition;
}

# evaluates thunks
sub amb {
	my $item = amb_call(@_);

	if ( ref $item eq 'CODE' ) {
		return $item->();
	} else {
		return $item;
	}
}


my $result = amb_find {
	my $x = amb(1, 2, 3);
	my $y = amb(1, 2, 3);

	amb_assert($x > 2);
	amb_assert($x + $y == 5);
	amb_assert($y <= 2);

	pass("returning a result");

	return [ $x, $y ];
};

ok( $result, "got a result" );

is( ref($result), "ARRAY", "it's an array ref" );

my ( $x, $y ) = @$result;

ok(defined($x), "got a value for x");
ok(defined($y), "got a value for y");

cmp_ok( $x, ">=", 2, "x assertion" );
cmp_ok( $y, "<=", 2, "y assertion" );
is( $x + $y, 5, "joint assertion" );

is( $x, 3, "x" );
is( $y, 2, "y" );

