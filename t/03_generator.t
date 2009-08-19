#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';

use ok 'Continuation::Delimited' => qw(cont_reset cont_shift);

sub generator (&) {
	my $body = shift;
	my $k;

	return sub {
		local *yield = sub {
			# a new value, $ret has been yielded
			my $ret = shift;

			warn "yielding $ret";
			Continuation::Delimited::stk();

#$^D="DXstvRl";
			cont_shift {
				# capture the state in $k, and return $ret from cont_reset { }
				# on the next invocation of the generator $k will be invoked to
				# return from yield(), resuming the genrator body
				$k = shift;
$^D="";
				warn "captured $k, returning $ret";
				return $ret;
			};
		};

		if ( $k ) {
			warn "$k exists, resuming";
			# the generator is being invoked again while it is running
			Continuation::Delimited::stk();
			&$k;
		} else {
			warn "restarting";
			# the generator is being invoked with no captured state
			return cont_reset {
				Continuation::Delimited::stk();
				printf("invoking body $body");
				my $ret = &$body; # FIXME test that @_ is passed through properly
				# the generator has returned normall (using return instead of
				# yield), so it's finished. clear the captured state (if any) and return
				# the value from cont_reset normally
				warn "normal return, $ret";
				undef $k;
				return $ret;
			};
		}
	}
}

my $gen = generator {
	warn "in generator";
	yield(1);
	yield(2);
	yield(3);
	#yield($_) for 1 .. 3; # FIXME check that yield returns the right thing

	return "finished";
};

# FIXME also test for interleaved generators

ok( $gen, "got a generator" );
is( ref($gen), "CODE", "looks like a coderef" );

is( $gen->(), 1, "first yield" );
is( $gen->(), 2, "second yield" );
is( $gen->(), 3, "third yield" );
is( $gen->(), "finished", "normal return" );

is( $gen->(), 1, "first yield" );
is( $gen->(), 2, "second yield" );
is( $gen->(), 3, "third yield" );
is( $gen->(), "finished", "normal return" );


