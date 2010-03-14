#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 31;

use ok 'Continuation::Delimited' => qw(delimit suspend);

sub generator (&) {
	my $body = shift;
	my $k;

	return sub {
		local *yield = sub {
			# a new value, $ret has been yielded
			my $ret = shift;

#$^D="DXstvRl";
			suspend {
				# capture the state in $k, and return $ret from delimit { }
				# on the next invocation of the generator $k will be invoked to
				# return from yield(), resuming the genrator body
				$k = shift;
$^D="";
				return $ret;
			};
		};

		if ( $k ) {
			# the generator is being invoked again while it is running
			return $k->(); # FIXME test &$k, causes segfaulting
		} else {
			# the generator is being invoked with no captured state
			return delimit {
				my $ret = &$body; # FIXME test that @_ is passed through properly
				# the generator has returned normall (using return instead of
				# yield), so it's finished. clear the captured state (if any) and return
				# the value from delimit normally
				undef $k;
				return $ret;
			};
		}
	}
}

my $manual = do {
	my $k;
	sub {
		if ( $k ) {
			return $k->(); # FIXME test &$k
		} else {
			return delimit {
				suspend { $k = shift; return 1 };
				suspend { $k = shift; return 2 };
				suspend { $k = shift; return 3 };

				undef $k;

				return "finished";
			}
		}
	}
};

my $auto = generator {
	yield(1);
	yield(2);
	yield(3);

	return "finished";
};

sub range {
	my ( $from, $to ) = @_;

	return generator {
		for my $i ( $from .. $to ) {
			yield($i);
		}

		return "finished";
	}
};

my $loop = range(1,3);

# FIXME also test for interleaved generators

foreach my $gen ( $manual, $auto, $loop ) {
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
}
