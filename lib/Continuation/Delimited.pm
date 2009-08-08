package Continuation::Delimited;

use 5.010000;
use strict;
use warnings;

use Sub::Exporter -setup => {
	exports => [qw(
		cont_reset
		cont_shift
	)],
};

our $VERSION = '0.01';

require XSLoader;
XSLoader::load('Continuation::Delimited', $VERSION);

__PACKAGE__

__END__
