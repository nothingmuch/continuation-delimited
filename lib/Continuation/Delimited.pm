package Continuation::Delimited;

use 5.010000;
use strict;
use warnings;

use B::Hooks::XSUB::CallAsOp;
use Magical::Hooker::Decorate;
use XS::Object::Magic;

use Sub::Exporter -setup => {
	exports => [qw(
		delimit
		suspend
	)],
};

our $VERSION = '0.01';

require XSLoader;
XSLoader::load('Continuation::Delimited', $VERSION);

__PACKAGE__

__END__
