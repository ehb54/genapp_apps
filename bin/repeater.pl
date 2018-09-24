#!/usr/bin/perl

# user defines

use JSON;

if ( !@ARGV ) {
    print "\{\"error\":\"$0 called with no arguments\"\}\n";
    exit;
}

$req = decode_json( shift );

$res = {};

# assemble output

$$res{ "e" } = sprintf( "%.6g", $$req{ "m" } * $$req{ "c" }**2 );

my $json = JSON->new;
$$res{ "_textarea" } = "JSON output from executable:\n" . $json->pretty->encode( $res ) . "\n";
$$res{ "_textarea" } .= "JSON input to executable:\n" . $json->pretty->encode( $req ) . "\n";

print encode_json( $res ) . "\n";

