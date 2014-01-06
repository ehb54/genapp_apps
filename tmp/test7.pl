#!/usr/bin/perl

use Switch;

@tries = ( "one", "two", "three", "four" );

foreach my $i ( @tries )
{
    print "try $i\n";
    switch( $i ) {
        case "one" { print "1" }
        case "two" { print "2" }
        case "three" { print "3" }
        else       { print "something else" }
    }
    print "\n";
}
