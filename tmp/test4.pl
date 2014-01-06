#!/usr/bin/perl

use Test::Deep::NoTest;

my $a = { a => b,
          c => d };

my $b = { a => b,
          c => d };

    
while ( my ( $k, $v ) = each %$a )
{
    print "a: $k $v\n";
}

while ( my ( $k, $v ) = each %$b )
{
    print "b: $k $v\n";
}

print eq_deeply( $a, $b ) ? "deeply match\n" : "don't deeply match\n";

