#!/usr/bin/perl
use JSON;

$json = '
{
 "header" : "Sassie 2",
 "menu" : [
           { 
            "id"      : "Build",
            "icon"    : "sassie_build.png",
            "modules" : [ 
                         { 
                          "id"    : "sassie_build_1",
                          "label" : "Build 1",
                          "views" : [ "Input", "Output", "Plots", "OpenGL" ]
                         },
                         { 
                          "id"    : "sassie_build_2",
                          "label" : "Build 2",
                          "views" : [ "Input", "Output", "Plots", "OpenGL" ]
                         }
                        ]
            },
           { 
            "id"      : "Tools",
            "icon"    : "sassie_tools.png",
            "modules" : [ 
                         { 
                          "id"    : "sassie_center",
                          "label" : "Center",
                          "views" : [ "Input", "Output", "Plots", "OpenGL" ]
                         },
                         { 
                          "id"    : "sassie_align",
                          "label" : "Align",
                          "views" : [ "Input", "Output", "Plots", "OpenGL" ]
                         },
                         { 
                          "id"    : "sassie_data_interpolation",
                          "label" : "Data Interpolation",
                          "views" : [ "Input", "Output", "Plots", "OpenGL" ]
                         }
                        ]
            }
          ]
}
';

$json =~ s/\n//g;

$ref = decode_json($json);

sub expand {
    my $arg = $_[ 0 ];
    my $tag = $_[ 1 ];
    my $pos = $_[ 2 ];
    $tag = "base" if !length( $tag );

    if ( ref( $arg ) eq 'HASH' )
    {
        while ( my ($k, $v ) = each %$arg )
        {
            if ( ref( $v ) )
            {
                print '-'x$pos . "$k is a ref:\n";
                expand( $v, $k, $pos + 1 );
            } else {
                print '-'x$pos . "<$tag> <$k> is $v\n";
            }
        }
    }
    if ( ref( $arg ) eq 'ARRAY' )
    {
        for ( my $i = 0; $i < @$arg; ++$i )
        {
            my $v = $$arg[ $i ];
            if ( ref( $v ) )
            {
                print '-'x$pos . "[$i] is a ref:\n";
                expand( $v, $tag, $pos + 1 );
            } else {
                print '-'x$pos . "<$tag> [$i] is $v\n";
            }
        }
    }
}    

sub ref_match {
    my $arg   = $_[ 0 ];
    my $match = $_[ 1 ];
    my $tag   = $_[ 2 ];
    my $pos   = $_[ 3 ];

    my @ret;
    
    $tag = "base" if !length( $tag );

    if ( ref( $arg ) eq 'HASH' )
    {
        while ( my ($k, $v ) = each %$arg )
        {
            if ( $k eq $match )
            {
                push @ret, $v;
            }
            if ( ref( $v ) )
            {
                print '-'x$pos . "$k is a ref:\n" if $debug;
                my $x = ref_match( $v, $match, $k, $pos + 1 );
                foreach my $k ( @$x )
                {
                    push @ret, $k;
                }
            } else {
                print '-'x$pos . "<[$tag]> <$k> is $v\n" if $debug;
            }
        }
    }
    if ( ref( $arg ) eq 'ARRAY' )
    {
        for ( my $i = 0; $i < @$arg; ++$i )
        {
            my $v = $$arg[ $i ];
            if ( ref( $v ) )
            {
                print '-'x$pos . "[$i] is a ref:\n" if $debug;
                my $x = ref_match( $v, $match, $tag, $pos + 1 );
                foreach my $k ( @$x )
                {
                    push @ret, $k;
                }
            } else {
                print '-'x$pos . "<[$tag]> [$i] is $v\n" if $debug;
            }
        }
    }

    \@ret;
}    

sub ref_match2 {
    my $arg   = $_[ 0 ];
    my $match = $_[ 1 ];
    my $tag   = $_[ 2 ];
    my $pos   = $_[ 3 ];

    my %ret;
    
    $tag = "base" if !length( $tag );

    if ( ref( $arg ) eq 'HASH' )
    {
        while ( my ($k, $v ) = each %$arg )
        {
            if ( $k eq $match )
            {
                $ret{ $v }++;
            }
            if ( ref( $v ) )
            {
                print '-'x$pos . "$k is a ref:\n" if $debug;
                my $x = ref_match2( $v, $match, $k, $pos + 1 );
                while( my ( $k, $v ) = each %$x )
                {
                    $ret{ $k }++;
                }
            } else {
                print '-'x$pos . "<[$tag]> <$k> is $v\n" if $debug;
            }
        }
    }
    if ( ref( $arg ) eq 'ARRAY' )
    {
        for ( my $i = 0; $i < @$arg; ++$i )
        {
            my $v = $$arg[ $i ];
            if ( ref( $v ) )
            {
                print '-'x$pos . "[$i] is a ref:\n" if $debug;
                my $x = ref_match2( $v, $match, $tag, $pos + 1 );
                while( my ( $k, $v ) = each %$x )
                {
                    $ret{ $k }++;
                }
            } else {
                print '-'x$pos . "<[$tag]> [$i] is $v\n" if $debug;
            }
        }
    }

    \%ret;
}    
        

$m = "modules";

$x = ref_match( $ref, $m );
print "-----$m-----\n";

foreach my $k ( @$x )
{
    print "$k\n" if !ref( $k );
    if ( ref( $k ) eq 'ARRAY' )
    {
        foreach my $j ( @$k )
        {
            print "-> $j\n" if !ref( $j );
            if ( ref( $j ) eq 'HASH' )
            {
                while( my ( $k, $v ) = each %$j )
                {
                    print "--> $k $v\n" if !ref( $v );
                }
            }
        }
    }
}

print "-"x80 . "\n";

$x = ref_match2( $ref, $m );
print "-----$m-----\n";

while( my ( $k, $v ) = each %$x )
{
    print "$k $v\n";
    print "ref:" . ref( $k ) . "\n";
    print "ref:" . ref( \$k ) . "\n";
    if ( ref( $k ) eq 'ARRAY' )
    {
        print "array\n";
        foreach my $j ( @$k )
        {
            print "-> $j\n"; # if !ref( $j );
            if ( ref( $j ) eq 'HASH' )
            {
                while( my ( $k, $v ) = each %$j )
                {
                    print "--> $k $v\n"; # if !ref( $v );
                }
            }
        }
    }
}
