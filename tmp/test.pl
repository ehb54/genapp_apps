#!/usr/bin/perl

$href = { APR => 4, AUG => 8 };

$href2{ "APR" } = 4;
$href2{ "AUG" } = 8;

sub print_ref {
    my $ref = $_[ 0 ];
    my $tag = $_[ 1 ];
    my $cnt = $_[ 2 ];
    $cnt = 40 if $cnt <= 0;
    print '-'x$cnt . "\n";
    print "print ref tag <$tag> ref <$ref> cnt <$cnt>\n";
    if ( ref( $ref ) eq 'HASH' )
    {
        while ( my ($k, $v ) = each %$ref )
        {
            print "$tag: $k $v\n";
        }
    }
    if ( ref( $ref ) eq 'ARRAY' )
    {
        for ( my $i = 0; $i < @$ref; ++$i )
        {
            print "$tag: $i $$ref[ $i ]\n";
        }
    }
    if ( ref( $ref ) eq 'REF' )
    {
        print_ref( $$ref, "ref: $tag", $cnt - 2 );
    }
    print '-'x$cnt . "\n";
}

++$pos; print "$pos:\n"; 
print_ref( \$href, "href try as \\\$" );
++$pos; print "$pos:\n"; 
print_ref( $href, "href try as \$" );

++$pos; print "$pos:\n"; 
print_ref( \%href2, "href2 try as \\\%" );
++$pos; print "$pos:\n"; 
print_ref( %href2, "href2 try as \%" );

