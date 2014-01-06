#!/usr/bin/perl

sub rethash {

    my $vers = $_[ 0 ];
    my $ret = {};

    if ( $vers == 1 )
    {
        $$ret{ "e" } = "b";
        $$ret{ "el" } = "ectra";
    } else {
        $$ret{ "hi" } = "there";
        $$ret{ "why" } = "not";

        my $add = rethash( 1 );

        while ( my ($k, $v ) = each %$add )
        {
            $$ret{ $k } = $v;
        }
    }        

    $ret;
}


$x = rethash();

while ( my ($k, $v ) = each %$x )
{
    print "$k $v\n";
}

