#!/usr/bin/perl

# user defines

use JSON;

if ( !@ARGV ) {
    print "\{\"error\":\"$0 called with no arguments\"\}\n";
    exit;
}

sub check_json {
    my $in = $_[ 0 ];
    my $out = '';

    @ol = split /\n/, $in;
    my @l = grep !/^\s*#/ , @ol;
    my $l = join '', @l;
    my $json;
    eval {
        $json = decode_json( $l );
    } || do {
        my $e = $@;
        
        # figure out line #

        my ( $cp ) = $e =~ /at character offset (\d+) /;
        my $i;
        my $cpos = $cp;
        for ( $i = 0; $i < @ol; ++$i ) {
            next if $ol[ $i ] =~ /^\s*#/;
            $cpos -= length( $ol[ $i ] );
            last if $cpos < 0;
        }

        my $sline = $i - 2;
        my $eline = $i + 2;
        $sline = 0 if $sline < 0;
        $eline = @ol - 1 if $eline >= @ol;

        $out .= "JSON Error in JSON near these lines:\n";
        for ( my $j = $sline; $j <= $eline; ++$j ) {
            my $uj = $j + 1;
            $out .= "$uj: $ol[$j]";
            $out .= "$uj: " .'^'x(length($ol[$j])) . "\n" if $j == $i;
        }
    };
    return $out;
}


sub trim($)
{
    my $string = shift;
    $string =~ s/^\s+//g;
    $string =~ s/\s+$//g;
    return $string;
}

$req = decode_json( shift );

$res = {};

# assemble output

$combined  = $$req{ 'input_method' } eq "combined";

if ( $combined ) {
    if ( !length( $$req{ 'combined_input' } ) ) {
        $$res{ '_textarea' } = "Empty input, nothing to plot!";
    } else {
        $json_err  = check_json( $$req{ 'combined_input' } );
        if ( length( $json_err ) ) {
            $$res{ '_textarea' } .= "$json_err";
        } else {
            $$res{ 'plotout' } = decode_json( $$req{ 'combined_input' } );
        }
    }
} else {
    $ok              = 1;
    $layout_provided = 0;
    if ( !length( $$req{ 'data_input' } ) ) {
        $$res{ '_textarea' } = "Empty data input, nothing to plot!";
        $ok = 0;
    } else {
        $json_data_err  = check_json( $$req{ 'data_input' } );

        if ( length( $json_data_err ) ) {
            $$res{ '_textarea' } .= "data $json_data_err";
            $ok = 0;
        }
        if ( length( $$req{ 'layout_input' } ) ) {
            $json_layout_err  = check_json( $$req{ 'layout_input' } );

            if ( length( $json_layout_err ) ) {
                $$res{ '_textarea' } .= "layout $json_layout_err";
                $ok = 0;
            } else {
                $layout_provided = 1;
            }
        }

        if ( $ok ) {
            $$res{ 'plotout' } = {};
            $$res{ 'plotout' }{ 'data' } = decode_json( $$req{ 'data_input' } );
            if ( $layout_provided && $$req{ 'layout_input' } ) {
                $$res{ 'plotout' }{ 'layout' } = decode_json( $$req{ 'layout_input' } );
            }
        }
    }
}
        

# my $json = JSON->new;
# $$res{ "_textarea" } = "JSON output from executable:\n" . $json->pretty->encode( $res ) . "\n";
# $$res{ "_textarea" } .= "JSON input to executable:\n" . $json->pretty->encode( $req ) . "\n";

print encode_json( $res ) . "\n";

