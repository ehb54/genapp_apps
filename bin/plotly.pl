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

$$res{ "plotbar" } = decode_json(
    '{
        "data": [
            {
                "x": [
                    "giraffes",
                    "orangutans",
                    "monkeys"
                    ],
                 "y": [
                        20,
                        14,
                        23
                    ],
                 "type": "bar"
            }
         ]
    }' );

$$res{ "plotline" } = decode_json(
    '{
       "data" : [
            {
                "x": [1, 2, 3, 4],
                "y": [10, 15, 13, 17],
                "mode": "markers",
                "marker": {
                    "color": "rgb(219, 64, 82)",
                    "size": 12
                }
            },
            {
                "x" : [2, 3, 4, 5],
                "y" : [16, 5, 11, 9],
                "mode" : "lines",
                "line" : {
                    "color" : "rgb(55, 128, 191)",
                    "width": 3
                }
            },
            {
                "x" : [1, 2, 3, 4],
                "y" : [12, 9, 15, 12],
                "mode" : "lines+markers",
                "marker" : {
                    "color" : "rgb(128, 0, 128)",
                    "size": 8
                },
                "line" : {
                        "color" : "rgb(128, 0, 128)",
                        "width" : 1
                }
            }
        ],
        "layout" : {
            "title" : "Line and Scatter Styling"
        }
    }' );


my $json = JSON->new;
$$res{ "_textarea" } = "JSON output from executable:\n" . $json->pretty->encode( $res ) . "\n";
$$res{ "_textarea" } .= "JSON input to executable:\n" . $json->pretty->encode( $req ) . "\n";

print encode_json( $res ) . "\n";

