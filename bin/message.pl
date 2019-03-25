#!/usr/bin/perl

# user defines

use JSON;
use IO::Socket;   

if ( !@ARGV ) {
    print "\{\"error\":\"$0 called with no arguments\"\}\n";
    exit;
}

$req = decode_json( shift );

sub sendmsg {};

my $domsg = $$req{ "_uuid" } && $$req{ "_udpport" } && $$req{ "_udphost" };

my $sock = IO::Socket::INET->new( Proto => 'udp', PeerPort => $$req{ "_udpport" }, PeerAddr => $$req{ "_udphost" } ) if $domsg;

sub message_box {
    return if !$domsg;
    my $text = $_[0];
    my $icon = $_[1];
    print "sendmsg: $prog $text\n" if $debug;
    
    if ( length( $text ) || length( $prog ) ) {
        my $msg  = {};
        $$msg{ "_uuid" } = $$req{ "_uuid" };
        $$msg{ "_message" } = decode_json( 
            "{
                 \"icon\" : \"$icon\"
                 ,\"text\" : \"$text\"
             }" );

        $sock->send( encode_json( $msg ) );
    }
}

my $res = {};

# assemble output

message_box( "Here is a test mesasge", "information.png" );
sleep 3;
$$res{ 'result' } = "message sent";
    
# uncomment to see json input & output in output textarea
# my $json = JSON->new;
# $$res{ "_textarea" } = "JSON output from executable:\n" . $json->pretty->encode( $res ) . "\n";
# $$res{ "_textarea" } .= "JSON input to executable:\n" . $json->pretty->encode( $req ) . "\n";

print encode_json( $res ) . "\n";

