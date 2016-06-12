#!/usr/bin/perl

use JSON;
use Data::Dumper;
use IO::Socket;
use File::Basename;
use File::Copy;
use FindBin '$Bin';

if ( !@ARGV )
{
    print "\"error\":\"program called with no arguments\"\n";
    exit;
}


###############################################
$json = shift;
$ref = decode_json( $json );

$res = {};
$p = $$ref{ "_base_directory" };

$pathtopdb = ${ $$ref{ "pdb_location" } }[0];
$pdbfilename = basename($pathtopdb);
####################################################

# setup up the socket handle once:
my $sock = IO::Socket::INET->new( Proto => 'udp', PeerPort => $$ref{ "_udpport" }, PeerAddr => $$ref{ "_udphost" } );
$msg = {};
$$msg{ "_uuid" } = $$ref{ "_uuid" };


##############################################

open my $command, "$Bin/c36a6_svn132/exec/gnu/charmm -i $Bin/script_toppar/write_pdb_crd_psf.inp |";
open my $output, "> output";
open OUT_log, ">$p/logfile.txt";

$$msg{ "_textarea" } = "Starting Calculations...\n\n";
$sock->send( encode_json( $msg ) );
while (<$command>)
{
    my $outputline = "$output $_";
    if ($outputline =~ m/GLOB/)
    {
	$outputline =~ s/^\S+\s*//;
    }
    print OUT_log $outputline;
    $$msg{ "_textarea" } = $outputline;
    $sock->send( encode_json( $msg ) );
}

close OUT_log; 

$$msg{ "_textarea" } = "\nCalculations Finished! \n";
$sock->send( encode_json( $msg ) );


#############################################
$$res{ "logfile" } = "$p/logfile.txt";


print encode_json( $res ). "\n" ;
