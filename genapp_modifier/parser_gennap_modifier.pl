#!/usr/bin/perl

use JSON;
use Data::Dumper;


my $file = '/home/alexey/QuaFit/Commented/quatest_commented.dat';

if ( !@ARGV )
{
    print "\"error\":\"program called with no arguments\"\n";
    exit;
}

#my $file = @ARGV[0];



#########################################
$json = shift;
$ref = decode_json( $json );

$res = {};
#$$res{ "hi" } = "data_interpolation executable";

$p = $$ref{ "_base_directory" };

#$number_of_cycles_per_subrun = $$ref{ "number_of_cycles_per_subrun" };

$$res{ "outputfile" } = "$p/quafit.txt";
open OUT, ">$p/quafit.txt";

## File transformation ###########################
open(my $fh, '<', $file) or die "Can't read file '$file' [$!]\n";
while (my $line = <$fh>) {
    my $line_copy = $line;
    chomp $line;
    #print OUT $line;
    
    if ($line =~ /:/ && $line !~ m/:$/)
    {
	my $id = (split(/[:]/, $line))[0];
	my $dval = (split(/[:]/, $line))[-1];
	
	$dval =~ s/\s+$//;
	$dval =~ s/^\s*(.*?)\s*$/$1/;  # remove whitespace from both ends
	#push @{$data[$id]}, $dval;
	#print($id );
	
	my $id_key = $id;
	$id_key =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
	$id_key =~ tr/ /_/;             # replace whitespace with underscore
	$id_key =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
	$id_key =~ s/_$//;              # remove underscore at the end 
	$id_key =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  


	
	if ( length $id_key )
	{
	    my $label_key = $id; # label
	    $label_key =~ tr/a-zA-Z0-9_(\/)[] //cd; 
	    $label_key =~ s/^\s*(.*?)\s*$/$1/;
	    if ( $label_key =~ m/(1\/0)/ )
	    {
		$id_key =~ s/_10//ig;
		if ($dval == 0 && !(exists $ref->{$id_key}))   # Solved problem with checkboxes!!
		{
		    print OUT $line_copy; 
		}
				    
	    }
	    
	    for my $key (keys  $ref )
	    {
		if ($id_key eq $key)
		{
		    if ($$ref{$key} eq "on")
		    {
			$$ref{$key} = "1";
		    }
		    print OUT "$id:$$ref{$key}\n";
		}
	    }
	}
	else
	{
	    print OUT $line_copy;
	}
    }
    else 
    {
	print OUT $line_copy;
    }
}
####################################################33

close OUT;

print encode_json( $res );



