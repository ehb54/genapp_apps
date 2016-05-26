#!/usr/bin/perl

use JSON;
use Data::Dumper;
use File::Basename;
use FindBin '$Bin';

#my $file = "$Bin/quatest_tags.dat";
my $file = "$Bin/quaS004_test_tags.dat";

#print "The script is located in $Bin_path.\n";
#exit();

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
$p = $$ref{ "_base_directory" };

## Dir for project's files (usualy PDB base name, first 4 letters) ############

$pathtopdb = ${ $$ref{ "pdb_location" } }[0];
$pdbfilename = basename($pathtopdb);
$pdbfilename =~ s{\.[^.]+$}{}; # removes extension 

$pathtosas = ${ $$ref{ "sas_data_location" } }[0];
$sasfilename = basename($pathtosas);
#$sasfilename =~ s{\.[^.]+$}{}; # removes extension 

$pathtosym = ${ $$ref{ "sym_group_location" } }[0];
$symfilename = basename($pathtosym);
#$symfilename =~ s{\.[^.]+$}{}; # removes extension 
$familyname = substr($symfilename, 0, 4);

#print $familyname;
#exit;

$newdir =join "_", "Results", $pdbfilename;
mkdir $newdir, 0755;

###############################################################################

$$res{ "outputfile" } = "$p/quafit.txt";
open OUT, ">$p/quafit.txt";

my $tag_id_key;

## File transformation ###########################
open(my $fh, '<', $file) or die "Can't read file '$file' [$!]\n";
while (my $line = <$fh>) {
    my $line_copy = $line;
    
    # PROCESS SIMPLE FIELDS && COMPLEXER FIELDS OF SEVERAL VALUES  
    if ($line_copy =~ m/#TAG/ && $line_copy!~ m/%%%%/)
    {
	$line =~ s/#TAG//;
	chomp $line;
	
	my $id = (split(/[:]/, $line))[0];
	my $dval = (split(/[:]/, $line))[-1];
	
	my $id_key = $id;
	$id_key =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
	$id_key =~ tr/ /_/;             # replace whitespace with underscore
	$id_key =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
	$id_key =~ s/_$//;              # remove underscore at the end 
	$id_key =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  

	$dval =~ s/\s+$//;
	$dval =~ s/^\s*(.*?)\s*$/$1/;  # remove whitespace from both ends

	my @array1 = split /\s+/,$dval;
	
	if (@array1 == 1)
	{
	    my $label_key = $id; # label
	    $label_key =~ tr/a-zA-Z0-9_(\/)[] //cd; 
	    $label_key =~ s/^\s*(.*?)\s*$/$1/;
	    if ( $label_key =~ m/(1\/0)/ )
	    {
		$id_key =~ s/_10//ig;
		if ($dval == 0 && !(exists $ref->{$id_key}))   # Checkboxes!!
		{
		    $line_copy =~ s/#TAG//;
		    chomp $line_copy;
		    $line_copy =~ s/\s+$//;
		    print OUT $line_copy ."\n";
		}
		if ($dval == 1 && !(exists $ref->{$id_key}))   # Checkboxes!!
		{
		    print OUT "$id:0\n"; 
		}
		if ($$ref{$id_key} eq "on")
		{
		    $$ref{$id_key} = "1";
		    print OUT "$id:$$ref{$id_key}\n";
		}
	    }
	    else
	    {
		print OUT "$id:$$ref{$id_key}\n";
	    }
	}
	else
	{
	    my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
	    
	    my $tag_temp = "$array_ids[0]$id_key";
	    
	    if (exists $ref->{$tag_temp})
	    {
		print OUT "$id:";
		for ( my $j = 0; $j < @array1; $j++)
		{
		    my $tag = "$array_ids[$j]$id_key";
		    print OUT "  $$ref{$tag}  ";
		}
		print OUT "\n";
	    }
	    else
	    {
		$line_copy =~ s/#TAG//;
		chomp $line_copy;
		print OUT $line_copy . "\n"; 
	    }
	}
    }
    else  #ddd
    {
	if ($line_copy =~ m/#TAG/ && $line_copy =~ m/%%%%/)     # LABELS in GenApp
	{
	    $line_copy =~ s/#TAG//;
	    $line_copy =~ s/\s+$//; # remove whitespace from the end
	    #chomp $line_copy;
	    print OUT "$line_copy\n";
	}
	else #dd
	{ 
	    # PROCESS Listboxes #############################################################
	    if ($line_copy =~ m/#T_CBR/)   
	    {
		$line_copy =~ s/#T_CBR//;
		chomp $line_copy;
		my $id_cbr;
			
		my @matches = $line_copy =~ /:/g;   #Check for lines with more than 1 :'s
		my $count = @matches;
		if ($count > 1)
		{
		    $id_cbr = (split(/[:]/, $line_copy))[0];
		    $dval_cbr_1 = (split(/[:]/, $line_copy))[1];
		    $dval_cbr_2 = (split(/[:]/, $line_copy))[-1];
		    
		    my $id_key = $id_cbr;
		    $id_key =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
		    $id_key =~ tr/ /_/;             # replace whitespace with underscore
		    $id_key =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
		    $id_key =~ s/_$//;              # remove underscore at the end 
		    $id_key =~ tr/A-Z/a-z/;         # replace uppercase to lowercase

		    $tag_id_key = $id_key;
		    
		    if (!(exists $ref->{$id_key}))   # Checkboxes!!
		    {
			print OUT $line_copy . "\n";
		    }
		    else 
		    {
			my $id = $dval_cbr_1;
			$id =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
			$id =~ tr/ /_/;             # replace whitespace with underscore
			$id =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
			$id =~ s/_$//;              # remove underscore at the end 
			$id =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  
			
			my $val = $dval_cbr_2;
			$val =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
			my @array1 = split /\s+/,$val;
			
			if (@array1 == 1)
			{
			    print OUT "$id_cbr:$dval_cbr_1:$$ref{$id}\n";
			}
			else
			{
			    my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
			    
			    my $tag_temp = "$array_ids[0]$id$id_key";
			    
			    if (exists $ref->{$tag_temp})
			    {
				print OUT "$id_cbr:$dval_cbr_1:";
				for ( my $j = 0; $j < @array1; $j++)
				{
				    my $tag = "$array_ids[$j]$id$id_key";
				    print OUT "  $$ref{$tag}  ";
				}
				print OUT "\n";
			    }
			    else
			    {
				print OUT $line_copy . "\n"; 
			    }
			}
		    }
		}
		else 
		{
		    if ( $line_copy =~ m/#END/ )
		    {
			$line_copy =~ s/#END//;
			$line_copy =~ s/\s+$//; # remove whitespace from the end
			#chomp $line_copy;
		    }
		    
		    my $dval_cbr_11 = (split(/[:]/, $line_copy))[0];
		    my $dval_cbr_22 = (split(/[:]/, $line_copy))[-1];

		    
		    if (!(exists $ref->{$tag_id_key}))   # Checkboxes!!
		    {
			chomp $line_copy;
			print OUT $line_copy . "\n";
		    }
		    else 
		    {
			my $id = $dval_cbr_11;
			$id =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
			$id =~ tr/ /_/;             # replace whitespace with underscore
			$id =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
			$id =~ s/_$//;              # remove underscore at the end 
			$id =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  
			
			my $val = $dval_cbr_22;
			$val =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
			my @array1 = split /\s+/,$val;
			
			if (@array1 == 1)
			{
			    print OUT "$dval_cbr_11:$$ref{$id}\n";
			}
			else
			{
			    my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );

			    my $tag_temp = "$array_ids[0]$id$tag_id_key";
			    
			    if (exists $ref->{$tag_temp})
			    {
				print OUT "$dval_cbr_11:";
				for ( my $j = 0; $j < @array1; $j++)
				{
				    my $tag = "$array_ids[$j]$id$tag_id_key";
				    print OUT "  $$ref{$tag}  ";
				}
				print OUT "\n";
			    }
			    else
			    {
				chomp $line_copy;
				print OUT $line_copy . "\n"; # To CORRECT!!
			    }
			}
		    }
		}
	    }
	    else    
	    {
		if ($line_copy =~ m/#EXPSAS/)
		{
		    $line_copy =~ s/#EXPSAS//;
		    chomp $line_copy;
		    my $id_exp = (split(/[:]/, $line))[0];
		    #my $dval = (split(/[:]/, $line))[-1];
		    
		    print OUT "$id_exp:$sasfilename\n";
		}
		else
		{
		    if ($line_copy =~ m/#UNITSTR/)
		    {
			$line_copy =~ s/#UNITSTR//;
			chomp $line_copy;
			my $id_exp = (split(/[:]/, $line))[0];
			#my $dval = (split(/[:]/, $line))[-1];
		    
			print OUT "$id_exp:$pdbfilename\n";
		    }
		    else
		    {
			if($line_copy =~ m/#SYM/)
			{
			    $line_copy =~ s/#SYM//;
			    chomp $line_copy;
			    my $id_exp = (split(/[:]/, $line))[0];
			    #my $dval = (split(/[:]/, $line))[-1];
			    
			    print OUT "$id_exp:$familyname\n";
			}
			else # THE REST
			{
			    #chomp $line_copy;
			    $line_copy =~ s/\s*$/$1/; # remove whitespace from both ends
			    print OUT "$line_copy\n";
			}
		    }
		}
	    }
	}
    }
}
####################################################33

close OUT;
close OUT1;


print encode_json( $res );



