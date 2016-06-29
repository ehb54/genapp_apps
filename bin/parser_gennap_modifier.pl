#!/usr/bin/perl

use JSON;
use Data::Dumper;
use IO::Socket;
use File::Basename;
use File::Copy;
use FindBin '$Bin';
use File::Path;

#use strict;
#use warnings;

my $file = "$Bin/quaS004_test_tags_nested_repeaters.dat";
#my $file = "$Bin/quaS004_test_tags.dat";

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

#$pathtopdb = ${ $$ref{ "pdb_location" } }[0];
#$pdbfilename = basename($pathtopdb);
#$pdbfilename =~ s{\.[^.]+$}{}; # removes extension 

$pathtosas = ${ $$ref{ "sas_data_location" } }[0];
$sasfilename = basename($pathtosas);
#$sasfilename =~ s{\.[^.]+$}{}; # removes extension 

$pathtosym = ${ $$ref{ "sym_group_location" } }[0];
$symfilename = basename($pathtosym);
#$symfilename =~ s{\.[^.]+$}{}; # removes extension 
$familyname = substr($symfilename, 0, 4);

open OUT_code, ">$p/code.usr";
print OUT_code $$ref{ "codenumber" };
close OUT_code;


$newdir =join "_", "Results", $pdbfilename;
if ( -e $newdir) {
    rmtree $newdir;
    if(-e $newdir) 
    {
        print "Directory '$newdir' still exists";
	exit;
    }
}

mkdir $newdir, oct("777");


###############################################################################

my $output_filename = join "", "$p/qua", $$ref{ "codenumber" }, ".dat";
open OUT, ">$output_filename";

my $temp_input_file = 'qua_temp.dat';
open OUT_TEMP, ">$temp_input_file";

my $tag_id_key;
my @rdlines;

## File transformation ###########################
open(my $fh, '<', $file) or die "Can't read file '$file' [$!]\n";
while (my $line = <$fh>) {
    my $line_copy = $line;

    if ($line_copy =~ m/#RDREP/)
    {
	$line_copy =~ s/#RDREP//;
	chomp $line_copy;
	push (@rdlines, $line_copy);
	next;
    }
    print OUT_TEMP $line_copy;
    
}
close OUT_TEMP;

## File transformation ###########################
#open(my $fh, '<', $file) or die "Can't read file '$file' [$!]\n";
open(my $fh, '<', $temp_input_file) or die "Can't read file '$file' [$!]\n";
while (my $line = <$fh>) {
    my $line_copy = $line;

   
    if ($line_copy =~ m/Rigid Domain/)
    {
	my $hashrd = $$ref{'num_rigid_domains'}->{pdb_location};
	my $length = scalar( @{ $hashrd } );
	for ( my $k = 0; $k < $length; $k++)
	{
	    print OUT $line_copy;
	    #print "Var: " . $$ref{'num_rigid_domains'}->{pdb_location}[0][0]; 
	    #exit;
	    my @temp_rdlines = @rdlines;
	    for (my $i = 0; $i < @temp_rdlines; $i++)
	    {
		if ($temp_rdlines[$i] =~ m/#TAG/)
		{
		    $temp_rdlines[$i] =~ s/#TAG//;
		    chomp $temp_rdlines[$i];
		    
		    my $id = (split(/[:]/, $temp_rdlines[$i]))[0];
		    my $id_key = $id;
		    $id_key =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
		    $id_key =~ tr/ /_/;             # replace whitespace with underscore
		    $id_key =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
		    $id_key =~ s/_$//;              # remove underscore at the end 
		    $id_key =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  
		    
		    print OUT $id.":". $$ref{'num_rigid_domains'}->{pdb_location}[0][$k] ."\n";  ## DOES NOT WORK!!!!!
		}
		else
		{
		    $temp_rdlines[$i] =~ s/\s+$//;
		    print OUT $temp_rdlines[$i] ."\n";
		}
	    }
	}
    }
    
    # PROCESS SIMPLE FIELDS && COMPLEXER FIELDS OF SEVERAL VALUES  
    elsif ($line_copy =~ m/#TAG/ && $line_copy!~ m/%%%%/)
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
		    
		    if ($j != @array1 -1)
		    {
			my $len = (16 - length($$ref{$tag}))/2.0; 
			print OUT " " x $len;
			print OUT $$ref{$tag};
			print OUT " " x $len;			
		    }
		    else
		    {
			my $len = (5 - length($$ref{$tag}))/2.0; 
			print OUT " " x $len;
			print OUT " " . $$ref{$tag};
		    }
		    
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
    elsif ($line_copy =~ m/#TAG/ && $line_copy =~ m/%%%%/)     # LABELS in GenApp
    {
	$line_copy =~ s/#TAG//;
	$line_copy =~ s/\s+$//; # remove whitespace from the end
	#chomp $line_copy;
	print OUT "$line_copy\n";
    }
    
    # PROCESS Listboxes #############################################################
    elsif ($line_copy =~ m/#T_CBR/)   
    {
	$line_copy =~ s/#T_CBR//;
	chomp $line_copy;
	#$line_copy =~ s/\s*$/$1/; # remove whitespace from both ends
	$line_copy =~ s/\s+$//;
	
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
			    if ($j != @array1 -1)
			    {
				my $len = (16 - length($$ref{$tag}))/2.0; 
				print OUT " " x $len;
				print OUT $$ref{$tag};
				print OUT " " x $len;			
			    }
			    else
			    {
				my $len = (5 - length($$ref{$tag}))/2.0; 
				print OUT " " x $len;
				print OUT " " . $$ref{$tag};
			    }
			    
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
			    if ($j != @array1 -1)
			    {
				my $len = (16 - length($$ref{$tag}))/2.0; 
				print OUT " " x $len;
				print OUT $$ref{$tag};
				print OUT " " x $len;			
			    }
			    else
			    {
				my $len = (5 - length($$ref{$tag}))/2.0; 
				print OUT " " x $len;
				print OUT " " . $$ref{$tag};
			    }
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
    # END PROCESS LISTBOXES ##################
    elsif ($line_copy =~ m/#EXPSAS/)
    {
	$line_copy =~ s/#EXPSAS//;
	chomp $line_copy;
	my $id_exp = (split(/[:]/, $line))[0];
	#my $dval = (split(/[:]/, $line))[-1];
	
	print OUT "$id_exp:$sasfilename\n";
    }
    elsif ($line_copy =~ m/#NUMRD/)
    {
	$line_copy =~ s/#NUMRD//;
	chomp $line_copy;
	my $id_exp = (split(/[:]/, $line))[0];
	#my $dval = (split(/[:]/, $line))[-1];
	
	#print OUT "$id_exp:$pdbfilename\n";
	my $size = $$ref{'num_rigid_domains'}->{pdb_location};
	my $length = scalar( @{ $size } );
	print OUT $id_exp.":". $length."\n";
    }
    
    elsif($line_copy =~ m/#SYM/)
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
	#$line_copy =~ s/\s*$/$1/; # remove whitespace from both ends
	$line_copy =~ s/\s+$//;
	print OUT "$line_copy\n";
    }
}

####################################################

close OUT;

####################################################

print encode_json( $res ). "\n" ;



