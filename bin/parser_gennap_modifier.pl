#!/usr/bin/perl

use JSON;
use Data::Dumper;
use IO::Socket;
use File::Basename;
use File::Copy;
use FindBin '$Bin';
use File::Path;
use POSIX;

###########################################################

sub Process_pdb_reps {
	   
    my $temp_rdlines = $_[0];
    my $k = $_[1];
    my $repeatername = $_[2];
    my $pdb_location = $_[3];
    
    $temp_rdlines =~ s/#PDB//;
    chomp $temp_rdlines;
    
    my $id = (split(/[:]/, $temp_rdlines))[0];
    my $id_key = $id;
    $id_key =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
    $id_key =~ tr/ /_/;             # replace whitespace with underscore
    $id_key =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
    $id_key =~ s/_$//;              # remove underscore at the end
    $id_key =~ tr/A-Z/a-z/;         # replace uppercase to lowercase 
    
    my $pdbfilename = basename($$ref{$repeatername}->{$pdb_location}[$k][0]);
    $pdbfilename =~ s{\.[^.]+$}{};  # removes extension;
    print OUT $id.":". $pdbfilename ."\n";
}	

sub Process_aasyntax_reps {

    my $temp_rdlines = $_[0];
    my $k = $_[1];
    my $repeatername = $_[2];
       
    $temp_rdlines =~ s/#SYNTAXAA//;
    chomp $temp_rdlines;
    
    my $id_key = 'aa_number';
    my $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(5 - $len);			

    $id_key = 'rdfl_number';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(3 - $len);

    $id_key = 'sym_number';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(3 - $len);

    $id_key = 'screw_axis_number';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(4 - $len);
        
    $id_key = 'atom_code';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(6 - $len);

    $id_key = 'coor_vect';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(18 - $len);

    $id_key = 'condition';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(20 - $len);
    
    my $id_key_upper_level = 'val_name_vect';
    if (exists $$ref{$repeatername}->{$id_key_upper_level}[$k]{value}{val_name_vect_value})
    {
	$len = length($$ref{$repeatername}->{$id_key_upper_level}[$k]{value}{val_name_vect_value});
	print OUT $$ref{$repeatername}->{$id_key_upper_level}[$k]{value}{val_name_vect_value};
	print OUT " " x ceil(18 - $len);
    }
    elsif (exists $$ref{$repeatername}->{$id_key_upper_level}[$k]{name}{val_name_vect_name})
    {
	$len = length($$ref{$repeatername}->{$id_key_upper_level}[$k]{name}{val_name_vect_name});
	print OUT $$ref{$repeatername}->{$id_key_upper_level}[$k]{name}{val_name_vect_name};
	print OUT " " x ceil(18 - $len);
    }
    else
    {
	$len = length($$ref{$repeatername}->{$id_key_upper_level}[$k]);
	print OUT $$ref{$repeatername}->{$id_key_upper_level}[$k];
	print OUT " " x ceil(18 - $len);
    }
    
    $id_key = 'fact_value';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(10 - $len);

    $id_key = 'method';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(7 - $len);

    $id_key = 'k';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(9 - $len);

    $id_key = 'name';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(7 - $len);

    $id_key = 'flag';
    $len = length($$ref{$repeatername}->{$id_key}[$k]); 
    print OUT $$ref{$repeatername}->{$id_key}[$k];
    print OUT " " x ceil(4 - $len);


    print OUT "\n";
}	

sub Process_tag_reps {
    
    my $line_copy = $_[0];
    my $k = $_[1];
    my $repeatername = $_[2];

    $line_copy =~ s/#TAG//;
    chomp $line_copy;
    
    my $id = (split(/[:]/, $line_copy))[0];
    my $dval = (split(/[:]/, $line_copy))[-1];
    
    my $id_key = $id;
    $id_key =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
    $id_key =~ tr/ /_/;             # replace whitespace with underscore
    $id_key =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
    $id_key =~ s/_$//;              # remove underscore at the end 
    $id_key =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  
    
    $dval =~ s/\s+$//;
    $dval =~ s/^\s*(.*?)\s*$/$1/;   # remove whitespace from both ends
    
    my @array1 = split /\s+/,$dval;
    
    if (@array1 == 1)
    {
	my $label_key = $id; # label
	$label_key =~ tr/a-zA-Z0-9_(\/)[] //cd; 
	$label_key =~ s/^\s*(.*?)\s*$/$1/;
	if ( $label_key =~ m/(1\/0)/ )
	{
	    $id_key =~ s/_10//ig;
	    if ($dval == 0 && !(exists $$ref{$repeatername}->{$id_key}[$k]))   # Checkboxes!!
	    {
		$line_copy =~ s/#TAG//;
		chomp $line_copy;
		$line_copy =~ s/\s+$//;
		print OUT $line_copy ."\n";
	    }
	    if ($dval == 1 && !(exists $$ref{$repeatername}->{$id_key}[$k]))   # Checkboxes!!
	    {
		print OUT "$id:0\n"; 
	    }
	    if ($$ref{$repeatername}->{$id_key}[$k] eq "on")
	    {
		$$ref{$repeatername}->{$id_key}[$k] = "1";
		print OUT "$id:$$ref{$repeatername}->{$id_key}[$k]\n";
	    }
	}
	else
	{
	    #print "VAR: " . $id .":". $$ref{$repeatername}->{$id_key};
	    #exit;
	    print OUT "$id:$$ref{$repeatername}->{$id_key}[$k]\n";
	}
    }
    else
    {
	my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
	
	#if (!exists ($$ref{$repeatername}->{$id_key}[$k]))
	if ($$ref{$repeatername}->{$id_key}[$k] == null)
	{
	    $line_copy =~ s/#TAG//;
	    chomp $line_copy;
	    print OUT $line_copy . "\n"; 
	}
	else
	{
	    print OUT "$id:";
	    for ( my $j = 0; $j < @array1; $j++)
	    {
		my $tag = "$array_ids[$j]$id_key";
				
		if ($j != @array1 -1)
		{
		    my $len = (16 - length($$ref{$repeatername}->{$id_key}[$k]{$tag}))/2.0; 
		    print OUT " " x $len;
		    print OUT $$ref{$repeatername}->{$id_key}[$k]{$tag};
		    print OUT " " x $len;			
		}
		else
		{
		    my $len = (5 - length($$ref{$repeatername}->{$id_key}[$k]{$tag}))/2.0; 
		    print OUT " " x $len;
		    print OUT " " . $$ref{$repeatername}->{$id_key}[$k]{$tag};
		}
	    }
	    print OUT "\n";
	}
    }   
}


sub Process_t_cbr_reps { 

    my $line_copy = $_[0];
    my $k = $_[1];
    my $repeatername = $_[2];

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
	
	#print $id_key ."  " . $$ref{$repeatername}->{$id_key}{1}{alpha};
	#exit;

	#if (!(exists $$ref{$repeatername}->{$id_key}[$k]))   # Checkboxes!!
	if ($$ref{$repeatername}->{$id_key}[$k] == null)   # Checkboxes!!
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
		print OUT "$id_cbr:$dval_cbr_1:$$ref{$repeatername}->{$id_key}[$k]{$id}\n";
	    }
	    else
	    {
		my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
		
		print OUT "$id_cbr:$dval_cbr_1:";
		for ( my $j = 0; $j < @array1; $j++)
		{
		    my $tag = "$array_ids[$j]$id$id_key";
		    
		    if ($j != @array1 -1)
		    {
			my $len = (16 - length($$ref{$repeatername}->{$id_key}[$k]{$tag}))/2.0; 
			print OUT " " x $len;
			print OUT $$ref{$repeatername}->{$id_key}[$k]{$tag};
			print OUT " " x $len;
		    }
		    else
		    {
			my $len = (5 - length($$ref{$repeatername}->{$id_key}[$k]{$tag}))/2.0; 
			print OUT " " x $len;
			print OUT " " . $$ref{$repeatername}->{$id_key}[$k]{$tag};
		    }
		}
		print OUT "\n";
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
	
	#if (!(exists $$ref{$repeatername}->{$tag_id_key}[$k]))   # Checkboxes!!
	if ($$ref{$repeatername}->{$tag_id_key}[$k] == null) 
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
		print OUT "$dval_cbr_11:$$ref{$repeatername}->{$tag_id_key}[$k]{$id}\n";
	    }
	    else
	    {
		my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
		
		print OUT "$dval_cbr_11:";
		for ( my $j = 0; $j < @array1; $j++)
		{
		    my $tag = "$array_ids[$j]$id$tag_id_key";
		    if ($j != @array1 -1)
		    {
			my $len = (16 - length($$ref{$repeatername}->{$tag_id_key}[$k]{$tag}))/2.0; 
			print OUT " " x $len;
			print OUT $$ref{$repeatername}->{$tag_id_key}[$k]{$tag};
			print OUT " " x $len;			
		    }
		    else
		    {
			my $len = (5 - length($$ref{$repeatername}->{$tag_id_key}[$k]{$tag}))/2.0; 
			print OUT " " x $len;
			print OUT " " . $$ref{$repeatername}->{$tag_id_key}[$k]{$tag};
		    }
		}
		print OUT "\n";
	    }
	}
    }
}


### MAIN PROGRAM ###################################################
my $file = "$Bin/quatest_tags_general_template_nested_rep.dat";
#my $file = "$Bin/quaS004_test_tags_nested_repeaters.dat";
#my $file = "$Bin/quaS004_test_tags.dat";

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
my $pdbfilename_rd = basename($$ref{'num_rigid_domains'}->{pdb_location}[0][0]);
$pdbfilename_rd =~ s{\.[^.]+$}{};  # removes extension;

open OUT_code, ">$p/code.usr";
print OUT_code $$ref{ "codenumber" };
close OUT_code;


$newdir =join "_", "Results", $pdbfilename_rd;
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
my @aglines, @rdlines, @explines, @fllines, @aaconstlines;

## File transformation ###########################
open(my $fh, '<', $file) or die "Can't read file '$file' [$!]\n";
while (my $line = <$fh>) {
    my $line_copy = $line;

    if ($line_copy =~ m/#AGREP/)
    {
	$line_copy =~ s/#AGREP//;
	chomp $line_copy;
	push (@aglines, $line_copy);
	next;
    }
    if ($line_copy =~ m/#LREP/)
    {
	$line_copy =~ s/#LREP//;
	chomp $line_copy;
	push (@fllines, $line_copy);
	next;
    }
    if ($line_copy =~ m/#RDREP/)
    {
	$line_copy =~ s/#RDREP//;
	chomp $line_copy;
	push (@rdlines, $line_copy);
	next;
    }
    if ($line_copy =~ m/#AACONST/)
    {
	$line_copy =~ s/#AACONST//;
	chomp $line_copy;
	push (@aaconstlines, $line_copy);
	next;
    }  	
    if ($line_copy =~ m/#EXPREP/)
    {
	$line_copy =~ s/#EXPREP//;
	chomp $line_copy;
	push (@explines, $line_copy);
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

    if ($line_copy =~ m/Aggregate  #AGBEGIN/)
    {
	$line_copy =~ s/#AGBEGIN//;
	
	my $hashrd = $$ref{'num_ags'}->{point_group_symmetry};
	my $length = scalar( @{ $hashrd } );

	for ( my $k = 0; $k < $length; $k++)
	{
	    print OUT $line_copy;
	    my @temp_rdlines = @aglines;
	    for (my $i = 0; $i < @temp_rdlines; $i++)
	    {
		if ($temp_rdlines[$i] =~ m/#TAG/)
		{
		    Process_tag_reps($temp_rdlines[$i], $k, 'num_ags');
		}
		elsif ($temp_rdlines[$i] =~ m/#T_CBR/)
		{
		    Process_t_cbr_reps($temp_rdlines[$i], $k, 'num_ags');
		}
		else
		{
		    $temp_rdlines[$i] =~ s/\s+$//;
		    print OUT $temp_rdlines[$i] ."\n";
		}
	    }
	}
    }
    
    elsif ($line_copy =~ m/Linker     #LINKERBEGIN/)
    {
	$line_copy =~ s/#LINKERBEGIN//;
	
	my $hashrd = $$ref{'num_flinkers'}->{primary_structure_from_n_to_cterm};
	my $length = scalar( @{ $hashrd } );

	for ( my $k = 0; $k < $length; $k++)
	{
	    print OUT $line_copy;
	    my @temp_rdlines = @fllines;
	    for (my $i = 0; $i < @temp_rdlines; $i++)
	    {
		if ($temp_rdlines[$i] =~ m/#TAG/)
		{
		    Process_tag_reps($temp_rdlines[$i], $k, 'num_flinkers');
		}
		elsif ($temp_rdlines[$i] =~ m/#AACTERMFL/)
		{
		    $temp_rdlines[$i] =~ s/#AACTERMFL//;
		    chomp $temp_rdlines[$i];
		    my $id_exp = (split(/[:]/, $temp_rdlines[$i]))[0];
		    
		    print OUT $id_exp.":". $$ref{'num_flinkers'}->{aa__of_the_pdb_of_the_previous_rd_cterm}[$k] ."\n";
		}
		else
		{
		    $temp_rdlines[$i] =~ s/\s+$//;
		    print OUT $temp_rdlines[$i] ."\n";
		}
	    }
	}
    }

    elsif ($line_copy =~ m/Rigid Domain/)
    {
	my $hashrd = $$ref{'num_rigid_domains'}->{pdb_location};
	my $length = scalar( @{ $hashrd } );

	#print "VAR: " .$$ref{'num_rigid_domains'}->{geom_center_vect}[0]{Startingmodulus_anggeom_center_vect};
	#exit;
	for ( my $k = 0; $k < $length; $k++)
	{
	    print OUT $line_copy;
	    my @temp_rdlines = @rdlines;
	    for (my $i = 0; $i < @temp_rdlines; $i++)
	    {
		if ($temp_rdlines[$i] =~ m/#TAG/)
		{
		    Process_tag_reps($temp_rdlines[$i], $k, 'num_rigid_domains');
		}
		elsif ($temp_rdlines[$i] =~ m/#T_CBR/)
		{
		    Process_t_cbr_reps($temp_rdlines[$i], $k, 'num_rigid_domains');
		}
		elsif ($temp_rdlines[$i] =~ m/#PDB/)
		{
		    Process_pdb_reps($temp_rdlines[$i], $k, 'num_rigid_domains', 'pdb_location');
		}
		else
		{
		    $temp_rdlines[$i] =~ s/\s+$//;
		    print OUT $temp_rdlines[$i] ."\n";
		}
	    }
	}
    }
    elsif ($line_copy =~ m/#AABEGIN/)
    {
	$line_copy =~ s/#AABEGIN//;
	print OUT $line_copy;

	my $hashrd = $$ref{'num_aaconst'}->{aa_number};
	my $length = scalar( @{ $hashrd } );
	
	for ( my $k = 0; $k < $length; $k++)
	{
	    my @temp_rdlines = @aaconstlines;
	    for (my $i = 0; $i < @temp_rdlines; $i++)
	    {
		if ($temp_rdlines[$i] =~ m/#SYNTAXAA/)
		{
		    Process_aasyntax_reps($temp_rdlines[$i], $k, 'num_aaconst');
		}
		else
		{
		    $temp_rdlines[$i] =~ s/\s+$//;
		    print OUT $temp_rdlines[$i] ."\n";
		}
	    }
	}
    }
    elsif ($line_copy =~ m/Number of Experiments/)
    {
	my $hashrd = $$ref{'num_exp'}->{exp_location};
	my $length = scalar( @{ $hashrd } );

	my $label = (split(/[:]/, $line_copy))[0];
	print OUT $label .":". $length . "\n";
	
	#print "VAR: " .$$ref{'num_rigid_domains'}->{geom_center_vect}[0]{Startingmodulus_anggeom_center_vect};
	#exit;
	for ( my $k = 0; $k < $length; $k++)
	{
	    my @temp_rdlines = @explines;
	    for (my $i = 0; $i < @temp_rdlines; $i++)
	    {
		if ($temp_rdlines[$i] =~ m/#TAG/)
		{
		    Process_tag_reps($temp_rdlines[$i], $k, 'num_exp');
		}
		elsif ($temp_rdlines[$i] =~ m/#T_CBR/)
		{
		    Process_t_cbr_reps($temp_rdlines[$i], $k, 'num_exp');
		}
		elsif ($temp_rdlines[$i] =~ m/#PDB/)
		{
		    Process_pdb_reps($temp_rdlines[$i], $k, 'num_exp', 'exp_location');
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
	$line_copy =~ s/#TAG//;
	chomp $line_copy;
	
	my $id = (split(/[:]/, $line_copy))[0];
	my $dval = (split(/[:]/, $line_copy))[-1];
	
	my $id_key = $id;
	$id_key =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
	$id_key =~ tr/ /_/;             # replace whitespace with underscore
	$id_key =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
	$id_key =~ s/_$//;              # remove underscore at the end 
	$id_key =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  

	$dval =~ s/\s+$//;
	$dval =~ s/^\s*(.*?)\s*$/$1/;   # remove whitespace from both ends

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
	    
	    if (exists $ref->{$id_key})
	    {
		print OUT "$id:";
		for ( my $j = 0; $j < @array1; $j++)
		{
		    my $tag = "$array_ids[$j]$id_key";
		    
		    if ($j != @array1 -1)
		    {
			my $len = (16 - length($$ref{$id_key}{$tag}))/2.0; 
			print OUT " " x $len;
			print OUT $$ref{$id_key}{$tag};
			print OUT " " x $len;			
		    }
		    else
		    {
			my $len = (5 - length($$ref{$id_key}{$tag}))/2.0; 
			print OUT " " x $len;
			print OUT " " . $$ref{$id_key}{$tag};
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

    ### LABELS #####################################################################
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
		    print OUT "$id_cbr:$dval_cbr_1:$$ref{$id_key}{$id}\n";
		}
		else
		{
		    my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
		    
		    print OUT "$id_cbr:$dval_cbr_1:";
		    for ( my $j = 0; $j < @array1; $j++)
		    {
			my $tag = "$array_ids[$j]$id$id_key";

			#print $id_key . "  " . $tag . "  " .$$ref{$id_key}{$tag};
			#exit; 

			if ($j != @array1 -1)
			{
			    my $len = (16 - length($$ref{$id_key}{$tag}))/2.0; 
			    print OUT " " x $len;
			    print OUT $$ref{$id_key}{$tag};
			    print OUT " " x $len;
			}
			else
			{
			    my $len = (5 - length($$ref{$id_key}{$tag}))/2.0; 
			    print OUT " " x $len;
			    print OUT " " . $$ref{$id_key}{$tag};
			}
			
		    }
		    print OUT "\n";
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
		    print OUT "$dval_cbr_11:$$ref{$tag_id_key}{$id}\n";
		}
		else
		{
		    my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
		    
		    print OUT "$dval_cbr_11:";
		    for ( my $j = 0; $j < @array1; $j++)
		    {
			my $tag = "$array_ids[$j]$id$tag_id_key";
			if ($j != @array1 -1)
			{
			    my $len = (16 - length($$ref{$tag_id_key}{$tag}))/2.0; 
			    print OUT " " x $len;
			    print OUT $$ref{$tag_id_key}{$tag};
			    print OUT " " x $len;			
			}
			else
			{
			    my $len = (5 - length($$ref{$tag_id_key}{$tag}))/2.0; 
			    print OUT " " x $len;
			    print OUT " " . $$ref{$tag_id_key}{$tag};
			}
		    }
		    print OUT "\n";
		}
	    }
	}
    }
    # END PROCESS LISTBOXES ##################
    elsif ($line_copy =~ m/#NUMAG/)
    {
	$line_copy =~ s/#NUMAG//;
	chomp $line_copy;
	my $id_exp = (split(/[:]/, $line))[0];
	#my $dval = (split(/[:]/, $line))[-1];
	
	#print OUT "$id_exp:$pdbfilename\n";
	my $size = $$ref{'num_ags'}->{point_group_symmetry};
	my $length = scalar( @{ $size } );
	print OUT $id_exp.":". $length."\n";
    }
    elsif ($line_copy =~ m/#NUMFL/)
    {
	$line_copy =~ s/#NUMFL//;
	chomp $line_copy;
	my $id_exp = (split(/[:]/, $line))[0];

	my $size = $$ref{'num_flinkers'}->{primary_structure_from_n_to_cterm};
	my $length = scalar( @{ $size } );
	
	if ($length)
	{
	    $line_copy =~ s/#L/$length/ig;
	}
	else
	{
	    $line_copy =~ s/#L/0/ig;
	}
	if (exists $$ref{'seq_rd_fl_checkbox'}->{seq_rd_fl_pdb})
	{
	    my $seq = $$ref{'seq_rd_fl_checkbox'}->{seq_rd_fl_pdb};
	    $line_copy =~ s/#SEQ/$seq/ig;
	}
	else
	{
	    $line_copy =~ s/#SEQ//ig;
	}
		
	print OUT $line_copy . "\n";
        #print OUT $id_exp.":". $length."\n";
    }
    elsif ($line_copy =~ m/#NUMRD/)
    {
	$line_copy =~ s/#NUMRD//;
	chomp $line_copy;
	my $id_exp = (split(/[:]/, $line))[0];
	#my $dval = (split(/[:]/, $line))[-1];
	
	my $size = $$ref{'num_rigid_domains'}->{pdb_location};
	my $length = scalar( @{ $size } );

	$line_copy =~ s/#NRD/$length/ig;
	print OUT $line_copy . "\n";
	#print OUT $id_exp.":". $length."\n";
    }
    elsif ($line_copy =~ m/#STONE_FLAG/)
    {
	$line_copy =~ s/#STONE_FLAG//;
	chomp $line_copy;
	my $id_exp = (split(/[:]/, $line))[0];
	
	if (exists $$ref{'stone_exp_flag'}->{stone_exp_name})
	{
	    print OUT $id_exp.":0\n";
	}
	else
	{
	    print OUT $id_exp.":1\n";
	}
    }
    elsif ($line_copy =~ m/#STONE_NAME/)
    {
	$line_copy =~ s/#STONE_NAME//;
	chomp $line_copy;
	my $id_exp = (split(/[:]/, $line))[0];
	
	if (exists $$ref{'stone_exp_flag'}->{stone_exp_name})
	{
	    my $symfilename = basename($$ref{'stone_exp_flag'}->{stone_exp_name}[0]);
	    $symfilename =~ s{\.[^.]+$}{}; # removes extension 1
	    my $familyname = substr($symfilename, 0, 4);

	    print OUT $id_exp.":". $familyname . "\n";
	}
	else
	{
	    print OUT $line_copy . "\n";
	}
    }
    else # THE REST
    {
	$line_copy =~ s/\s+$//;
	print OUT "$line_copy\n";
    }
}

####################################################

close OUT;

####################################################

print encode_json( $res ). "\n" ;



