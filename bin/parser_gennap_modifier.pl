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
    my $j = $_[1];
    my $k = $_[2];
    my $repeatername = $_[3];
    my $pdb_location = $_[4];
    my $ext = $_[5];
   
    
    $temp_rdlines =~ s/#PDB//;
    chomp $temp_rdlines;
    
    my $id = (split(/[:]/, $temp_rdlines))[0];
    my $id_key = $id;
    $id_key =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
    $id_key =~ tr/ /_/;             # replace whitespace with underscore
    $id_key =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
    $id_key =~ s/_$//;              # remove underscore at the end
    $id_key =~ tr/A-Z/a-z/;         # replace uppercase to lowercase 
    
    #print "VAR: " . $$ref{'num_states'}->{$repeatername}[$j]{$pdb_location}[$k][0]; 
    #exit;
    
    my $pdbfilename = basename($$ref{'num_states'}->{$repeatername}[$j]{$pdb_location}[$k][0]);
    if ($ext)
    {
	$pdbfilename =~ s{\.[^.]+$}{};  # removes extension;
    }
    print OUT $id.":". $pdbfilename ."\n";
}	

sub Process_pdb_reps_exp {
    
    my $temp_rdlines = $_[0];	   
    my $k = $_[1]; 
    my $repeatername = $_[2]; 
    my $pdb_location = $_[3]; 
    my $ext = $_[4];

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
    if ($ext)
    {
	$pdbfilename =~ s{\.[^.]+$}{};  # removes extension;
    }
    print OUT $id.":". $pdbfilename ."\n";
}	

sub Process_aasyntax_reps {

    my $temp_rdlines = $_[0];
    my $j = $_[1];
    my $k = $_[2];
    my $repeatername = $_[3];
       
    $temp_rdlines =~ s/#SYNTAXAA//;
    chomp $temp_rdlines;
    
    my $id_key = 'aa_number';
    my $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(5 - $len);			

    $id_key = 'rdfl_number';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(3 - $len);

    $id_key = 'sym_number';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(3 - $len);

    $id_key = 'screw_axis_number';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(4 - $len);
        
    $id_key = 'atom_code';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(6 - $len);

    $id_key = 'coor_vect';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(18 - $len);

    $id_key = 'condition';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(20 - $len);
    
    my $id_key_upper_level = 'val_name_vect';
    if (exists $$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{value}{val_name_vect_value})
    {
	$len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{value}{val_name_vect_value});
	print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{value}{val_name_vect_value};
	print OUT " " x ceil(18 - $len);
    }
    elsif (exists $$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{name}{val_name_vect_name})
    {
	$len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{name}{val_name_vect_name});
	print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{name}{val_name_vect_name};
	print OUT " " x ceil(18 - $len);
    }
    else
    {
	$len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]);
	print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k];
	print OUT " " x ceil(18 - $len);
    }
    
    $id_key = 'fact_value';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(10 - $len);

    $id_key = 'method';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(7 - $len);

    $id_key = 'k';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(9 - $len);

    $id_key = 'name';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(7 - $len);

    $id_key = 'flag';
    my $choice = $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    if ($choice eq 'minusone')
    {
	$choice = '-1';
    }
    if ($choice eq 'zero')
    {
	$choice = '0';
    }
    if ($choice eq 'one')
    {
	$choice = '1';
    }	
    if ($choice eq 'one')
    {
	$choice = '1';
    }	
    if ($choice eq 'two')
    {
	$choice = '2';
    }	
    $len = length($choice); 
    print OUT $choice;
    print OUT " " x ceil(4 - $len);


    print OUT "\n";
}	

sub Process_aapairsyntax_reps {

    my $temp_rdlines = $_[0];
    my $j = $_[1];
    my $k = $_[2];
    my $repeatername = $_[3];
       
    $temp_rdlines =~ s/#SYNTAXAAPAIR//;
    chomp $temp_rdlines;

    my $id_key = 'aa_number_i';
    my $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(5 - $len);			

    $id_key = 'rdfl_number_i';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(3 - $len);

    $id_key = 'sym_number_i';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(3 - $len);

    $id_key = 'screw_axis_number_i';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(4 - $len);
        
    $id_key = 'atom_code_i';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(7 - $len);

    $id_key = 'aa_number_j';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(5 - $len);			

    $id_key = 'rdfl_number_j';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(3 - $len);

    $id_key = 'sym_number_j';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(3 - $len);

    $id_key = 'screw_axis_number_j';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(4 - $len);
        
    $id_key = 'atom_code_j';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(7 - $len);



    $id_key = 'coor_vect_pair';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(18 - $len);

    $id_key = 'condition_pair';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(20 - $len);
    
    my $id_key_upper_level = 'val_name_vect_pair';
    if (exists $$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{value}{val_name_vect_value_pair})
    {
	$len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{value}{val_name_vect_value_pair});
	print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{value}{val_name_vect_value_pair};
	print OUT " " x ceil(18 - $len);
    }
    elsif (exists $$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{name}{val_name_vect_name_pair})
    {
	$len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{name}{val_name_vect_name_pair});
	print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]{name}{val_name_vect_name_pair};
	print OUT " " x ceil(18 - $len);
    }
    else
    {
	$len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k]);
	print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key_upper_level}[$k];
	print OUT " " x ceil(18 - $len);
    }
    
    $id_key = 'fact_value_pair';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(10 - $len);

    $id_key = 'method_pair';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(7 - $len);

    $id_key = 'k_pair';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(9 - $len);

    $id_key = 'name_pair';
    $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]); 
    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    print OUT " " x ceil(7 - $len);

    $id_key = 'flag_pair';
    my $choice = $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
    if ($choice eq 'minusone')
    {
	$choice = '-1';
    }
    if ($choice eq 'zero')
    {
	$choice = '0';
    }
    if ($choice eq 'one')
    {
	$choice = '1';
    }	
    if ($choice eq 'one')
    {
	$choice = '1';
    }	
    if ($choice eq 'two')
    {
	$choice = '2';
    }	
    $len = length($choice); 
    print OUT $choice;
    print OUT " " x ceil(4 - $len);

    print OUT "\n";

}

sub Process_tag_reps {
    
    my $line_copy = $_[0];
    my $j = $_[1];
    my $k = $_[2];
    my $repeatername = $_[3];

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
	    if ($dval == 0 && !(exists $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]))   # Checkboxes!!
	    {
		$line_copy =~ s/#TAG//;
		chomp $line_copy;
		$line_copy =~ s/\s+$//;
		print OUT $line_copy ."\n";
	    }
	    if ($dval == 1 && !(exists $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]))   # Checkboxes!!
	    {
		print OUT "$id:0\n"; 
	    }
	    if ($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k] eq "on")
	    {
		$$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k] = "1";
		print OUT "$id:$$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]\n";
	    }
	}
	else
	{
	    if ($id_key eq 'flag_for_phi_psi_and_chi_variations')
	    {
		my $choice = $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k];
		if ($choice eq 'minusone')
		{
		    $choice = '-1';
		}
		if ($choice eq 'zero')
		{
		    $choice = '0';
		}
		if ($choice eq 'one')
		{
		    $choice = '1';
		}	
		if ($choice eq 'one')
		{
		    $choice = '1';
		}	
		if ($choice eq 'two')
		{
		    $choice = '2';
		}
		
		print OUT "$id:$choice\n";
	    }
	    else
	    {
		print OUT "$id:$$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]\n";
	    }
	}
    }
    else
    {
	my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
	
	#if (!exists ($$ref{$repeatername}->{$id_key}[$k]))
	if ($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k] == null)
	{
	    $line_copy =~ s/#TAG//;
	    chomp $line_copy;
	    print OUT $line_copy . "\n"; 
	}
	else
	{
	    print OUT "$id:";
	    for ( my $g = 0; $g < @array1; $g++)
	    {
		my $tag = "$array_ids[$g]$id_key";
				
		if ($g != @array1 -1)
		{
		    #my $len = (16 - length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag}))/2.0; 
		    #print OUT " " x $len;
		    #print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};
		    #print OUT " " x $len;
		    
		    my $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag}); 
		    print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};
		    print OUT " " x ceil(16 - $len);
			
		}
		else
		{
		    #my $len = (5 - length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag}))/2.0; 
		    #print OUT " " x $len;
		    #print OUT " " . $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};
		    
		    my $choice = $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};
		    if ($choice eq 'minusone')
		    {
			$choice = '-1';
		    }
		    if ($choice eq 'zero')
		    {
			$choice = '0';
		    }
		    if ($choice eq 'one')
		    {
			$choice = '1';
		    }	
		    if ($choice eq 'one')
		    {
			$choice = '1';
		    }	
		    if ($choice eq 'two')
		    {
			$choice = '2';
		    }	

		    #my $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag}); 
		    #print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};
		    my $len = length($choice);
		    print OUT $choice;
		    print OUT " " x ceil(5 - $len);
		}
	    }
	    print OUT "\n";
	}
    }   
}


sub Process_tag_reps_exp {
    
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
	    for ( my $g = 0; $g < @array1; $g++)
	    {
		my $tag = "$array_ids[$g]$id_key";
				
		if ($g != @array1 -1)
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
    my $j = $_[1];
    my $k = $_[2];
    my $repeatername = $_[3];

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
	
	#if (!(exists $$ref{$repeatername}->{$id_key}[$k]))   # Checkboxes!!
	if ($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k] == null)   # Checkboxes!!
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
	    
	    #print " VAR test:  $id  " . $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$id};
	    #exit;
	    
	    my $val = $dval_cbr_2;
	    $val =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
	    my @array1 = split /\s+/,$val;
	    
	    if (@array1 == 1)
	    {
		print OUT "$id_cbr:$dval_cbr_1:$$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$id}\n";
	    }
	    else
	    {
		my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
		
		print OUT "$id_cbr:$dval_cbr_1:";
		for ( my $g = 0; $g < @array1; $g++)
		{
		    my $tag = "$array_ids[$g]$id$id_key";
		    
		    if ($g != @array1 -1)
		    {
			#my $len = (16 - length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag}))/2.0; 
			#print OUT " " x $len;
			#print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};
			#print OUT " " x $len;
			
			my $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag}); 
			print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};
			print OUT " " x ceil(16 - $len);

		    }
		    else
		    {
			#my $len = (5 - length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag}))/2.0; 
			#print OUT " " x $len;
			#print OUT " " . $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};

			my $choice = $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};
			if ($choice eq 'minusone')
			{
			    $choice = '-1';
			}
			if ($choice eq 'zero')
			{
			    $choice = '0';
			}
			if ($choice eq 'one')
			{
			    $choice = '1';
			}	
			if ($choice eq 'one')
			{
			    $choice = '1';
			}
			if ($choice eq 'two')
			{
			    $choice = '2';
			}
			
			#my $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag}); 
			#print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};
			my $len = length($choice);
			print OUT $choice;
			print OUT " " x ceil(5 - $len);

			#my $len = length($$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag}); 
			#print OUT $$ref{'num_states'}->{$repeatername}[$j]{$id_key}[$k]{$tag};
			#print OUT " " x ceil(5 - $len);
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
	if ($$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k] == null) 
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
		print OUT "$dval_cbr_11:$$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k]{$id}\n";
	    }
	    else
	    {
		my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
		
		print OUT "$dval_cbr_11:";
		for ( my $g = 0; $g < @array1; $g++)
		{
		    my $tag = "$array_ids[$g]$id$tag_id_key";
		    if ($g != @array1 -1)
		    {
			#my $len = (16 - length($$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k]{$tag}))/2.0; 
			#print OUT " " x $len;
			#print OUT $$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k]{$tag};
			#print OUT " " x $len;	
			
			my $len = length($$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k]{$tag}); 
			print OUT $$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k]{$tag};
			print OUT " " x ceil(16 - $len);
		
		    }
		    else
		    {
			#my $len = (5 - length($$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k]{$tag}))/2.0; 
			#print OUT " " x $len;
			#print OUT " " . $$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k]{$tag};

			my $choice = $$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k]{$tag};
			if ($choice eq 'minusone')
			{
			    $choice = '-1';
			}
			if ($choice eq 'zero')
			{
			    $choice = '0';
			}
			if ($choice eq 'one')
			{
			    $choice = '1';
			}	
			if ($choice eq 'one')
			{
			    $choice = '1';
			}
			if ($choice eq 'two')
			{
			    $choice = '2';
			}
			
			my $len = length($choice);
			print OUT $choice;
			print OUT " " x ceil(5 - $len);


			#my $len = length($$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k]{$tag}); 
			#print OUT $$ref{'num_states'}->{$repeatername}[$j]{$tag_id_key}[$k]{$tag};
			#print OUT " " x ceil(5 - $len);
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
my $pdbfilename_rd = basename($$ref{'num_states'}->{num_rigid_domains}[0]{pdb_location}[0][0]);
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


################################################################################

#print " VAR:  " . $$ref{'num_states'}->{num_rigid_domains}[1]{geom_center_vect}[1]{Startingmodulus_anggeom_center_vect};
#exit;

###############################################################################

my $output_filename = join "", "$p/qua", $$ref{ "codenumber" }, ".dat";
open OUT, ">$output_filename";

my $temp_input_file = 'qua_temp.dat';
open OUT_TEMP, ">$temp_input_file";

my $tag_id_key;
my @stlines, @aglines, @rdlines, @explines, @fllines, @aaconstlines, @aacodeflines, @aapairconstlines, @aapaircodeflines;

## File transformation ###########################
open(my $fh, '<', $file) or die "Can't read file '$file' [$!]\n";
while (my $line = <$fh>) {
    my $line_copy = $line;

    if ($line_copy =~ m/#STATES/)
    {
	$line_copy =~ s/#STATES//;
	chomp $line_copy;
	push (@stlines, $line_copy);
	next;
    } 

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
    
    if ($line_copy =~ m/#AACODEF/)
    {
	$line_copy =~ s/#AACODEF//;
	chomp $line_copy;
	push (@aacodeflines, $line_copy);
	next;
    }  	
    if ($line_copy =~ m/#AACONST/)
    {
	$line_copy =~ s/#AACONST//;
	chomp $line_copy;
	push (@aaconstlines, $line_copy);
	next;
    }  
    if ($line_copy =~ m/#AAPAIRCODEF/)
    {
	$line_copy =~ s/#AAPAIRCODEF//;
	chomp $line_copy;
	push (@aapaircodeflines, $line_copy);
	next;
    }  	
    if ($line_copy =~ m/#AAPAIRCONST/)
    {
	$line_copy =~ s/#AAPAIRCONST//;
	chomp $line_copy;
	push (@aapairconstlines, $line_copy);
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

    my $hash_st = $$ref{'num_states'}->{number_of_mc_states_configurations};
    my $length_st = scalar( @{ $hash_st } );
    
    if ($line_copy =~ m/State   #STBEGIN/)
    {
	$line_copy =~ s/#STBEGIN//;
	
	for ( my $j = 0; $j < $length_st; $j++)
	{
	    print OUT $line_copy;
	
	    # STATES ############################
	    my @temp_lines = @stlines;
	    for (my $i = 0; $i < @temp_lines; $i++)
	    {
		if ($temp_lines[$i] =~ m/#NUMMC/)
		{
		    $temp_lines[$i] =~ s/#NUMMC//;
		    chomp $temp_lines[$i];
		    my $id_exp = (split(/[:]/, $temp_lines[$i]))[0];
		    
		    my $length = $$ref{'num_states'}->{number_of_mc_states_configurations}[$j];
		    
		    $temp_lines[$i] =~ s/#NMC/$length/ig;
		    print OUT $temp_lines[$i] . "\n";
		}

		elsif ($temp_lines[$i] =~ m/#NUMRD/)
		{
		    $temp_lines[$i] =~ s/#NUMRD//;
		    chomp $temp_lines[$i];
		    my $id_exp = (split(/[:]/, $temp_lines[$i]))[0];
		    #my $dval = (split(/[:]/, $line))[-1];
		    
		    my $size = $$ref{'num_states'}->{num_rigid_domains}[$j]{pdb_location}[0];
		    my $length = scalar( @{ $size } );
		    
		    $temp_lines[$i] =~ s/#NRD/$length/ig;
		    print OUT $temp_lines[$i] . "\n";
		    #print OUT $id_exp.":". $length."\n";
		}
		elsif ($temp_lines[$i] =~ m/#NUMFL/)
		{
		    $temp_lines[$i] =~ s/#NUMFL//;
		    chomp $temp_lines[$i];
		    my $id_exp = (split(/[:]/, $line))[0];
		    
		    my $size = $$ref{'num_states'}->{num_flinkers}[$j]{primary_structure_from_n_to_cterm};
		    my $length = scalar( @{ $size } );
		    
		    if ($length)
		    {
			$temp_lines[$i] =~ s/#L/$length/ig;
		    }
		    else
		    {
			$temp_lines[$i] =~ s/#L/0/ig;
		    }
		    if (exists $$ref{'num_states'}->{seq_rd_fl_checkbox}[$j]{seq_rd_fl_pdb})
		    {
			my $seq = $$ref{'num_states'}->{seq_rd_fl_checkbox}[$j]{seq_rd_fl_pdb};
			$temp_lines[$i] =~ s/#SEQ/$seq/ig;
		    }
		    else
		    {
			$temp_lines[$i] =~ s/#SEQ//ig;
		    }
		    
		    print OUT $temp_lines[$i] . "\n";
		    #print OUT $id_exp.":". $length."\n";
		}
		
		elsif ($temp_lines[$i] =~ m/#NUMAG/)
		{
		    $temp_lines[$i] =~ s/#NUMAG//;
		    chomp $temp_lines[$i];
		    my $id_exp = (split(/[:]/, $temp_lines[$i]))[0];
		    #my $dval = (split(/[:]/, $line))[-1];
		    
		    #print OUT "$id_exp:$pdbfilename\n";
		    my $size = $$ref{'num_states'}->{num_ags}[$j]{point_group_symmetry};
		    my $length = scalar( @{ $size } );
		    print OUT $id_exp.":". $length."\n";
		}
		else
		{
		    $temp_lines[$i] =~ s/\s+$//;
		    print OUT $temp_lines[$i] ."\n";
		}
	    }
	    # AGGREGATES #################################
	    my $hash_ag = $$ref{'num_states'}->{'num_ags'}[$j]{point_group_symmetry};
	    my $length_ag = scalar( @{ $hash_ag } );
	    
	    for ( my $k = 0; $k < $length_ag; $k++)
	    {
		#print OUT $line_copy;
		my @temp_aglines = @aglines;
		for (my $i = 0; $i < @temp_aglines; $i++)
		{
		    if ($temp_aglines[$i] =~ m/#TAG/)
		    {
			Process_tag_reps($temp_aglines[$i], $j, $k, 'num_ags');
		    }
		    elsif ($temp_aglines[$i] =~ m/#T_CBR/)
		    {
		        Process_t_cbr_reps($temp_aglines[$i], $j, $k, 'num_ags');
		    }
		    else
		    {
			$temp_aglines[$i] =~ s/\s+$//;
			print OUT $temp_aglines[$i] ."\n";
		    }
		}
	    }
	    # RIGID DOMAINS  ############################
	    my $hash_rd = $$ref{'num_states'}->{num_rigid_domains}[$j]{pdb_location};
	    my $length_rd = scalar( @{ $hash_rd } );
	    
	    #print "VAR: " .$$ref{'num_rigid_domains'}->{geom_center_vect}[0]{Startingmodulus_anggeom_center_vect};
	    #exit;
	    for ( my $k = 0; $k < $length_rd; $k++)
	    {
		#print OUT $line_copy;
		my @temp_rdlines_int = @rdlines;
		for (my $i = 0; $i < @temp_rdlines_int; $i++)
		{
		    if ($temp_rdlines_int[$i] =~ m/#TAG/)
		    {
			Process_tag_reps($temp_rdlines_int[$i], $k, 'num_rigid_domains');
		    }
		    elsif ($temp_rdlines_int[$i] =~ m/#T_CBR/)
		    {
			Process_t_cbr_reps($temp_rdlines_int[$i], $j, $k, 'num_rigid_domains');
		    }
		    elsif ($temp_rdlines_int[$i] =~ m/#PDB/)
		    {
			Process_pdb_reps($temp_rdlines_int[$i], $j, $k, 'num_rigid_domains', 'pdb_location', 1);
		    }
		    else
		    {
			$temp_rdlines_int[$i] =~ s/\s+$//;
			print OUT $temp_rdlines_int[$i] ."\n";
		    }
		}
	    }
	    # FLEXIBLE LINKERS ##############################
	    
	    my $hash_fl = $$ref{'num_states'}->{num_flinkers}[$j]{primary_structure_from_n_to_cterm};
	    my $length_fl = scalar( @{ $hash_fl } );
	    
	    for ( my $k = 0; $k < $length_fl; $k++)
	    {
		#print OUT $line_copy;
		my @temp_fllines = @fllines;
		for (my $i = 0; $i < @temp_fllines; $i++)
		{
		    if ($temp_fllines[$i] =~ m/#TAG/)
		    {
			Process_tag_reps($temp_fllines[$i], $j, $k, 'num_flinkers');
		    }
		    elsif ($temp_fllines[$i] =~ m/#AACTERMFL/)
		    {
			$temp_fllines[$i] =~ s/#AACTERMFL//;
			chomp $temp_fllines[$i];
			my $id_exp = (split(/[:]/, $temp_fllines[$i]))[0];
			
			print OUT $id_exp.":". $$ref{'num_states'}->{num_flinkers}[$j]{aa__of_the_pdb_of_the_previous_rd_cterm}[$k] ."\n";
		    }
		    else
		    {
			$temp_fllines[$i] =~ s/\s+$//;
			print OUT $temp_fllines[$i] ."\n";
		    }
		}
	    }
	    
	    ## AA CONSTRAITNS #####################################
	    my @temp_aalines_def = @aacodeflines;
	    for (my $i = 0; $i < @temp_aalines_def; $i++)
	    {
		$temp_aalines_deff[$i] =~ s/\s+$//;
		print OUT $temp_aalines_def[$i] ."\n";
	    }
	    
	    my $hash_aa = $$ref{'num_states'}->{num_aaconst}[$j]{aa_number};
	    my $length_aa = scalar( @{ $hash_aa } );
	    
	    for ( my $k = 0; $k < $length_aa; $k++)
	    {
		my @temp_aalines = @aaconstlines;
		for (my $i = 0; $i < @temp_aalines; $i++)
		{
		    if ($temp_aalines[$i] =~ m/#SYNTAXAA/)
		    {
			Process_aasyntax_reps($temp_aalines[$i], $j, $k, 'num_aaconst');
		    }
		    else
		    {
			$temp_aalines[$i] =~ s/\s+$//;
			print OUT $temp_aalines[$i] ."\n";
		    }
		}
	    }
	    
	    # PAIR AA CONSTRAINTS ################################
	    my @temp_aapairlines_def = @aapaircodeflines;
	    for (my $i = 0; $i < @temp_aapairlines_def; $i++)
	    {
		$temp_aapairlines_deff[$i] =~ s/\s+$//;
		print OUT $temp_aapairlines_def[$i] ."\n";
	    }

	    my $hash_pair = $$ref{'num_states'}->{num_pair_aaconst}[$j]{aa_number_i};
	    my $length_pair = scalar( @{ $hash_pair } );
	    
	    for ( my $k = 0; $k < $length_pair; $k++)
	    {
		my @temp_pairlines = @aapairconstlines;
		for (my $i = 0; $i < @temp_pairlines; $i++)
		{
		    if ($temp_pairlines[$i] =~ m/#SYNTAXAAPAIR/)
		    {
			Process_aapairsyntax_reps($temp_pairlines[$i], $j, $k, 'num_pair_aaconst');
		    }
		    else
		    {
			$temp_pairlines[$i] =~ s/\s+$//;
			print OUT $temp_pairlines[$i] ."\n";
		    }
		}
	    }
	}
    }
    
    # EXPERIMENTS ##############################################################
    elsif ($line_copy =~ m/Number of Experiments/)
    {
	my $hash_exp = $$ref{'num_exp'}->{exp_location};
	my $length_exp = scalar( @{ $hash_exp } );

	my $label = (split(/[:]/, $line_copy))[0];
	print OUT $label .":". $length_exp . "\n";
	
	#print "VAR: " .$$ref{'num_rigid_domains'}->{geom_center_vect}[0]{Startingmodulus_anggeom_center_vect};
	#exit;
	for ( my $k = 0; $k < $length_exp; $k++)
	{
	    my @temp_explines = @explines;
	    for (my $i = 0; $i < @temp_explines; $i++)
	    {
		if ($temp_explines[$i] =~ m/#TAG/)
		{
		    Process_tag_reps_exp($temp_explines[$i], $k, 'num_exp');
		}
		elsif ($temp_explines[$i] =~ m/#PDB/)
		{
		    Process_pdb_reps_exp($temp_explines[$i], $k, 'num_exp', 'exp_location');
		}
		else
		{
		    $temp_explines[$i] =~ s/\s+$//;
		    print OUT $temp_explines[$i] ."\n";
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
		if($id_key eq 'icolumn3')
		{
		    my $choice = $$ref{$id_key};
		    if ($choice eq 'zero')
		    {
			$choice = 0;
		    }
		    if ($choice eq 'one')
		    {
			$choice = 1;
		    }
		    print OUT "$id:$choice\n";
		}
		else
		{
		    print OUT "$id:$$ref{$id_key}\n";
		}
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
			#my $len = (16 - length($$ref{$id_key}{$tag}))/2.0; 
			#print OUT " " x $len;
			#print OUT $$ref{$id_key}{$tag};
			#print OUT " " x $len;	

			my $len = length($$ref{$id_key}{$tag}); 
			print OUT $$ref{$id_key}{$tag};
			print OUT " " x ceil(16 - $len);
		    }
		    else
		    {
			#my $len = (5 - length($$ref{$id_key}{$tag}))/2.0; 
			#print OUT " " x $len;
			#print OUT " " . $$ref{$id_key}{$tag};

			my $len = length($$ref{$id_key}{$tag}); 
			print OUT $$ref{$id_key}{$tag};
			print OUT " " x ceil(5 - $len);
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
			    #my $len = (16 - length($$ref{$id_key}{$tag}))/2.0; 
			    #print OUT " " x $len;
			    #print OUT $$ref{$id_key}{$tag};
			    #print OUT " " x $len;

			    my $len = length($$ref{$id_key}{$tag}); 
			    print OUT $$ref{$id_key}{$tag};
			    print OUT " " x ceil(16 - $len);
			}
			else
			{
			    #my $len = (5 - length($$ref{$id_key}{$tag}))/2.0; 
			    #print OUT " " x $len;
			    #print OUT " " . $$ref{$id_key}{$tag};
			    
			    my $len = length($$ref{$id_key}{$tag}); 
			    print OUT $$ref{$id_key}{$tag};
			    print OUT " " x ceil(5 - $len);
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
			    #my $len = (16 - length($$ref{$tag_id_key}{$tag}))/2.0; 
			    #print OUT " " x $len;
			    #print OUT $$ref{$tag_id_key}{$tag};
			    #print OUT " " x $len;	

			    my $len = length($$ref{$tag_id_key}{$tag}); 
			    print OUT $$ref{$tag_id_key}{$tag};
			    print OUT " " x ceil(16 - $len);
			}
			else
			{
			    #my $len = (5 - length($$ref{$tag_id_key}{$tag}))/2.0; 
			    #print OUT " " x $len;
			    #print OUT " " . $$ref{$tag_id_key}{$tag};

			    my $len = length($$ref{$tag_id_key}{$tag}); 
			    print OUT $$ref{$tag_id_key}{$tag};
			    print OUT " " x ceil(16 - $len);
			}
		    }
		    print OUT "\n";
		}
	    }
	}
    }
    # END PROCESS LISTBOXES ##################
    elsif ($line_copy =~ m/#NUMST/)
    {
	$line_copy =~ s/#NUMST//;
	chomp $line_copy;
	my $id_exp = (split(/[:]/, $line))[0];
	#my $dval = (split(/[:]/, $line))[-1];
	
	#print OUT "$id_exp:$pdbfilename\n";
	my $size = $$ref{'num_states'}->{number_of_mc_states_configurations};
	my $length = scalar( @{ $size } );
	print OUT $id_exp.":". $length."\n";
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

#exit;

####################################################

# setup up the socket handle once:
my $sock = IO::Socket::INET->new( Proto => 'udp', PeerPort => $$ref{ "_udpport" }, PeerAddr => $$ref{ "_udphost" } );
$msg = {};
$$msg{ "_uuid" } = $$ref{ "_uuid" };

##############################################

open my $command, "$Bin/quafit_source/quafit |";
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
close $command;
close $output;

$$msg{ "_textarea" } = "\nCalculations Finished! \n";
$sock->send( encode_json( $msg ) );

##################################################### 

my $mask1 = join "", "*", $$ref{ "codenumber" }, "*.cst";
my $mask2 = join "", "*", $$ref{ "codenumber" }, "*.csw"; 
unlink glob($mask1);
unlink glob($mask2);

my $old_loc = join "", "$p/qua", $$ref{ "codenumber" }, "*.*";
my $arc_dir = $newdir;

for my $file (glob $old_loc) {
    move ($file, $arc_dir) or die $!;
}

my $archive_name = join "", "results_run_", $$ref{"projectname"}, "_", $pdbfilename, ".tar.gz";
system("tar", "czf", "$archive_name", "$newdir");

#$$res{ "outputfile" } = join "", $p, "/", $newdir, "/", "qua", $$ref{ "codenumber" }, ".dat";
$$res{ "logfile" } = "$p/logfile.txt";
$$res{ "results" } = "$p/$archive_name";


my $num_subruns_length = length( $$ref{"maximum_number_of_subrun"} );
my $num_subruns_formatted;

if ($num_subruns_length == 1)
{
    $num_subruns_formatted = join "", 0, 0, $$ref{"maximum_number_of_subrun"};
}
else {
    if ($num_subruns_length == 2)
    {
	$num_subruns_formatted = join "", 0, $$ref{"maximum_number_of_subrun"};
    }
    else {
	if ($num_subruns_length == 3)  ## Number of SubRuns must be between 0 and 999 ??
	{
	    $num_subruns_formatted = $$ref{"maximum_number_of_subrun"};
	}
    }
}

my $pdbpartname = join "", "qua", $$ref{ "codenumber" }, $num_subruns_formatted;
#print $pdbpartname;
opendir(DIR, $newdir) || die "Cannot open $data_dir\n";
@files = grep { /^${pdbpartname}_[0-9]{9}\.pdb$/ } readdir(DIR);
closedir(DIR);

@files = reverse sort @files;
$$res{ "quaternary_pdb" } = join "", $p, "/", $newdir, "/",  @files[0];

print encode_json( $res ). "\n" ;




