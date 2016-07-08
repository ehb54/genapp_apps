#!/usr/bin/perl

use JSON;
use Data::Dumper;

#my $file = 'quatest_tags.dat';

if ( !@ARGV )
{
    print "\"error\":\"program called with no arguments\"\n";
    exit;
}

my $file = @ARGV[0];

my %data;

my $num;

my $int_data = {};

open(my $fh, '<', $file) or die "Can't read file '$file' [$!]\n";
while (my $line = <$fh>) {
    
 ## T_CBR #######################################################
    if ($line =~ m/#T_CBR/)
    {
	$line =~ s/#T_CBR//;
	chomp $line;
	
	my $id_cbr;
	my $id_cbr_numbered;
	my $dval_cbr;
	my @array_cbr = ();

	my @matches = $line =~ /:/g;   #Check for lines with more than 1 :'s
	my $count = @matches;
	if ($count > 1)
	{
	    ++$num;
	    $id_cbr = (split(/[:]/, $line))[0];
	    $dval_cbr_1 = (split(/[:]/, $line))[1];
	    $dval_cbr_2 = (split(/[:]/, $line))[-1];
	    
	    $$int_data{ $dval_cbr_1 } = $dval_cbr_2;
	    
	    $id_numbered_cbr = $id_cbr;
	    substr($id_numbered_cbr, 0, 0) = 100+$num;
	}
	else 
	{
	    if ( $line =~ m/#END/ )
	    {
		$line =~ s/#END//;
		chomp $line;
		my $dval_cbr_11 = (split(/[:]/, $line))[0];
		my $dval_cbr_22 = (split(/[:]/, $line))[-1];
		$$int_data{ $dval_cbr_11 } = $dval_cbr_22;
	
		push @array_cbr, $int_data;
		$data{ $id_numbered_cbr } = \@array_cbr;
		$int_data = {};                               # Need to CLEAR temporary hash!!!
	    }
	    else
	    {
		my $dval_cbr_11 = (split(/[:]/, $line))[0];
		my $dval_cbr_22 = (split(/[:]/, $line))[-1];
		$$int_data{ $dval_cbr_11 } = $dval_cbr_22;
	    }
	}
	
	#for my $href ( @{ $data{ $id_numbered_cbr }} ) {
	#    for my $role ( keys %$href ) {
        #	print "$role=$href->{$role} \n";
	#    }
	#}
    }
    
 ## TAG #######################################################
   if ($line =~ m/#TAG/)
    {
	$line =~ s/#TAG//;
	chomp $line;
	++$num;

	my $id;
	my $dval;
	my @array;
	
	if ($line =~ m/%%%%/)
	{
	    $id = $line;
	    $dval = "";
	}
	
	if ($line =~ /:/ && $line !~ m/:$/)
	{
	    $id = (split(/[:]/, $line))[0];
	    $dval = (split(/[:]/, $line))[-1];
	}
	
       	$dval =~ s/\s+$//;
	$dval =~ s/^\s*(.*?)\s*$/$1/;  # remove whitespace from both ends

	my $id_numbered = $id;
	substr($id_numbered, 0, 0) = 100+$num;

	push @array, $dval;
	$data{ $id_numbered } = \@array;
	#print $data { $id_numbered }[0] . "\n";
    }
}
###########################################################

#$size = keys %data;

print "\# this is a module file, any module specific info belongs here
{
   \"moduleid\" : \"quafit_test\",
   \"label\"    : \"QuaFit\",
   \"help\"     : \"help_module\",
   \"extendedjsoninputtags\" : \"true\",
   \"nowrap\"   : \"true\",
   \"fields\"   : [";

print "
                 { 
		    \"role\"       : \"input\",
	            \"id\"         : \"projectname\",
	            \"label\"      : \"Project name\",
	            \"type\"       : \"text\",
	            \"required\"   : \"true\",
	            \"help\"       : \"Enter a project name\",
	            \"specifiedproject\" : \"Run_\" 
		 },
                {
		    \"role\"       : \"input\",
	            \"id\"         : \"codenumber\",
	            \"label\"      : \"Code Number\",
	            \"type\"       : \"text\",
	            \"required\"   : \"true\",
	            \"help\"       : \"\"
                 },
       ";

for my $key ( sort keys %data ) 
{
    #my $value = $data{$key};
    
    my $hash_counter = 0;
    my $role_counter = 0;
    my $listbox_string;

    my @value = @{$data{$key}};
    
    for my $href ( @value ) {
	for my $role ( keys %$href ) 
	{
	    my $role_clean = $role;
	    
	    $role_clean =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
	    $role_clean =~ tr/ /_/;             # replace whitespace with underscore
	    $role_clean =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
	    $role_clean =~ s/_$//;              # remove underscore at the end 
	    $role_clean =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  
	    
	    $listbox_string = $role_clean . "~$role_clean" . "~" . $listbox_string; 
	    $role_counter = 0;    
	    while ($$href{$role} =~ /\S+/g)
	    {
		++$role_counter;
		
	    }
	    ++$hash_counter;
	}
    }
    
    $listbox_string =~ s/~$//; 

    my $id_key = $key;    # id
    my $label_key = $key; # label
        
    substr($id_key, 0, 3) = "";     # remove first character (index for sorting) 
    
    $id_key =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
    $id_key =~ tr/ /_/;             # replace whitespace with underscore
    $id_key =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
    $id_key =~ s/_$//;              # remove underscore at the end 
    $id_key =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  
    
    substr($label_key, 0, 3) = "";     # remove first character (index for sorting) 
    $label_key =~ tr/a-zA-Z0-9_(\/)[] //cd; 
    $label_key =~ s/^\s*(.*?)\s*$/$1/;
    
    if ( ($label_key =~ m/(1\/0)/) || ($hash_counter != 0) )
    {
	my $check_value = "false";
	my $repeater = "no";
	$label_key =~ s/ \(1\/0\)//ig;
	$id_key =~ s/_10//ig;
	
	if ( ($value[0] eq "1") || ($hash_counter != 0) )
	{
	    $check_value = "true";
	}
	if ($hash_counter != 0)
	{
	    $repeater = "yes";
	}

	# IF STONE EXP BLOCK ####################################
	if ($label_key eq 'Optimisation free fraction')
	{
	    print "\n";
	    open(my $fh, '<', 'stone_exp.json') or die "Can't read file '$file' [$!]\n";
	    while (my $line = <$fh>) {
		print $line;
	    }
	}

	## IF RIGID DOMAIN BLOCK ###################################
	my $repline = "\#";
	if ($label_key eq 'Geom Center Vect')
	{
	    print "\n";
	    open(my $fh, '<', 'rd_repeater.json') or die "Can't read file '$file' [$!]\n";
	    while (my $line = <$fh>) {
		print $line;
	    }
	}
	if ($label_key eq 'Geom Center Vect' or $label_key eq 'Rotation Angles')
	{
	    $repline = " \"repeat\" : \"num_rigid_domains\", ";
	}
	## Part fo EXPERIMENT block ####################### 
	if ($label_key eq 'Flag Background')
	{
	    $repline = " \"repeat\" : \"num_exp\", ";
	}
	## Part fo AGGREGATE block ####################### 
	if ($label_key eq 'Point Group Reference System' or $label_key eq 'Screw Polymerization Number' or $label_key eq 'Screw Axis Orientation' or $label_key eq 'Rotation along Screw Axis (deg)' or $label_key eq 'Screw Translation along z (Ang)' or $label_key eq 'Relative mass density hydr shell')
	{
	    $repline = " \"repeat\" : \"num_ags\", ";
	}

	print "
                 { 
                     \"role\"     : \"input\",  
                     \"id\"       : \"$id_key\",
                     \"label\"    : \"$label_key\",
                     \"type\"     : \"checkbox\",  
                     \"checked\"  : \"$check_value\",
		     \"required\" : \"true\",
                     \"repeater\" : \"$repeater\",
                   $repline
		     \"help\"     : \"\"
		 }," ;   
		 #;
                 #    " ;
	
	## COMMENTED OUT WHAT RELATES TO LISTBOXES ##########
	#if ( $role_counter > 1 )
	#{
	#    print "
        #         { 
        #             \"role\"     : \"input\",  
        #             \"id\"       : \"$id_key$id_key\",
        #             \"label\"    : \"\",
	#          "
        # 	  ;
	#    print "  \"type\"     : \"listbox\",
        #             \"values\"   : \"$listbox_string\",
        #             \"default\"  : \"\",
        #             \"repeater\" : \"yes\",
        #             \"repeat\"   : \"$id_key\",
       	#	     \"help\"     : \"\"
	#	 },"   
	#	 ;
	#}
	## END COMMENTED OUT WHAT RELATES TO LISTBOXES ##########

	#else
	#{
	#   print "   \"type\"     : \"checkbox\",  
        #             \"checked\"  : \"$check_value\",
 	#	     \"required\" : \"true\",
        #             \"repeater\" : \"$repeater\",
	#	     \"help\"     : \"\"
	#	 },"    
	#	 ;
	#}
	
	for my $href ( @value ) {
	    for my $role ( keys %$href )
	    {
		my $id = $role;
		my $val = $$href{$role};
		$val =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends

		$id =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
		$id =~ tr/ /_/;             # replace whitespace with underscore
		$id =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
		$id =~ s/_$//;              # remove underscore at the end 
		$id =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  
		
		if ( $role_counter == 1 )
		{
		    my $pattern = "\#";
		    my $sizetext = 4;
		    if ($id eq 'deg__beta' or $id eq 'alpha' or $id eq 'gamma')
		    {
			$pattern = "\"pattern\":\"^([-+])?(([1-9][0-9]*)|(0))?([.][0-9]+)?([eE][-+]?[0-9]+)?\$\"";
			$sizetext = 2;
		    }

		    print "
		   { 
		     \"role\"     : \"input\",  
                     \"id\"       : \"$id\",
                     \"label\"    : \"$id\",
                     \"type\"     : \"text\",  
                     \"default\"  : \"$val\",
 		     \"required\" : \"true\",
                     \"size\"     : $sizetext,
                     \"repeat\"   : \"$id_key\",
                     \"norow\"    : \"true\",
                     $pattern,
 		     \"help\"     : \"\"	
 		 },"    
		 ;
		}
		else
		{
		  # ADDED 'NAME (readonly)' INSTEAD OF LISTBOXES ##
		  print "
		              { 
		                \"role\"     : \"input\",  
                                \"id\"       : \"$id$id_key\",
                                \#\"label\"    : \"$id\",
                                \"label\"    : \"\",
                                \"type\"     : \"text\",  
                                \"default\"  : \"$id\",
 		                \"required\" : \"true\",
                                \"size\"     : 5,
                                \"repeat\"   : \"$id_key\",
                                \"norow\"    : \"true\",
                                \"readonly\" : \"true\",
 		                \"help\"     : \"\"	
 		           },"    
		  	     ;		
		    my @array1 = split /\s+/,$val;
		    my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );
		    for ( my $i = 0; $i < @array1; $i++)
		    {
			my $norow_stat = "true";
			my $sizetext = 2;
			my $type = "text";
			my $value = "\#";
			my $default = "\"default\":\"$array1[$i]\"";
			my $pattern = "\"pattern\":\"^([-+])?(([1-9][0-9]*)|(0))?([.][0-9]+)?([eE][-+]?[0-9]+)?\$\"";
			#my $pattern = "\"pattern\":\"^([-+])?[0-9]*([,.])?([0-9]+)?([eE][-+]?[0-9]+)?\$\"";
			if ($array_ids[$i] eq 'Flag')
			{
			    $norow_stat = "false";
			    $sizetext = 2; 
			    $type = "listbox";
			    $value = "\"values\":\"-1~minusone~0~zero~1~one~2~two\"";
			    $default = "\"default\": \"zero\"";
			    $pattern = "\#";
			}
			print "
		                { 
		                  \"role\"     : \"input\",  
                                  \"id\"       : \"$array_ids[$i]$id$id_key\",
                                  \"label\"    : \"$array_ids[$i]\",
                                  \"type\"     : \"$type\",  
                                  \"required\" : \"true\",
                                  \"size\"     : $sizetext,
                                  $value,
                                  $default,
                                  \#\"repeat\"   : \"$id_key$id_key:$id\",
                                  \"repeat\"   : \"$id_key\",
                                  \"norow\"    : \"$norow_stat\",
                                  $pattern, 
 		                  \"help\"     : \"\"	
 		             },"    
			     ;			
		    }
		}
	    }
	}
    }
    else
    {
	
	# IF STATES BLOCK ###################################
	my $repline = "\#";
	if ($label_key eq 'Number of MC States Configurations')
	{
	    print "\n";
	    open(my $fh, '<', 'states_repeater.json') or die "Can't read file '$file' [$!]\n";
	    while (my $line = <$fh>) {
		print $line;
	    }
	}
	if ($label_key eq 'Number of MC States Configurations')
	{
	    $repline = " \"repeat\" : \"num_states\", ";
	}	

	# IF AGGREGATES BLOCK ###################################
	#my $repline = "\#";
	if ($label_key eq 'Point Group Symmetry')
	{
	    print "\n";
	    open(my $fh, '<', 'agg_repeater.json') or die "Can't read file '$file' [$!]\n";
	    while (my $line = <$fh>) {
		print $line;
	    }
	}
	if ($label_key eq 'Point Group Symmetry' or $label_key eq 'Point Group Reference System' or $label_key eq 'Screw Polymerization Number' or $label_key eq 'Screw Axis Orientation' or $label_key eq 'Rotation along Screw Axis (deg)' or $label_key eq 'Screw Translation along z (Ang)' or $label_key eq 'Relative mass density hydr shell')
	{
	    $repline = " \"repeat\" : \"num_ags\", ";
	}	

	# IF FLEXIBLE LINKERS DATA BLOCK ###################################
	#my $repline = "\#";
	if ($label_key eq 'Primary structure from N to Cterm')
	{
	    print "\n";
	    open(my $fh, '<', 'fl_repeater.json') or die "Can't read file '$file' [$!]\n";
	    while (my $line = <$fh>) {
		print $line;
	    }
	}
	if ($label_key eq 'Flag for phi psi and chi variations')
	{
	    print "\n";
	    open(my $fh, '<', 'fl_repeater_aacterm.json') or die "Can't read file '$file' [$!]\n";
	    while (my $line = <$fh>) {
		print $line;
	    }
	}	
	if ($label_key eq 'Primary structure from N to Cterm' or $label_key eq 'RD  attached to the linkers Nterm' or $label_key eq 'AA  of the PDB of the previous RD' or $label_key eq 'RD  attached to the linkers Cterm' or $label_key eq 'Flag for phi psi and chi variations' or $label_key eq 'Starting values of phi angles' or $label_key eq 'Starting values of psi angles' or $label_key eq 'Starting values of chi angles') 
	{
	    $repline = " \"repeat\" : \"num_flinkers\", ";
	}	

	# IF EXPERIMENTAL/AA_CONSTRAINTS/PAIR_AA_CONSTRAINTS DATA BLOCK ###################################
	#my $repline = "\#";
	if ($label_key eq 'Concentration (g/l)')
	{
	    print "\n";
	    open(my $fh, '<', 'aa_constraints.json') or die "Can't read file '$file' [$!]\n";
	    while (my $line = <$fh>) {
		print $line;
	    }
	    print "\n";
	    open(my $fh, '<', 'pair_aa_constraints.json') or die "Can't read file '$file' [$!]\n";
	    while (my $line = <$fh>) {
		print $line;
	    }
	    print "\n";
	    open(my $fh, '<', 'exp_repeater.json') or die "Can't read file '$file' [$!]\n";
	    while (my $line = <$fh>) {
		print $line;
	    }
	}
	if ($label_key eq 'Concentration (g/l)' or $label_key eq 'XRays (10) / Neutrons (x_D2O) Flag' or $label_key eq 'Scatt Dens Solv (1012 cm Angs3)' or $label_key eq 'Minimum Q In Data File (Angs1)' or $label_key eq 'Maximum Q In Data File (Angs1)' or $label_key eq 'Parameter K for Poissonian Errors' or $label_key eq 'Number of Lines To Skip In Data File' or $label_key eq 'Num of QPoints To Average' or $label_key eq 'Maximum Distance for p(r) Calculation' or $label_key eq 'Scaling Factor (if 0 Is Optimized)' or $label_key eq 'Flag Background' or $label_key eq 'Family Number (Same Scaling Factor)')
	{
	    $repline = " \"repeat\" : \"num_exp\", ";
	}	
	
	
	if ($value[0] eq "")
	{
	    print "
		        { 
			  \"role\"     : \"input\",  
			  \"id\"       : \"$id_key\",
			  \"label\"    : \"$label_key\",
			  \"type\"     : \"label\",
			  \"prehline\" : \"true\",
			  \"posthline\": \"true\",
			  \"default\"  : \"\"
			},"  
			;
	}
	else
	{
	    ## Part of AGGREGATE block #######################################
	    #my $repline = "\#";
	    if ($label_key eq 'Point Group Symmetry' or $label_key eq 'Point Group Reference System' or $label_key eq 'Screw Polymerization Number' or $label_key eq 'Screw Axis Orientation' or $label_key eq 'Rotation along Screw Axis (deg)' or $label_key eq 'Screw Translation along z (Ang)' or $label_key eq 'Relative mass density hydr shell')
	    {
		$repline = " \"repeat\" : \"num_ags\", ";
	    }
	   	    
	    my $val = $value[0];
	    $val =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
	    
	    my @array1 = split /\s+/,$val;
	    my @array_ids = ('Starting', 'Lower', 'Upper', 'Sampling', 'Flag' );

	    if ( @array1 > 1 )
	    {
		print "
		   { 
		     \"role\"     : \"input\",  
                     \"id\"       : \"$id_key\",
                     \"label\"    : \"$label_key\",
                     \"type\"     : \"checkbox\",  
                     \"checked\"  : \"true\",
 		     \"required\" : \"true\",
                     \"repeater\" : \"yes\", 
                    $repline
                     \"help\"     : \"\"	
 		 },"    
		 ;
		
		for ( my $i = 0; $i < @array1; $i++)
		{
		   
		    my $norow_stat = "true";
		    my $sizetext = 2;
		    my $type = "text";
		    my $value = "\#";
		    my $default = "\"default\":\"$array1[$i]\"";
		    my $pattern = "\"pattern\":\"^([-+])?(([1-9][0-9]*)|(0))?([.][0-9]+)?([eE][-+]?[0-9]+)?\$\"";
		    #my $pattern = "\"pattern\":\"^([-+])?[0-9]*([,.])?([0-9]+)?([eE][-+]?[0-9]+)?\$\"";
 
		    if ($array_ids[$i] eq 'Flag')
		    {
			$norow_stat = "false";
			$sizetext = 2; 
			$type = "listbox";
			$value = "\"values\":\"-1~minusone~0~zero~1~one~2~two\"";
			$default = "\"default\": \"zero\"";
			$pattern = "\#";
		    }
		    print "
		                { 
		                  \"role\"     : \"input\",  
                                  \"id\"       : \"$array_ids[$i]$id_key\",
                                  \"label\"    : \"$array_ids[$i]\",
                                  \"type\"     : \"$type\",
                                  $value,
                                  \"required\" : \"true\",
                                  \"size\"     : $sizetext,
                                  $default,
                                  \"repeat\"   : \"$id_key\",
                                  $pattern,
                                  \"norow\"    : \"true\",
 		                  \"help\"     : \"\"	
 		             },"    
			     ;			
		}
	    }
	    else
	    {
		my $value = "\#";
		my $type = "text";
		my $default = "\"default\":\"$value[0]\"";
		if ($label_key eq 'Flag for phi psi and chi variations')
		{
		    $type = "listbox";
		    $value = "\"values\":\"0~zero~1~one~2~two\"";
		    $default = "\"default\":\"two\"";
		}
		my $pattern = "\#";
		my $sizetext = 6;
		if ($label_key eq 'deg__beta' or $label_key eq 'alpha' or $label_key eq 'gamma')
		{
		    $pattern = "\"pattern\":\"^([-+])?(([1-9][0-9]*)|(0))?([.][0-9]+)?([eE][-+]?[0-9]+)?\$\"";
		    #$pattern = "\"pattern\":\"^([-+])?[0-9]*([,.])?([0-9]+)?([eE][-+]?[0-9]+)?\$\"";
		    $sizetext = 2;
		}
		if ($label_key eq 'Screw Polymerization Number')
		{
		    $pattern = "\"pattern\":\"^((0)|([1-9][0-9]*))\$\"";   # Regex for Zero OR positive integers with no leading Zero
		}

		print "
		   { 
		     \"role\"     : \"input\",  
                     \"id\"       : \"$id_key\",
                     \"label\"    : \"$label_key\",
                     \"type\"     : \"$type\",
                     $value,
                     \"size\"     : $sizetext,
                     $default,
 		     \"required\" : \"true\",
                     $pattern,
                     $repline
 		     \"help\"     : \"\"	
 		 },"    
		 ;
	    }
	}
    }
}

#########  OUTPUT  ######################################################################
print "
	         # { 
	         #    \"role\"     : \"output\",  
	         #    \"id\"       : \"outputfile\",
	         #    \"label\"    : \"Quafit Parameter File: \",
	         #    \"type\"     : \"file\"  
	         # }, 
	          { 
	             \"role\"     : \"output\",  
	             \"id\"       : \"logfile\",
	             \"label\"    : \"Log File: \",
	             \"type\"     : \"file\"  
	          }, 
	          { 
	             \"role\"     : \"output\",  
	             \"id\"       : \"results\",
	             \"label\"    : \"File with Results: \",
	             \"type\"     : \"file\"  
	          }, 

	         {
                   \"role\"  : \"output\",
                   \"id\"    : \"quaternary_pdb\",
                   \"label\" : \"Assembled Quaternary Structure (right click for options): \", 
                   \"type\"  : \"atomicstructure\",
		   \"jsmoladd\" : \"hide HOH;color amino;spacefill;spin on\",
		   \"width\"     : 450,
	           \"height\"    : 450
		 }";

print "
             ], \n
   \"executable\"   : \"parser_gennap_modifier.pl\",
   \"submitpolicy\" : \"all\"
}\n"
    ;


