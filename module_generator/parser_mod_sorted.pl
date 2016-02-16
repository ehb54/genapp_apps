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
	#my $int_data = {};

	my @matches = $line =~ /:/g;   #Check for lines with more than 1 ":"
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

	    push (@array_cbr,$int_data);
	    
	}
	else 
	{
	    my $dval_cbr_11 = (split(/[:]/, $line))[0];
	    my $dval_cbr_22 = (split(/[:]/, $line))[-1];
	    
	    $$int_data{ $dval_cbr_11 } = $dval_cbr_22;
	    #push @array_cbr, {$dval_cbr_11 => $dval_cbr_22};
	    push @array_cbr, $int_data;
	}
	
	$data{ $id_numbered_cbr } = \@array_cbr;
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
   \"fields\"   : [";

for my $key ( sort keys %data ) 
{
    #my $value = $data{$key};
    
    my $hash_counter = 0;
    my @value = @{$data{$key}};
    
    for my $href ( @value ) {
	for my $role ( keys %$href ) {
	    #print "$role=$href->{$role} \n";
	    if (exists( $href->{$role}) )
	    {
		++$hash_counter;
	    }
	}
    }

    my $id_key = $key;    # id
    my $label_key = $key; # label
    ++$a;
    
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
	my $check_value;
	my $repeater;
	$label_key =~ s/ \(1\/0\)//ig;
	$id_key =~ s/_10//ig;
	
	if ( ($value[0] eq "1") || ($hash_counter != 0) )
	{
	    $check_value = "true";
	}
	
	if ( ($value[0] eq "0") )
	{
	    $check_value = "false";
	}

	if ($hash_counter != 0)
	{
	    $repeater = "true";
	}
	else 
	{ 
	    $repeater = "false";
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
		     \"help\"     : \"\"
		 },"    
		 ;
	
	for my $href ( @value ) {
	    for my $role ( keys %$href )
	    {
		my $id = $role;
		my $val = $$href{$role};
		$id =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
		$id =~ tr/ /_/;             # replace whitespace with underscore
		$id =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
		$id =~ s/_$//;              # remove underscore at the end 
		$id =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  
		
		$val =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
		
		print "
		   { 
		     \"role\"     : \"input\",  
                     \"id\"       : \"$id\",
                     \"label\"    : \"$id\",
                     \"type\"     : \"text\",  
                     \"default\"  : \"$val\",
 		     \"required\" : \"true\",
                     \"size\"     : \"6\",
                     \"repeat\"   : \"$id_key\",
                     \"norow\"    : \"true\",
 		     \"help\"     : \"\"	
 		 },"    
		 ;	    
	    }
	}
    }
    else
    {
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
  	    print "
		   { 
		     \"role\"     : \"input\",  
                     \"id\"       : \"$id_key\",
                     \"label\"    : \"$label_key\",
                     \"type\"     : \"text\",  
                     \"default\"  : \"$value[0]\",
 		     \"required\" : \"true\",
 		     \"help\"     : \"\"	
 		 },"    
		 ;
	}
    }
}
print "
	          { 
	             \"role\"     : \"output\",  
	             \"id\"       : \"outputfile\",
	             \"label\"    : \"Quafit Output\",
	             \"type\"     : \"file\"  
	          }"    
    ;


print "
             ], \n
   \"executable\"   : \"parser_gennap_modifier.pl\",
   \"submitpolicy\" : \"all\"
}\n"
    ;


