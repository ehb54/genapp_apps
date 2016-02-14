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

open(my $fh, '<', $file) or die "Can't read file '$file' [$!]\n";
while (my $line = <$fh>) {
    #chomp $line;
    if ($line =~ m/#TAG/)
    {
	$line =~ s/#TAG//;
	chomp $line;
	
    #if ($line =~ /:/ && $line !~ m/:$/)
    #{
	++$num;
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

	my $id_numbered = $id;
	substr($id_numbered, 0, 0) = 100+$num;

	#print "$id_numbered\n";

	if ( length $id_key )
	{	
	    $data{ $id_numbered } = $dval;
	}
    }
}

###########################################################


$size = keys %data;

# this is a module file, any module specific info belongs here
print "\# this is a module file, any module specific info belongs here
{
   \"moduleid\" : \"quafit_test\",
   \"label\"    : \"QuaFit\",
   \"help\"     : \"help_module\",
   \"fields\"   : [";

for my $key (sort keys %data ) {
        my $value = $data{$key};
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
	
	if ( $label_key =~ m/(1\/0)/ )
        {
	    my $check_value;
	    $label_key =~ s/ \(1\/0\)//ig;
	    $id_key =~ s/_10//ig;

	    if ($value == 1)
	    {
		$check_value = "true";
	    }
	    if ($value == 0)
	    {
		$check_value = "false";
	    }
	    print "
                 { 
                     \"role\"     : \"input\",  
                     \"id\"       : \"$id_key\",
                     \"label\"    : \"$label_key\",
                     \"type\"     : \"checkbox\",  
                     \"checked\"  : \"$check_value\",
		     \"required\" : \"true\",
		     \"help\"     : \"\"
		 },"    
		;
 #            if ( $a < $size)
 #            {
 #               print ",";
 #            }
       }
        else
        {
  	    print "
		   { 
		     \"role\"     : \"input\",  
                     \"id\"       : \"$id_key\",
                     \"label\"    : \"$label_key\",
                     \"type\"     : \"text\",  
                     \"default\"  : \"$value\",
 		     \"required\" : \"true\",
 		     \"help\"     : \"\"	
 		 },"    
 		;
 #            if ( $a < $size)
 #            {
 #               print ",";
 #            }
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



#print Dumper(\$data);
#print Dumper($res);

