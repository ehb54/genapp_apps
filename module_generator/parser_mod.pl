#!/usr/bin/perl

use JSON;
use Data::Dumper;


#my $file = 'qualast_cut.dat';

if ( !@ARGV )
{
    print "\"error\":\"program called with no arguments\"\n";
    exit;
}

my $file = @ARGV[0];

my %data;

open(my $fh, '<', $file) or die "Can't read file '$file' [$!]\n";
while (my $line = <$fh>) {
    chomp $line;
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
	    $data{ $id } = $dval;
	}
    }
}

###########################################################


$size = keys %data;

# this is a module file, any module specific info belongs here
print "\# this is a module file, any module specific info belongs here
{
   \"moduleid\" : \"quafit_module\",
   \"label\"    : \"QuaFit\",
   \"help\"     : \"help_module\",
   \"fields\"   : [";

for my $key ( keys %data ) {
        my $value = $data{$key};
	my $id_key = $key;    # id
	my $label_key = $key; # label
   
        ++$a;

	$id_key =~ s/^\s*(.*?)\s*$/$1/; # remove whitespace from both ends
	$id_key =~ tr/ /_/;             # replace whitespace with underscore
	$id_key =~ tr/a-zA-Z0-9_//cd;   # retain only alphanumeric and underscore
	$id_key =~ s/_$//;              # remove underscore at the end 
	$id_key =~ tr/A-Z/a-z/;         # replace uppercase to lowercase  

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
		 }"    
		;
             if ( $a < $size)
             {
                print ",";
             }
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
		 }"    
		;
             if ( $a < $size)
             {
                print ",";
             }
	}
}
print "
             ], \n
   \"executable\"   : \"your_executable\",
   \"submitpolicy\" : \"all\"
}\n"
    ;



#print Dumper(\$data);
#print Dumper($res);

