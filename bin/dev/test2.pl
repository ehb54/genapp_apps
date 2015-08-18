#!/usr/bin/perl

my $running = 1;
my $jobid = 713431;
my $remotesys = 'gridchem@comet.sdsc.edu';

$output_dir = '/home/gridchem/vortexshedding/vortexshedding_proj/default_job.comet.sdsc.xsede.org.713479.150723';


# check for session.log
my @getresults = ( 'session.log',
                   'spr_ell.map',
                   'spr_ell.map2',
                   'spr_ell.mpg' 
    );

# setup to_get aa

{
    my %toget;

    foreach my $k ( @getresults ) {
        $toget{ $k }++;
    }

# check if they exist

    my $checkall = '{' . ( join ',',@getresults ) . "}";

    my $cmd = "ssh $remotesys 'cd $output_dir; ls -1 $checkall' 2>/dev/null\n";
    print $cmd;
    my @results = `$cmd`;
    grep chomp, @results;
    
    my $getem = '{' . ( join ',',@results ) . "}";
    if ( @results == 1 ) {
        $getem = join '',@results;
    }
        
    $cmd = "scp $remotesys:$output_dir/$getem .\n";
    
    print $cmd;

    foreach my $k ( @results ) {
        my $id = $k;
        $id =~ s/(\W|_)//g;
        print "$k -> $id\n";
        delete $toget{ $k };
    }

    my @missing;
    foreach my $k ( keys %toget ) {
        push @missing, $k;
    }

    print "missing " . ( join ",", @missing ) . "\n";
}
