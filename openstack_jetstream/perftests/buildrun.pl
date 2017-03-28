#!/usr/bin/perl

@namdl = ( 
    "212o202gcc540"
    ,"212o202gcc540O2"
#    ,"212o202gcc540O3"
    ,"212o202gcc630"
    ,"212o202gcc630O2"
#    ,"212o202gcc630O3"
    ,"212o202icc"
    ,"212o202iccO2"
#    ,"212o202iccO3"
#    ,"212o202iccipo"
#    ,"212o202iccxhost"
    ,"212o202iccxhostipo"
    );

@ppnl = ( 22, 24 );

@nodesl = ( 1, 2, 3, 4 );

@flavorl = ( 
    "h24", "m1.xlarge",
    ,"h44", "m1.xxlarge"
    );


$out = "";

$any = 0;

$runs = 0;

foreach $namd ( @namdl ) {
    foreach $ppn ( @ppnl ) {
        for ( $i = 0; $i < @flavorl; $i += 2 ) {
            foreach $nodes ( @nodesl ) {
                $flavor_tag = $flavorl[ $i ];
                $flavor = $flavorl[ $i + 1 ];
                $procs = $nodes * $ppn;
                $tag = "p${procs}-${flavor_tag}-$namd";

                $comma = "," if $any++;

                $out .= <<__EOF
        ${comma}{
            "tag"   : "$tag"
            ,"prerun"  : "rm namdmd_log.out 2> /dev/null"
            ,"modify" :
            {
                "namdversion" : "$namd"
                ,"os_flavor"  : "$flavor"
                ,"os_ppn"     : $ppn
                ,"os_nodes"   : $nodes
                ,"cmd_timeout" : "6m"
            }
        }
__EOF
    ;
                $runs++;
            }
        }
    }
}

print $out;
print "total runs=$runs\n";

