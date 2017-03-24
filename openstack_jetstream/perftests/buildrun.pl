#!/usr/bin/perl

@namdl = ( 
    "212o202gcc540"
    ,"212o202gcc540-O2"
    ,"212o202gcc540-O3"
    ,"212o202gcc630"
    ,"212o202gcc630-O2"
    ,"212o202gcc630-O3"
    ,"212o202icc"
    ,"212o202icc-O2"
    ,"212o202icc-O3"
    );

$ppn = 24;

@nodesl = ( 1, 2, 3, 4 );

@flavorl = ( 
    "h24", "m1.xlarge",
    ,"h44", "m1.xxlarge"
    );


$out = "";

$any = 0;

foreach $namd ( @namdl ) {
    foreach $nodes ( @nodesl ) {
        for ( $i = 0; $i < @flavorl; $i += 2 ) {
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
            }
        }
__EOF
    ;
        }
    }
}

print $out;
