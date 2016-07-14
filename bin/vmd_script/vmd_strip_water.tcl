### USAGE
## vmd -parm some.parm -e thiscript -args list-of-dcd-files.dcd 

set notwat [atomselect top {not (water or name "CLA" "SOD" "POT") } ]

foreach i $argv {
    mol addfile $i type {dcd} first 0 last -1 step 1 waitfor all top
    set num [molinfo top get numframes]
    incr num -1
    set out [string map {.dcd .nowat.dcd} $i ]
    animate write dcd $out beg 0 end $num skip 1 waitfor all sel $notwat top
    animate delete all
}

quit 

