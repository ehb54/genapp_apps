### USAGE
## vmd -dispdev none -psf some.psf -e strip-wat.vmd -args "*.dcd" 

foreach i $argv {
    mol addfile $i type {dcd} first 0 last -1 step 1 waitfor all top
    axes location off
    mol modselect 0 top "nucleic or protein"
    mol modstyle 0 top newcartoon
    #mol modcolor 0 top structure
    mol selection "name SOD or name CLA or name POT"
    mol addrep top
    mol modstyle 1 top vdw
    mol modcolor 1 top type
    mol selection "name OH2"
    mol addrep top
    mol modstyle 2 top points 3
    mol modcolor 2 top type
    
    set num [molinfo top get numframes]
    # loop through the frames
    for {set i 0} {$i < $num} {incr i} {
	# go to the given frame
	animate goto $i
	display update
	# take the picture
	set filename snap.[format "%04d" $i].pov
	render POV3 $filename
    }
}

quit
