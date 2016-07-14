### USAGE
## vmd -dispdev none -psf some.psf -e strip-wat.vmd -args "*.dcd" 

foreach i $argv {
    mol addfile $i type {dcd} first 0 last -1 step 10 waitfor all top
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
