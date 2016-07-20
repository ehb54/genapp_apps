#!/bin/bash

/usr/local/bin/vmd -dispdev none -psf step2_solvator.xplor.psf -e vmd_movie_maker.tcl -args namd_equil.dcd > /dev/null

for file in snap.*.pov
do
    echo "camera {orthographic" >>  $file
    echo "location <0, 0, -1.5>" >> $file
    echo "rotate <90,0,0> " >> $file
    echo "}" >>  $file
    /usr/local/bin/povray -d -H420 -W420 $file > /dev/null
    rm $file
done


/usr/bin/ffmpeg -i snap.%04d.png -c:v libx264 -r 30 -pix_fmt yuv420p out_init.mp4 > /dev/null
/usr/bin/ffmpeg -y -i out_init.mp4 -c:v libvpx -crf 18 -b:v 1M "$1".webm > /dev/null 
/usr/bin/ffmpeg -y -i out_init.mp4 -c:v libx264 -preset slow -pix_fmt yuv420p -b:v 1M -profile:v baseline -level 3.0 -c:a copy "$1".mp4 > /dev/null
#/usr/bin/ffmpeg -y -i out_init.mp4 -c:v libvpx -crf 18 -b:v 1M out_proc.webm > /dev/null 
#/usr/bin/ffmpeg -y -i out_init.mp4 -c:v libx264 -preset slow -pix_fmt yuv420p -b:v 1M -profile:v baseline -level 3.0 -c:a copy out_proc.mp4 > /dev/null


rm *png
rm out_init.mp4
