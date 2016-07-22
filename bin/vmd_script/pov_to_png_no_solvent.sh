#!/bin/bash

/usr/local/bin/vmd -dispdev none -psf step2_solvator.xplor.psf -e vmd_strip_water.tcl -args namd_equil.dcd > /dev/null

/usr/local/bin/vmd -dispdev none -psf step1_pdbreader.xplor.psf -e vmd_movie_maker.tcl -args namd_equil.nowat.dcd > /dev/null

numfiles=$(ls -l snap.*.pov | wc -l)
count=1
time=0.0

for file in snap.*.pov
do
    echo "Processing file $file: $count of $numfiles"
    echo "camera {orthographic" >>  $file
    echo "location <0, 0, -1.7>" >> $file
    echo "rotate <90,0,0> " >> $file
    echo "look_at <0,0,0> " >> $file
    echo "}" >>  $file

    time=`echo | awk '{print "'"$time"'" + "'"$1"'"}'`
    echo "text {" >>  $file
    echo "ttf \"timrom.ttf\" \"Time: $time ps\" 10, 0" >> $file
    #echo "pigment {Red}" >> $file
    echo "finish { reflection .5 specular 1 }" >> $file
    echo "scale 0.12                          //modifying the scale of text" >>  $file
    echo "rotate <90,0,0> " >> $file
    echo "translate  0.75*z-1.1*x               //moving text to the place we want" >>  $file

    echo "}" >>  $file

    /usr/local/bin/povray -d -H420 -W420 $file > /dev/null
    rm $file
    count=$((count+1))
done


/usr/bin/ffmpeg -i snap.%04d.png -c:v libx264 -r 30 -pix_fmt yuv420p out_init.mp4 > /dev/null
/usr/bin/ffmpeg -y -i out_init.mp4 -c:v libvpx -crf 18 -b:v 1M "$2".webm > /dev/null 
/usr/bin/ffmpeg -y -i out_init.mp4 -c:v libx264 -preset slow -pix_fmt yuv420p -b:v 1M -profile:v baseline -level 3.0 -c:a copy "$2".mp4 > /dev/null
#/usr/bin/ffmpeg -y -i out_init.mp4 -c:v libvpx -crf 18 -b:v 1M out_proc.webm > /dev/null 
#/usr/bin/ffmpeg -y -i out_init.mp4 -c:v libx264 -preset slow -pix_fmt yuv420p -b:v 1M -profile:v baseline -level 3.0 -c:a copy out_proc.mp4 > /dev/null


rm *png
rm out_init.mp4
