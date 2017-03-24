#!/bin/bash

cd ~/namdrunner/perf && rsync -avz genappcentos.openstack:ostest . 
cd ostest && perl namd_perf_sum.pl namd_perf.txt && gnuplot <<EOF
set term png size 1100,900 enhanced font Times 18
set output "benchmark.png"
set title "namd Jetstream performance 'benchmark'\n50k atoms, 96 cores (4*xlarge)"
set xlabel "date time UTC"
set ylabel "days/ns"
set yrange [0:]
set xdata time
set timefmt "%Y-%m-%dT%H:%M:%S+00:00"
set format x "%m-%d\n%H:%M"
plot "plotbenchmark.txt" using 1:2 with boxes title ""
set output
set term png size 1100,900 enhanced font Times 18
set output "timing.png"
set title "namd Jetstream performance 'timing'\n50k atoms, 96 cores (4*xlarge)"
set xlabel "date time UTC"
set ylabel "cpu s/step"
set yrange [0:]
set xdata time
set timefmt "%Y-%m-%dT%H:%M:%S+00:00"
set format x "%m-%d\n%H:%M"
plot "plottiming.txt" using 1:2 with boxes title ""
set output
set term png size 1100,900 enhanced font Times 18
set output "timing2.png"
set title "namd Jetstream performance 'timing'\n50k atoms, 96 cores (4*xlarge)"
set xlabel "date time UTC"
set ylabel "cpu s/step"
set yrange [0:0.1]
set xdata time
set timefmt "%Y-%m-%dT%H:%M:%S+00:00"
set format x "%m-%d\n%H:%M"
plot "plottiming.txt" using 1:2 with boxes title ""
set output
EOF
