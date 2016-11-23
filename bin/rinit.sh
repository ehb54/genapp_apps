#!/bin/sh


source /etc/bashrc
source /etc/profile

module load mpi/openmpi/2.0.1/gcc-5.4.0

#ssh -o "StrictHostKeyChecking=no" 10.0.0.84 echo
#ssh -o "StrictHostKeyChecking=no" 10.0.0.85 echo
#ssh -o "StrictHostKeyChecking=no" 10.0.0.86 echo
#ssh -o "StrictHostKeyChecking=no" 10.0.0.87 echo

#export ORTE_DAEMON_UMASK_VALUE=0007

export PATH=$PATH:/usr/local/bin

exec /opt/genapp/namdrunner/bin/namdrun_oscluster.py "$@"

#echo `printenv`
