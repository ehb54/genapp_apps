#!/usr/bin/perl

$results = 'INFO: Currently the walltime is translated to cputime directly 

INFO: /home/gridchem/bin/dev/gridchem_common Job to be submitted using 24 processors for Hrs:Min _0:05_ in queue _compute_ with PSN _uic151_ 

INFO: we are in DOMAIN sdsc_comet

INFO: SOURCING /home/gridchem/bin/dev/gridchem_local_sdsc_comet (LOCAL STUFF ie variables that change from site to site)

DEBUG: DATE:Thu Jul 23 04:08:45 PDT 2015 -  COMMAND_RECEIVED: /home/gridchem/bin/dev/gridchem_common -C nek5000 -a uic151 -q compute -W 0:05 -n 24 -M 4000 -g emre -p vortexshedding_proj -e ccg-admin@gridchem.uits.iu.edu,emre@biochem.uthscsa.edu -O /home/gridchem/vortexshedding/emre/d8c01970-2652-11e5-8e4b-b3dd3ddb7487 -l 0 -j default_job /home/gridchem/vortexshedding/emre/d8c01970-2652-11e5-8e4b-b3dd3ddb7487/spr_ell.usr,spr_ell.rea,SIZE 

WARNING: problem with grid-proxy-info having ZERO timeleft line = subject  : /C=US/O=National Center for Supercomputing Applications/CN=Gridchem Chemistry Community User
issuer   : /C=US/O=National Center for Supercomputing Applications/OU=Certificate Authorities/CN=MyProxy CA 2013
identity : /C=US/O=National Center for Supercomputing Applications/CN=Gridchem Chemistry Community User
type     : end entity credential
strength : 2048 bits
path     : /tmp/x509up_u457935
timeleft : 0:00:00
INFO: Temporarily setting the  X509_USER_PROXY to /tmp/x509up_u457935 based on grid-proxy-info
INFO: setting the  X509_USER_PROXY to /home/gridchem/vortexshedding/emre/d8c01970-2652-11e5-8e4b-b3dd3ddb7487/x509up_u457935 and copied the cert there from /tmp
DEBUG: processors request based on input is  
DEBUG: processors request based on input is 24 
DEBUG: CPU_Time_arg=0:05 before  making it pure minutes
DEBUG: CPU_Time_arg=5 after making it pure minutes 
DEBUG: local_preamble_part1= 
INFO: cd\'ing into /home/gridchem/vortexshedding/emre/d8c01970-2652-11e5-8e4b-b3dd3ddb7487 

DEBUG: pwd is /home/gridchem/vortexshedding/emre/d8c01970-2652-11e5-8e4b-b3dd3ddb7487
SUBMITTING JOB
QSUB OUTPUT IS:
Submitted batch job 713469
JOBID_FROM_CLUSTER_SCRIPT= 713469
SCRATCHDIR_FROM_CLUSTER_SCRIPT= /home/gridchem/vortexshedding/vortexshedding_proj/default_job.comet.sdsc.xsede.org.713469.150723
JOB_SUBMITTED_DATE=150723
INFO: executing /usr/bin/qstat 713469

Job id              Name             Username        Time Use S Queue          
------------------- ---------------- --------------- -------- - ---------------
713469              default_job      gridchem        00:00:00 Q compute        
';

( my $jobid_submitted )            = $results =~ /\nSubmitted batch job (\d+)\n/;
( my $jobid_cluster_script )       = $results =~ /\nJOBID_FROM_CLUSTER_SCRIPT= (\d+)\n/;
( my $scratchdir_cluster_script )  = $results =~ /\nSCRATCHDIR_FROM_CLUSTER_SCRIPT= (.+)\n/;

print "
jobid_submitted $jobid_submitted
jobid_cluster_script $jobid_cluster_script
scratchdir_cluster_script $scratchdir_cluster_script
";

