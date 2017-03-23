#!/usr/bin/python

import os
import glob
import re
import shutil
import sys
import subprocess
import json
import cStringIO
import shlex 
import socket
import time
import pty
import select
import atexit
import signal
import functools
import itertools
import math

from itertools import groupby


from StringIO import StringIO
#from math import exp, expm1, log10, log, log1p
from subprocess import Popen, PIPE, STDOUT

if (len(sys.argv) < 1):
    print "\{\"error\":\"module called with no arguments\"\}\n";

udp_errors = 0
udp_errors_after_retry = 0

##############################################################################
def message_box(text,icon):
    
    global udp_errors
    global udp_errors_after_retry

    _message = {}
    _message['icon'] = icon
    _message['text'] = text
    
    UDP_IP = json_variables['_udphost']
    UDP_PORT = json_variables['_udpport']
    sock = socket.socket(socket.AF_INET, # Internet
                         socket.SOCK_DGRAM) # UDP
    
    socket_dict={}
    socket_dict['_uuid'] = json_variables['_uuid']
    socket_dict['_message'] = _message

    doc_string = json.dumps(socket_dict)
    try:
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))
    except:
        # try again after 1 second
        udp_errors += 1
        time.sleep(1)
        try:
            sock.sendto(doc_string,(UDP_IP,UDP_PORT))
        except:
            # give up
            udp_errors_after_retry += 1
            pass
# ignore exception    
    return

#### MAIN ####################################################################


output_res = {}

#output_res[ 'message_1' ] = os.system('module list') 
#output_res[ 'message_2' ] = os.system('which mpirun') 


mpirun_p = Popen(['which','mpirun'], stdout=PIPE)
mpirun_name = mpirun_p.stdout.read().strip()


LD_PATH = os.environ['LD_LIBRARY_PATH']
PATH = os.environ['PATH']

#print "PATH:   ", PATH, "   LD_PATH:   ", LD_PATH, "   mpirun:   ", mpirun_name   
#exit()

json_variables = " "


InitialDirectoryStr = os.path.abspath(os.path.dirname(sys.argv[0]))
#InputDir = InitialDirectoryStr + '/willitfit_source/Source'

argv_io_string = StringIO(sys.argv[1])
json_variables = json.load(argv_io_string)

#####################################################
FNULL = open(os.devnull, 'w')

#print json.dumps( json_variables )
#exit()

script_location = json_variables['script_location']
ScriptStr = script_location[0]
Scriptfilename = os.path.basename(ScriptStr)
PathArrScript = re.split(json_variables['_logon'], ScriptStr)
ScriptLink = '..' + PathArrScript[-1]
subprocess.call(["ln", "-sf", ScriptLink, Scriptfilename], stdout=FNULL, stderr=subprocess.STDOUT)


select_uploader = json_variables['select_uploader']


if select_uploader == 'manually':
    coor_location = json_variables['coor_location']
    CoorStr = coor_location[0]
    Coorfilename = os.path.basename(CoorStr)
    PathArrCoor = re.split(json_variables['_logon'], CoorStr)
    CoorLink = '..' + PathArrCoor[-1]
    subprocess.call(["ln", "-sf", CoorLink, Coorfilename], stdout=FNULL, stderr=subprocess.STDOUT)
    #exit()

    psf_location = json_variables['psf_location']
    PsfStr = psf_location[0]
    Psffilename = os.path.basename(PsfStr)
    PathArrPsf = re.split(json_variables['_logon'], PsfStr)
    PsfLink = '..' + PathArrPsf[-1]
    subprocess.call(["ln", "-sf", PsfLink, Psffilename], stdout=FNULL, stderr=subprocess.STDOUT)
    
    #print "File: " + Psffilename
    #exit()
    
    toppar_location = json_variables['toppar_location']
    TopparStr = toppar_location[0]
    Topparfilename = os.path.basename(TopparStr)
    PathArrToppar = re.split(json_variables['_logon'], TopparStr)
    TopparLink = '..' + PathArrToppar[-1]
    subprocess.call(["ln", "-sf", TopparLink, Topparfilename], stdout=FNULL, stderr=subprocess.STDOUT)

    ## TOPPAR DIR ##################################################################################
    subprocess.call(["unzip", "-o", Topparfilename], stdout=FNULL, stderr=subprocess.STDOUT)
    
    if int(json_variables['additional_files']) > 0:
        for i in range(int(json_variables['additional_files'])):
            AuxStr = json_variables['aux_file'][i][0] 
            Auxfilename = os.path.basename(AuxStr)
            PathArrAux = re.split(json_variables['_logon'], AuxStr)
            AuxLink = '..' + PathArrAux[-1]
            subprocess.call(["ln", "-sf", AuxLink, Auxfilename], stdout=FNULL, stderr=subprocess.STDOUT)
            
            #print "Aux Files: " + json_variables['aux_file'][i][0] 
            #exit()


if select_uploader == 'archive':
    entirearchive_location = json_variables['entire_archive']
    EntireArchiveStr = entirearchive_location[0]
    EntireArchivefilename = os.path.basename(EntireArchiveStr)
    PathArrEntireArchive = re.split(json_variables['_logon'], EntireArchiveStr)
    EntireArchiveLink = '..' + PathArrEntireArchive[-1]
    subprocess.call(["ln", "-sf", EntireArchiveLink, EntireArchivefilename], stdout=FNULL, stderr=subprocess.STDOUT)

    ## Unzip #################################################################################
    subprocess.call(["unzip", "-o", EntireArchivefilename], stdout=FNULL, stderr=subprocess.STDOUT)
    #print "Archive selected!!"
    #exit()
    
numproc = json_variables['numproc']


## Clean Up ######################################################
#for CleanUp in glob.glob('./*.*'):                              # IF cleaning - do not forget to also exclude hostfile !!!   
#    print CleanUp
#    #exit()
#    if not CleanUp.endswith(Coorfilename):
#        if not CleanUp.endswith(Psffilename):
#            if not CleanUp.endswith(Scriptfilename):
#                if not CleanUp.endswith(Topparfilename):
#                    if not CleanUp.endswith(Velfilename):
#                        if not CleanUp.endswith(Boxfilename):
#                            os.remove(CleanUp)

## UDP messaging ##################################################
UDP_IP = json_variables['_udphost']
UDP_PORT = json_variables['_udpport']
sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP

socket_dict={}
socket_dict['_uuid'] = json_variables['_uuid']



################################################################################################
## PARSE INPUT FILE #########################################################

base_dir = json_variables[ '_base_directory' ]

dcd_path = ''
vel_path = ''
coor_path = ''
xsc_path = ''
xst_path = ''
vel_path_restart = ''
coor_path_restart = ''
xsc_path_restart = ''

########### NAMD Inputs (minimisation for now)  ################################################

namd_version = json_variables[ 'namdversion' ]

CompilerList = []

CompilerList.append(['211o202gcc540','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-5.4.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['211o202gcc540O2','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-5.4.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['211o202gcc540O3','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-5.4.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['211o202gcc630','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-6.3.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['211o202gcc630O2','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-6.3.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['211o202gcc630O3','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-6.3.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['211o202icc',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-icc/Linux-x86_64-icc/namd2'])
CompilerList.append(['211o202iccxhost',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-icc-xhost/Linux-x86_64-icc/namd2'])
CompilerList.append(['211o202iccipo',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-icc-ipo/Linux-x86_64-icc/namd2'])
CompilerList.append(['211o202iccxhostipo',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-icc-xhost-ipo/Linux-x86_64-icc/namd2'])
CompilerList.append(['211o202iccO2',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-icc/Linux-x86_64-icc/namd2'])
CompilerList.append(['211o202iccO3',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.11/NAMD_2.11_Source_openpmi_2.0.2_gcc-icc/Linux-x86_64-icc/namd2'])


CompilerList.append(['212o202gcc540','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-5.4.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['212o202gcc540O2','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-5.4.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['212o202gcc540O3','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-5.4.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['212o202gcc630','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-6.3.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['212o202gcc630O2','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-6.3.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['212o202gcc630O3','/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-6.3.0/Linux-x86_64-g++/namd2'])
CompilerList.append(['212o202icc',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-icc/Linux-x86_64-icc/namd2'])    
CompilerList.append(['212o202iccxhost',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-icc-xhost/Linux-x86_64-icc/namd2'])
CompilerList.append(['212o202iccipo',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-icc-ipo/Linux-x86_64-icc/namd2'])
CompilerList.append(['212o202iccxhostipo',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-icc-xhost-ipo/Linux-x86_64-icc/namd2'])
CompilerList.append(['212o202iccO2',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-icc/Linux-x86_64-icc/namd2'])    
CompilerList.append(['212o202iccO3',   '/opt/genapp/Install/NAMD_JetstreamIU_0315_2017/2.12/NAMD_2.12_Source_openpmi_2.0.2_gcc-icc/Linux-x86_64-icc/namd2'])    

for versions in CompilerList:
    if namd_version == versions[0]:
        ExecutableFileLocationNamd = versions[1]

#if namd_version == 'twooneone':
#    ExecutableFileLocationNamd = '/opt/genapp/Install/NAMD_source_openmpi_2.0.1_gcc-5.4.0/NAMD_CVS-2016-09-15_Source/Linux-x86_64-g++/namd2'
#    Mpirun = 'mpirun'
#    Hosts = '-hostfile'

#if namd_version == 'twoonetwo':
#    ExecutableFileLocationNamd = '/opt/genapp/Install/NAMD2017_2.12/NAMD_source_openmpi_2.0.1_gcc-5.4.0/Linux-x86_64-g++/namd2'
#    Mpirun = 'mpirun'
#    Hosts = '-hostfile'

#if namd_version == 'icc':
#    ExecutableFileLocationNamd = '/opt/genapp/namdrunner/bin/namd_bin/NAMD_source_icc/Linux-x86_64-g++/namd2'
#    Mpirun = '/opt/intel/compilers_and_libraries/linux/mpi/intel64/bin/mpirun'
#    Hosts = '-hosts'



#ExecutableFileLocationNamd = InitialDirectoryStr + '/namd_bin/NAMD_2.11_Linux-x86_64/namd2'
#ExecutableFileLocationNamd = '/opt/genapp/Install/NAMD_source_openmpi_2.0.1_gcc-5.4.0/NAMD_CVS-2016-09-15_Source/Linux-x86_64-g++/namd2'
#ExecutableFileLocationNamd = '/opt/genapp/Install/NAMD2017_2.12/NAMD_source_openmpi_2.0.1_gcc-5.4.0/Linux-x86_64-g++/namd2'

#ips = json_variables['_clusteripsjoined']

hostfile = json_variables['_clusterhostfile']

ProcessToCallNamdMd = []
#ProcessToCallNamdMd.append('mpirun')
ProcessToCallNamdMd.append(mpirun_name)
ProcessToCallNamdMd.append('-hostfile')
ProcessToCallNamdMd.append(hostfile)

ProcessToCallNamdMd.append('-x')
ProcessToCallNamdMd.append('LD_LIBRARY_PATH=%s' % LD_PATH)
ProcessToCallNamdMd.append('-x')
ProcessToCallNamdMd.append('PATH=%s' % PATH)

ProcessToCallNamdMd.append(ExecutableFileLocationNamd)
ProcessToCallNamdMd.append(Scriptfilename)

#print ProcessToCallNamdMd
#exit()

## RUN NAMD MD  ##########################################################
ProgressText = 'Simulation Progress: '
ProgressText_supl = ''
steps_cumul = 0
numsteps_md = ''

socket_dict["_textarea"] = 'Starting MD Simulation...\n\n'
socket_dict["progress_output"] =  str(0.0)
socket_dict["progress_text"] = ProgressText + '0%'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    try:
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))
    except:
        # try again after 1 second
        udp_errors += 1
        time.sleep(1)
        try:
            sock.sendto(doc_string,(UDP_IP,UDP_PORT))
        except:
            # give up
            udp_errors_after_retry += 1
            pass

master_namdmd, slave_namdmd = pty.openpty()
ProcessNamdmd = subprocess.Popen(ProcessToCallNamdMd,stdout=slave_namdmd,stdin=PIPE,bufsize=0,close_fds=True)
    
stdout_namdmd = os.fdopen(master_namdmd, 'r', 0)
namdmd_log = open('namdmd_log.out','w')
path_to_live_log = str(base_dir) + "/" + 'namdmd_log.out'

html_base  = json_variables['_osroot']
#application= json_variables['_application']

path_updated = path_to_live_log.replace(html_base + "/", "")
#path_updated_1 = application + path_updated
path_updated_1 =  path_updated

#print "Webroot: " + html_base + ", application: " + application +  ", pathupdated: " + path_updated + ", pathupdated_1: " + path_updated_1
#exit()

socket_dict['live_log'] =  "<a href=" + path_updated_1 + " target='_blank'>" + "Output Log File" + "</a>" 

#socket_dict['live_log'] = path_updated_1


error_string_md = ''
timeout = .04 # seconds


while True:
    ready, _, _ = select.select([master_namdmd], [], [], timeout)
    if ready:
        output = stdout_namdmd.readline()
        if not output:
            break
        if output:
            socket_dict["_textarea"] = output#.strip()
            print >> namdmd_log, output.rstrip()
            namdmd_log.flush()
            
            if "Info: DCD FILENAME" in output:
                dcdArr = re.split(r'[\s]*', output)
                dcd_path = str(base_dir) + "/" + dcdArr[3]
            
            if "Info: XST FILENAME" in output:
                xstArr = re.split(r'[\s]*', output)
                xst_path = str(base_dir) + "/" + xstArr[3]
            
            if "Info: OUTPUT FILENAME" in output:
                outArr = re.split(r'[\s]*', output)
                vel_path = str(base_dir) + "/" + outArr[3] + ".vel"
                coor_path = str(base_dir) + "/" + outArr[3] + ".coor"
                xsc_path =  str(base_dir) + "/" + outArr[3] + ".xsc"
            
            if "Info: RESTART FILENAME" in output:
                restartArr = re.split(r'[\s]*', output)
                vel_path_restart = str(base_dir) + "/" + restartArr[3] + ".vel"
                coor_path_restart = str(base_dir) + "/" + restartArr[3] + ".coor"
                xsc_path_restart =  str(base_dir) + "/" + restartArr[3] + ".xsc"

            if "Info: NUMBER OF STEPS" in output:
                stepsArr = re.split(r'[\s]*', output)
                numsteps_md = stepsArr[4]
                steps_cumul += float(numsteps_md)
            
            if (all(x in output for x in ['for','steps','Minimizing','TCL'])):
                stepsminArr = re.split(r'[\s]*', output)
                numsteps_md = stepsminArr[3]
                steps_cumul += float(numsteps_md)
                ProgressText_supl = 'Minimization '
                          
            if (all(x in output for x in ['for','steps','Running','TCL'])):
                stepsmdArr = re.split(r'[\s]*', output)
                numsteps_md = stepsmdArr[3]
                steps_cumul += float(numsteps_md)
                ProgressText_supl = 'MD run '
          
            if "ENERGY:" in output:
                OutArr = re.split(r'[\s]*', output)
                if OutArr[0] == 'ENERGY:':
                    socket_dict['progress_output'] = ( float(OutArr[1]) - (float(steps_cumul) - float(numsteps_md)) )/float(numsteps_md)
                    socket_dict['progress_text'] = ProgressText + ProgressText_supl + str(int ( ((float(OutArr[1]) - (float(steps_cumul) - float(numsteps_md)) )/float(numsteps_md))*100.0) ) + '%'
                    #socket_dict['progress_output'] = float(OutArr[1])/float(numsteps_md)
                    #socket_dict['progress_text'] = ProgressText + ProgressText_supl + str(int ( ((float(OutArr[1]))/float(numsteps_md))*100.0) ) + '%'
                        
                               
            if "ERROR:" in output:
                socket_dict["_textarea"] = output#.strip()
                error_string_md += "<p>" + output + "</p>"
                
            if socket_dict:
                doc_string = json.dumps(socket_dict)
                try:
                    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                except:
                    # try again after 1 second
                    udp_errors += 1
                    time.sleep(1)
                    try:
                        sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                    except:
                        # give up
                        udp_errors_after_retry += 1
                        pass
                #time.sleep(0.002)
    elif ProcessNamdmd.poll() is not None:  
        break
              
ProcessNamdmd.wait()
os.close(slave_namdmd)
os.close(master_namdmd)
                            
socket_dict["_textarea"] = '\nMD Simulation Completed...\n\n'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    try:
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))
    except:
        # try again after 1 second
        udp_errors += 1
        time.sleep(1)
        try:
            sock.sendto(doc_string,(UDP_IP,UDP_PORT))
        except:
            # give up
            udp_errors_after_retry += 1
            pass

output_res[ 'progress_output' ] = str(1.0)
output_res[ 'progress_text' ] = ProgressText + '100%' 

####################################################

outputmd_arr_files = []

ResultsDirectory = './' + 'SimulationResults/'

try:
    shutil.rmtree(ResultsDirectory)
except:
    pass

try:
    #ResultsDirectory = './' + Cardfilename + '-results/'
    os.mkdir(ResultsDirectory)
except:
    pass


if error_string_md:
    warning_str = "<p> Error:  </p> MD simulation terminated with the following error(s): <p> </p>" + error_string_md + "<p> Exiting... </p>"
    message_box(warning_str,'warning.png')
    output_res[ 'error' ] = 'MD simulation crashed. See the log file...'
        
    #coor_path_temp = os.path.basename(coor_path)
    #coor_path = str(base_dir) + "/" + os.path.splitext(coor_path_temp)[0] + '_restart.coor'
    coor_path = coor_path_restart

    #vel_path_temp = os.path.basename(vel_path)
    #vel_path = str(base_dir) + "/" + os.path.splitext(vel_path_temp)[0] + '_restart.vel'
    vel_path = vel_path_restart
    
    #xsc_path_temp = os.path.basename(xsc_path)
    #xsc_path = str(base_dir) + "/" + os.path.splitext(xsc_path_temp)[0] + '_restart.xsc'
    xsc_path = xsc_path_restart

    #xst_path_temp = os.path.basename(xst_path)
    #xst_path = str(base_dir) + "/" + os.path.splitext(xst_path_temp)[0] + '_restart.xst'
    
####################################################

if os.path.exists(dcd_path):
    outputmd_arr_files.append(dcd_path)
if os.path.exists(vel_path):
    outputmd_arr_files.append(vel_path)
    try:
        shutil.copy(vel_path, ResultsDirectory)
    except:
        pass
if os.path.exists(coor_path):
    outputmd_arr_files.append(coor_path)
    try:
        shutil.copy(coor_path, ResultsDirectory)
    except:
        pass
if os.path.exists(xsc_path):
    outputmd_arr_files.append(xsc_path)
    try:
        shutil.copy(xsc_path, ResultsDirectory)
    except:
        pass
if os.path.exists(xst_path):
    outputmd_arr_files.append(xst_path)
    try:
        shutil.copy(xst_path, ResultsDirectory)
    except:
        pass    

namdlog = str(base_dir) + "/" + 'namdmd_log.out'

if os.path.exists(namdlog):
    outputmd_arr_files.append(namdlog)
    try:
        shutil.copy(namdlog, ResultsDirectory)
    except:
        pass    

output_res[ 'outputmd' ] = outputmd_arr_files

## Making archive of results ##############################

archive_name = 'archived_results.zip'
if len(outputmd_arr_files):
    subprocess.call(["zip","-FS", "-r", archive_name, ResultsDirectory])

archive_name_path = str(base_dir) + "/" + archive_name

if os.path.exists(archive_name_path):
    output_res[ 'archivedres' ] = archive_name_path 

#####################################################
if json_variables[ '_logon' ] == 'emre':
    output_res[ 'udp_errors' ] = udp_errors
    output_res[ 'udp_errors_after_retry' ] = udp_errors_after_retry
print json.dumps( output_res )
