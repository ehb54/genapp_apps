#!/usr/bin/python

import os
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


from StringIO import StringIO
#from math import exp, expm1, log10, log, log1p
from subprocess import Popen, PIPE, STDOUT

if (len(sys.argv) < 1):
    print "\{\"error\":\"module called with no arguments\"\}\n";


#### MAIN ####################################################################

output_res = {}

#output_res[ 'progress_output' ] = 0 

json_variables = " "


InitialDirectoryStr = os.path.abspath(os.path.dirname(sys.argv[0]))
#InputDir = InitialDirectoryStr + '/willitfit_source/Source'

argv_io_string = StringIO(sys.argv[1])
json_variables = json.load(argv_io_string)

#####################################################

pdb_location = json_variables['pdb_location']
PdbStr = pdb_location[0]
Pdbfilename = os.path.basename(PdbStr)

## UDP messaging ##################################################
UDP_IP = json_variables['_udphost']
UDP_PORT = json_variables['_udpport']
sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP

socket_dict={}
socket_dict['_uuid'] = json_variables['_uuid']
###################################################################
try:
    shutil.rmtree('./toppar')
except:
    pass


TopparLocation = InitialDirectoryStr + '/script_toppar/toppar'
TopparLocationStr = InitialDirectoryStr + '/script_toppar/toppar.str'

shutil.copytree(TopparLocation, os.path.join(json_variables['_base_directory'],'toppar'))
shutil.copy(TopparLocationStr, json_variables['_base_directory'])


##################################################################
ScriptLocation = InitialDirectoryStr + '/script_toppar/write_pdb_crd_psf.inp'
ExecutableFileLocation = InitialDirectoryStr + '/c36a6_svn132/exec/gnu/charmm'

ProcessToCall = []
ProcessToCall.append(ExecutableFileLocation)
ProcessToCall.append('-i')
ProcessToCall.append(ScriptLocation)

#print ProcessToCall
#exit()

## RUN ##########################################################
socket_dict["_textarea"] = 'Starting Calculations...\n\n'
#socket_dict["progress_output"] =  str(0.0)
#socket_dict["progress_text"] =  'Fitting Progress: 0%'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))


master, slave = pty.openpty()
Process = subprocess.Popen(ProcessToCall,stdout=slave,stdin=PIPE,bufsize=0,close_fds=True)

stdout = os.fdopen(master, 'r', 0)
timeout = .04 # seconds


while True:
    #output = Process.stdout.readline()
    #signal.signal(signal.SIGTERM, functools.partial(handler,Process.pid))

    ready, _, _ = select.select([master], [], [], timeout)
    if ready:
        output = stdout.readline()
        if not output:
            break
        if output:
            if "Progress" not in output: 
                socket_dict["_textarea"] = output#.strip()
            else:
                OutArr = re.split(r'[\s]*', output)
                del OutArr[-1]
                #socket_dict['progress_output'] = OutArr[-1]
                #socket_dict['progress_text'] = 'Fitting Progress: ' + str(int(float(OutArr[-1])*100.0)) + '%'
            if socket_dict:
                doc_string = json.dumps(socket_dict)
                sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                #time.sleep(0.01)
    elif Process.poll() is not None:  
        break

Process.wait()
os.close(slave)
os.close(master)




socket_dict["_textarea"] = '\n Parameterization Finished...\n\n'
#socket_dict["progress_output"] =  str(0.0)
#socket_dict["progress_text"] =  'Fitting Progress: 0%'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))

########### NAMD Inputs (minimisation for now)  ################################################
if 'mm_md_options' in json_variables:
    ScriptLocation_namdmin  = InitialDirectoryStr + '/namd_templates/namd_template_whole_min.in'
    shutil.copy(ScriptLocation_namdmin, json_variables['_base_directory'])

    ExecutableFileLocationNamd = InitialDirectoryStr + '/namd_bin/NAMD_2.11_Linux-x86_64/namd2'

    ProcessToCallNamdMin = []
    ProcessToCallNamdMin.append(ExecutableFileLocationNamd)
    #ProcessToCallNamdMin.append('<')
    ProcessToCallNamdMin.append('namd_template_whole_min.in')

    print ProcessToCallNamdMin
    #exit()

    ## RUN ##########################################################
    socket_dict["_textarea"] = 'Starting Minimization...\n\n'
    #socket_dict["progress_output"] =  str(0.0)
    #socket_dict["progress_text"] =  'Fitting Progress: 0%'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))
        
        
    master_namdmin, slave_namdmin = pty.openpty()
    ProcessNamdmin = subprocess.Popen(ProcessToCallNamdMin,stdout=slave_namdmin,stdin=PIPE,bufsize=0,close_fds=True)
        
    stdout_namdmin = os.fdopen(master_namdmin, 'r', 0)
        
    while True:
        #output = Process.stdout.readline()
        #signal.signal(signal.SIGTERM, functools.partial(handler,Process.pid))
        
        ready, _, _ = select.select([master_namdmin], [], [], timeout)
        if ready:
            output = stdout_namdmin.readline()
            if not output:
                break
            if output:
                if "Progress" not in output: 
                    socket_dict["_textarea"] = output#.strip()
                else:
                    OutArr = re.split(r'[\s]*', output)
                    del OutArr[-1]
                    #socket_dict['progress_output'] = OutArr[-1]
                    #socket_dict['progress_text'] = 'Fitting Progress: ' + str(int(float(OutArr[-1])*100.0)) + '%'
                if socket_dict:
                    doc_string = json.dumps(socket_dict)
                    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                    #time.sleep(0.01)
        elif ProcessNamdmin.poll() is not None:  
            break
                                
    os.close(slave_namdmin)
    os.close(master_namdmin)
                                
    socket_dict["_textarea"] = '\n Minimization Finished...\n\n'
    #socket_dict["progress_output"] =  str(0.0)
    #socket_dict["progress_text"] =  'Fitting Progress: 0%'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))


#################################################################
base_dir = json_variables[ '_base_directory' ]
output_basename = 'step1_pdbreader'

outpdb = str(base_dir) + "/" + output_basename + ".pdb"
outcrd = str(base_dir) + "/" + output_basename + ".crd"
outpsf = str(base_dir) + "/" + output_basename + ".psf"
outxplorpsf = str(base_dir) + "/" + output_basename + ".xplor.psf"

output_res[ 'outputpdb' ] = str(outpdb)
output_res[ 'outputcrd' ] = str(outcrd)
output_res[ 'outputpsf' ] = str(outpsf)
output_res[ 'outputxplorpsf' ] = str(outxplorpsf)

outputmin_arr_files = []
outputmin_arr_files.append(str(base_dir) + "/" + 'namd_dna_whole_min' + ".coor")
outputmin_arr_files.append(str(base_dir) + "/" + 'namd_dna_whole_min' + ".vel")

output_res[ 'outputmin' ] = outputmin_arr_files

output_res[ 'outputpdb_view' ] = str(outpdb)
#output_res[ 'logfile' ] = str()

print json.dumps( output_res )
