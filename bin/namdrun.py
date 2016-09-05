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

##############################################################################
def message_box(text,icon):
    
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
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
    
    return

#### MAIN ####################################################################

output_res = {}

#output_res[ 'progress_output' ] = 0 

json_variables = " "


InitialDirectoryStr = os.path.abspath(os.path.dirname(sys.argv[0]))
#InputDir = InitialDirectoryStr + '/willitfit_source/Source'

argv_io_string = StringIO(sys.argv[1])
json_variables = json.load(argv_io_string)

#####################################################

coor_location = json_variables['coor_location']
CoorStr = coor_location[0]
Coorfilename = os.path.basename(CoorStr)

psf_location = json_variables['psf_location']
PsfStr = psf_location[0]
Psffilename = os.path.basename(PsfStr)

toppar_location = json_variables['toppar_location']
TopparStr = toppar_location[0]
Topparfilename = os.path.basename(TopparStr)

script_location = json_variables['script_location']
ScriptStr = script_location[0]
Scriptfilename = os.path.basename(ScriptStr)


## Clean Up ######################################################
for CleanUp in glob.glob('./*.*'):
    print CleanUp
    #exit()
    if not CleanUp.endswith(Coorfilename):
        if not CleanUp.endswith(Psffilename):
            if not CleanUp.endswith(Scriptfilename):
                if not CleanUp.endswith(Topparfilename):
                    os.remove(CleanUp)

## UDP messaging ##################################################
UDP_IP = json_variables['_udphost']
UDP_PORT = json_variables['_udpport']
sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP

socket_dict={}
socket_dict['_uuid'] = json_variables['_uuid']


## TOPPAR DIR ##################################################################################

subprocess.call(["unzip", "-o", Topparfilename])

################################################################################################
## PARSE INPUT FILE #########################################################

base_dir = json_variables[ '_base_directory' ]

dcd_path = ''
vel_path = ''
coor_path = ''
xsc_path = ''
xst_path = ''

numsteps_md = ''

with open(Scriptfilename) as f:
    for line in f:
        #if "outputname" in line:
        if line.startswith('outputname'):
            LineArr = re.split(r'[\s]*', line)
            
            vel_path =  str(base_dir) + "/" + LineArr[1] + ".vel"
            coor_path = str(base_dir) + "/" + LineArr[1] + ".coor"
            xsc_path =  str(base_dir) + "/" + LineArr[1] + ".xsc"
            xst_path =  str(base_dir) + "/" + LineArr[1] + ".xst" 
            
        #if "DCDfile" in line:
        if line.startswith('DCDfile'):
            LineArr = re.split(r'[\s]*', line)
            dcd_path = str(base_dir) + "/" + LineArr[1]
        #if "numsteps" in line:
        if line.startswith('numsteps'):
            LineArr = re.split(r'[\s]*', line)
            numsteps_md = LineArr[1]

########### NAMD Inputs (minimisation for now)  ################################################
ExecutableFileLocationNamd = InitialDirectoryStr + '/namd_bin/NAMD_2.11_Linux-x86_64/namd2'

ProcessToCallNamdMd = []
ProcessToCallNamdMd.append(ExecutableFileLocationNamd)
ProcessToCallNamdMd.append(Scriptfilename)

## RUN NAMD MD  ##########################################################
socket_dict["_textarea"] = 'Starting MD Simulation...\n\n'
socket_dict["progress_output"] =  str(0.0)
socket_dict["progress_text"] =  'MD Simulation Progress: 0%'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
    
    
master_namdmd, slave_namdmd = pty.openpty()
ProcessNamdmd = subprocess.Popen(ProcessToCallNamdMd,stdout=slave_namdmd,stdin=PIPE,bufsize=0,close_fds=True)
    
stdout_namdmd = os.fdopen(master_namdmd, 'r', 0)
namdmd_log = open('namdmd_log.out','w')

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
            
            if "ENERGY:" in output:
                OutArr = re.split(r'[\s]*', output)
                if OutArr[0] == 'ENERGY:':
                    socket_dict['progress_output'] = float(OutArr[1])/float(numsteps_md)
                    socket_dict['progress_text'] = 'MD Simulation Progress: ' + str(int ((float(OutArr[1])/float(numsteps_md))*100.0) ) + '%'
                               
            if "ERROR:" in output:
                socket_dict["_textarea"] = output#.strip()
                error_string_md += "<p>" + output + "</p>"
                
            if socket_dict:
                doc_string = json.dumps(socket_dict)
                sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                #time.sleep(0.002)
    elif ProcessNamdmd.poll() is not None:  
        break
              
ProcessNamdmd.wait()
os.close(slave_namdmd)
os.close(master_namdmd)
                            
socket_dict["_textarea"] = '\nMD Simulation Completed...\n\n'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))

    
output_res[ 'progress_output' ] = str(1.0)
output_res[ 'progress_text' ] = 'MD Simulation Progress: 100%' 



#ProcessNamdmd = subprocess.Popen(ProcessToCallNamdMd)
#ProcessNamdmd.wait()
####################################################

outputmd_arr_files = []

if os.path.exists(dcd_path):
    outputmd_arr_files.append(dcd_path)
if os.path.exists(vel_path):
    outputmd_arr_files.append(vel_path)
if os.path.exists(coor_path):
    outputmd_arr_files.append(coor_path)
if os.path.exists(xsc_path):
    outputmd_arr_files.append(xsc_path)
if os.path.exists(xst_path):
    outputmd_arr_files.append(xst_path)

namdlog = str(base_dir) + "/" + 'namdmd_log.out'
outputmd_arr_files.append(namdlog)

output_res[ 'outputmd' ] = outputmd_arr_files

#####################################################
print json.dumps( output_res )
