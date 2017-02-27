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
json_variables = " "

InitialDirectoryStr = os.path.abspath(os.path.dirname(sys.argv[0]))
#InputDir = InitialDirectoryStr + '/willitfit_source/Source'

argv_io_string = StringIO(sys.argv[1])
json_variables = json.load(argv_io_string)


## UDP messaging ##################################################
UDP_IP = json_variables['_udphost']
UDP_PORT = json_variables['_udpport']
sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP

socket_dict={}
socket_dict['_uuid'] = json_variables['_uuid']
###################################################################

#####################################################
FNULL = open(os.devnull, 'w')

#print json.dumps( json_variables )
#exit()

file_location = json_variables['file_to_process']
FileStr = file_location[0]
Filefilename = os.path.basename(FileStr)
PathArrFile = re.split(json_variables['_logon'], FileStr)
FileLink = '..' + PathArrFile[-1]
subprocess.call(["ln", "-sf", FileLink, Filefilename], stdout=FNULL, stderr=subprocess.STDOUT)

output_filename = json_variables['outputname']

ExecutableFileLocation = '/opt/genapp/Install/catdcd/LINUXAMD64/bin/catdcd4.0/catdcd'

ProcessToCall = []
#ProcessToCall.append('mpirun')
#ProcessToCall.append('-hostfile')
#ProcessToCall.append(hostfile)
ProcessToCall.append(ExecutableFileLocation)
ProcessToCall.append('-o')
ProcessToCall.append(output_filename)


if 'systemmanipulation' in json_variables:
    indexfile = json_variables['indexfile']
    IndexStr = indexfile[0]

    IndexStrname = os.path.basename(IndexStr)
    PathArrIndex = re.split(json_variables['_logon'], IndexStr)
    IndexLink = '..' + PathArrIndex[-1]
    subprocess.call(["ln", "-sf", IndexLink, IndexStrname], stdout=FNULL, stderr=subprocess.STDOUT)

        
    ProcessToCall.append('-i')
    ProcessToCall.append(IndexStrname)

if 'framesmanipulation' in json_variables:
    first_frame = json_variables['firstframe']
    last_frame = json_variables['lastframe']
    stride = json_variables['stride']
    
    ProcessToCall.append('-first')
    ProcessToCall.append(first_frame)
    ProcessToCall.append('-last')
    ProcessToCall.append(last_frame)
    ProcessToCall.append('-stride')
    ProcessToCall.append(stride) 

ProcessToCall.append(Filefilename) 

if 'framesmanipulation' not in json_variables and 'systemmanipulation' not in json_variables:
    #if 'systemmanipulation' not in json_variables:
    error_str = "<p> ERROR: No modification was selected! </p> <p> Frames manipulation or System maniputlation must be selected. </p>"
    _message = {}
    _message['icon'] = 'warning.png'
    _message['text'] = error_str
    output_res['_message'] = _message
    print json.dumps( output_res )
    #message_box(error_str,'warning.png')
    exit()

#print ProcessToCall
#exit()
####################################################
#Process = subprocess.Popen(ProcessToCall)
#Process.wait()

ProgressText = 'Progress: '
ProgressText_supl = ''
steps_cumul = 0
numsteps_md = ''

socket_dict["_textarea"] = 'Starting Transformation...\n\n'
socket_dict["progress_output"] =  str(0.0)
#socket_dict["progress_text"] =  'Fitting Progress: 0%'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))


master, slave = pty.openpty()
Process = subprocess.Popen(ProcessToCall,stdout=slave,stdin=PIPE,bufsize=0,close_fds=True)

stdout = os.fdopen(master, 'r', 0)
timeout = .04 # seconds
namdmd_log = open('catdcd_log.out','w')

while True:
    #output = Process.stdout.readline()
    #signal.signal(signal.SIGTERM, functools.partial(handler,Process.pid))

    ready, _, _ = select.select([master], [], [], timeout)
    if ready:
        output = stdout.readline()
        if not output:
            break
        if output:
            socket_dict["_textarea"] = output#.strip()
            print >> namdmd_log, output.rstrip()
            namdmd_log.flush()
            
            if socket_dict:
                doc_string = json.dumps(socket_dict)
                sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                #time.sleep(0.005)
    elif Process.poll() is not None:  
        break

              
Process.wait()
os.close(slave)
os.close(master)

socket_dict["_textarea"] = '\n Transformation Finished...\n\n'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))

####################################################

output_path = json_variables['_base_directory'] + "/" + output_filename
output_res[ 'outputtrajectory' ] = output_path 

output_res[ 'logout' ] = json_variables['_base_directory'] + "/" + 'catdcd_log.out'

print json.dumps( output_res )
