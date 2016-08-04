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
import codecs

from itertools import groupby


from StringIO import StringIO
#from math import exp, expm1, log10, log, log1p
from subprocess import Popen, PIPE, STDOUT

if (len(sys.argv) < 1):
    print "\{\"error\":\"module called with no arguments\"\}\n";

##############################################################################
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

argv_io_string = StringIO(sys.argv[1])
json_variables = json.load(argv_io_string)

base_dir = json_variables[ '_base_directory' ]

#####################################################

contrast_location = json_variables['contrast_file']
ContrastStr = contrast_location[0]
Contrastfilename = os.path.basename(ContrastStr)

## Clean Up ######################################################
for CleanUp in glob.glob('./*.*'):
    print CleanUp
    #exit()
    if not CleanUp.endswith(Contrastfilename):    
        os.remove(CleanUp)

## UDP messaging ##################################################
UDP_IP = json_variables['_udphost']
UDP_PORT = json_variables['_udpport']
sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP

socket_dict={}
socket_dict['_uuid'] = json_variables['_uuid']
###################################################################

ExecutableFileLocation = InitialDirectoryStr + '/exe/Contrast'

f_out = open("contrast_output.html", "w")
f_in = open(ContrastStr)

ProcessToCall = []
ProcessToCall.append(ExecutableFileLocation)

#print ProcessToCall
#exit()

## RUN CHARMM ##########################################################
#socket_dict["_textarea"] = 'Starting Contrast Calculations...\n\n'

#if socket_dict:
#    doc_string = json.dumps(socket_dict)
#    sock.sendto(doc_string,(UDP_IP,UDP_PORT))

Process = subprocess.Popen(ProcessToCall, stdin=f_in, stdout=f_out)

Process.wait()
f_out.close()
f_in.close()

#master, slave = pty.openpty()
#Process = subprocess.Popen(ProcessToCall,stdout=slave,stdin=PIPE,bufsize=0,close_fds=True)
#
#stdout = os.fdopen(master, 'r', 0)
#timeout = .04 # seconds


#while True:
#    #output = Process.stdout.readline()
#    #signal.signal(signal.SIGTERM, functools.partial(handler,Process.pid))
#
#    ready, _, _ = select.select([master], [], [], timeout)
#    if ready:
#        output = stdout.readline()
#        if not output:
#            break
#        if output:
#            socket_dict["_textarea"] = output#.strip()
#            if socket_dict:
#                doc_string = json.dumps(socket_dict)
#                sock.sendto(doc_string,(UDP_IP,UDP_PORT))
#                time.sleep(0.005)
#    elif Process.poll() is not None:  
#        break
#
##Process.wait()
#os.close(slave)
#os.close(master)

#socket_dict["_textarea"] = '\nContrast Calculations Finished...\n\n'
#if socket_dict:
#    doc_string = json.dumps(socket_dict)
#    sock.sendto(doc_string,(UDP_IP,UDP_PORT))

## END Contrast Calculations #########################################
html_string = "'"

forhtml = open('contrast_output.html')
htmllines = forhtml.readlines()
forhtml.close()

NumLines = len(htmllines)  

for li in range(NumLines):
    html_string += htmllines[li] 

outfile = str(base_dir) + "/" + "contrast_output.html"
output_res[ 'outputfile' ] = str(outfile)
output_res[ 'output_html' ] = str(html_string)
 
print json.dumps( output_res )
