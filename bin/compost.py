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

#contrast_location = json_variables['contrast_file']
#ContrastStr = contrast_location[0]
#Contrastfilename = os.path.basename(ContrastStr)

## Clean Up ######################################################
#for CleanUp in glob.glob('./*.*'):
#    print CleanUp
#    #exit()
#    #if not CleanUp.endswith(Contrastfilename):    
#    os.remove(CleanUp)

## UDP messaging ##################################################
UDP_IP = json_variables['_udphost']
UDP_PORT = json_variables['_udpport']
sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP

socket_dict={}
socket_dict['_uuid'] = json_variables['_uuid']
###################################################################

ExecutableFileLocation = InitialDirectoryStr + '/exe/Compost'

f_out = open("compost_output.html", "w")
#f_in = open(ContrastStr)

ProcessToCall = []
ProcessToCall.append(ExecutableFileLocation)

### Creating Input #####################################################

InputFile = open("Input_compost.txt", "w")

#Title
title_string = json_variables['title']
print >> InputFile, title_string
## Contrast points
num_contrast_points = json_variables['contrast_points_num'] 
print >> InputFile, num_contrast_points

qr_limit = json_variables['upper_qr_limit'] 
print >> InputFile, qr_limit

refine = ""
for i in range(int(num_contrast_points)):
    if json_variables['contrast_points_num_refine_scale'][i] == "on":
        refine = "R"
    else:
        refine = "F"
    
    currstring = refine + '\t' + json_variables['contrast_points_num_scale_value'][i] + '\t' + json_variables['contrast_points_num_start_point'][i] + '\t' + json_variables['contrast_points_num_contrast_data'][i][0] + '\t' + json_variables[ 'contrast_points_num_delta_rho_1'][i] + '\t' +  json_variables[ 'contrast_points_num_delta_rho_2' ][i] + '\t' + json_variables[ 'contrast_points_num_vol' ][i]
    print >>  InputFile, currstring

InputFile.close()
    
## RUN CHARMM ##########################################################
#socket_dict["_textarea"] = 'Starting Contrast Calculations...\n\n'

#if socket_dict:
#    doc_string = json.dumps(socket_dict)
#    sock.sendto(doc_string,(UDP_IP,UDP_PORT))

#Process = subprocess.Popen(ProcessToCall, stdin=f_in, stdout=f_out)

Inp_to_proc = open("Input_compost.txt")
Process = subprocess.Popen(ProcessToCall, stdin=Inp_to_proc, stdout=f_out)

Process.wait()
f_out.close()
#f_in.close()

## END Contrast Calculations #########################################
html_string = ""

forhtml = open('compost_output.html')
htmllines = forhtml.readlines()
forhtml.close()

NumLines = len(htmllines)  

for li in range(NumLines):
    html_string += htmllines[li] 

outfile = str(base_dir) + "/" + "compost_output.html"
output_res[ 'outputfile' ] = str(outfile)
output_res[ 'output_html' ] = str(html_string)
 
print json.dumps( output_res )
