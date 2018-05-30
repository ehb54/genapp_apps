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

from bokeh.plotting import figure
from bokeh.embed import components

from itertools import groupby


from StringIO import StringIO
#from math import exp, expm1, log10, log, log1p
from subprocess import Popen, PIPE, STDOUT

if (len(sys.argv) < 1):
    print "\{\"error\":\"module called with no arguments\"\}\n";

##############################################################################
def FileNameArr():
    ExpArr       = []
    FitArr_upper = []
    FitArr_lower = []
    
    for root, dirs, files in os.walk('./'):
        for name in files:
            filename = os.path.join(root, name)
            
            if '2d_fit_upper' in filename:
                FitArr_upper.append(filename)

            if '2d_fit_lower' in filename:
                FitArr_lower.append(filename)
                
            if '2d_exp_' in filename:
                ExpArr.append(filename)

    return ExpArr, FitArr_upper, FitArr_lower

################################################################################
def GetData(filename):
    file = open(filename, 'rU')
    data = file.readlines()
    file.close()
        
    xData = []
    yData = []
    eData = []
    
    for line in data:
        xData.append(re.split(r'[\s]*', line)[0])
        yData.append(re.split(r'[\s]*', line)[1])
        if '2d_exp_' in filename:
            eData.append(re.split(r'[\s]*', line)[2]) 

    return xData, yData, eData
    

##################################################################################
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

relax_location = json_variables['relax_location']
RelaxStr = relax_location[0]
Relaxfilename = os.path.basename(RelaxStr)


pdb_location   = json_variables['pdb_location']
PdbStr = pdb_location[0]
Pdbfilename = os.path.basename(PdbStr)

## UDP messaging ##################################################
UDP_IP = json_variables['_udphost']
UDP_PORT = json_variables['_udpport']
sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP

socket_dict={}
socket_dict['_uuid'] = json_variables['_uuid']


base_dir = json_variables[ '_base_directory' ]

ExecutableFileLocationRotdif = '/opt/genapp/rotdif/bin/rotdif-1.1.jar'
######################################################################
ProcessToCallRotdif = []
ProcessToCallRotdif.append('/usr/bin/java')
ProcessToCallRotdif.append('-jar')
#ProcessToCallRotdif.append('rotdif-1.1.jar')
ProcessToCallRotdif.append(ExecutableFileLocationRotdif)

ProcessToCallRotdif.append('-nogui')
#ProcessToCallRotdif.append('-nodynamics')

ProcessToCallRotdif.append('-pdb')
ProcessToCallRotdif.append(Pdbfilename)

ProcessToCallRotdif.append('-relax')
ProcessToCallRotdif.append(Relaxfilename)

if 'stat' not in json_variables:
    ProcessToCallRotdif.append('-nostat')

#print ProcessToCallRotdif
#exit()
## RUN ROTDIF MD  ##########################################################

socket_dict["_textarea"] = 'Starting ROTDIF...\n\n'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))

master_rotdif, slave_rotdif = pty.openpty()
ProcessRotdif = subprocess.Popen( ProcessToCallRotdif,stdout=slave_rotdif,stdin=PIPE,bufsize=0,close_fds=True)
    
stdout_rotdif = os.fdopen(master_rotdif, 'r', 0)
rotdif_log = open('rotdif_log.out','w')
path_to_live_log = str(base_dir) + "/" + 'rotdif_log.out'

#html_base  = json_variables['_osroot']
##application= json_variables['_application']
#path_updated = path_to_live_log.replace(html_base + "/", "")
##path_updated_1 = application + path_updated
#path_updated_1 =  path_updated
##print "Webroot: " + html_base + ", application: " + application +  ", pathupdated: " + path_updated + ", pathupdated_1: " + path_updated_1
##exit()
#socket_dict['live_log'] =  "<a href=" + path_updated_1 + " target='_blank'>" + "Output Log File" + "</a>" 
##socket_dict['live_log'] = path_updated_1


error_string_md = ''
timeout = .04 # seconds


while True:
    ready, _, _ = select.select([master_rotdif], [], [], timeout)
    if ready:
        output = stdout_rotdif.readline()
        #output = stdout_rotdif.read(10)
        if not output:
            break
        if output:
            socket_dict["_textarea"] = output#.strip()
            print >> rotdif_log, output.rstrip()
            rotdif_log.flush()
            
            if "Percent" in output:
                output_strip = output.strip()
                OutArr = re.split(r'[\s]*', output_strip)
                percent = OutArr[1][:-1]
                socket_dict['progress_output'] = float(percent)/float(100)
                socket_dict['progress_text'] = 'ROTDIF calculation progress: ' + str(int (float( percent ))) + '%'
                
            if socket_dict:
                doc_string = json.dumps(socket_dict)
                sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                #time.sleep(0.002)
    elif ProcessRotdif.poll() is not None:  
        break
              
ProcessRotdif.wait()
os.close(slave_rotdif)
os.close(master_rotdif)
                            
socket_dict["_textarea"] = '\nROTDIF Calculations Completed...\n\n'

if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                            
                            
output_res[ 'progress_output' ] = str(1.0)
output_res[ 'progress_text' ] =  'ROTDIF calculation progress: ' + '100%' 
### PLOT ##############################################################

output_plot_2d_fit_upper = []
output_plot_2d_fit_lower = []
output_plot_2d_exp       = []

min_data_x = []
min_data_y = []
max_data_x = []
max_data_y = []

num_exp       = len(FileNameArr()[0])
num_fit_upper = len(FileNameArr()[1])
num_fit_lower = len(FileNameArr()[2])

# resizing
for i in range(num_exp):
    output_plot_2d_exp.append([])
for i in range(num_fit_upper):
    output_plot_2d_fit_upper.append([])
for i in range(num_fit_lower):
    output_plot_2d_fit_lower.append([])


#Exp - providing numbers of Exp and Fit sets ARE the same ...
for i in range(num_exp):
    print "Filename: " + FileNameArr()[0][i]

    xExpArr = GetData(FileNameArr()[0][i])[0]
    yExpArr = GetData(FileNameArr()[0][i])[1]
    eExpArr = GetData(FileNameArr()[0][i])[2]   

    xFupArr = GetData(FileNameArr()[1][i])[0]
    yFupArr = GetData(FileNameArr()[1][i])[1]
    eFupArr = GetData(FileNameArr()[1][i])[2]   

    xFloArr = GetData(FileNameArr()[2][i])[0]
    yFloArr = GetData(FileNameArr()[2][i])[1]
    eFloArr = GetData(FileNameArr()[2][i])[2] 

    #xFupArrsorted = xFupArr.sort()
    #yFupArrsorted = yFupArr.sort()
       
    min_data_x.append( min(ii for ii in xFupArr ) )
    min_data_y.append( min(ii for ii in yExpArr ) )
    max_data_x.append( max(ii for ii in xFupArr ) )
    max_data_y.append( max(ii for ii in yExpArr ) )
        
    numpoints_exp = len(xExpArr)
    numpoints_fup = len(xFupArr)
    numpoints_flo = len(xFloArr)
            
    for j in range(numpoints_exp):
        output_plot_2d_exp[i].append([xExpArr[j],yExpArr[j],eExpArr[j]])     
    for j in range(numpoints_fup):
        output_plot_2d_fit_upper[i].append([xFupArr[j],yFupArr[j]])     
    for j in range(numpoints_fup):
        output_plot_2d_fit_lower[i].append([xFloArr[j],yFloArr[j]])     
               

## Plotting with FLOT #####################################################

min_x = min(min_data_x)
min_y = min(min_data_y)
max_x = max(max_data_x)
max_y = max(max_data_y)

min_y1 = float(min_y) - 0.2
max_y1 = float(max_y) + 0.2

#output_res[ 'error' ] = min_x + " " + max_x + " " + str(min_y1) + " " + str(max_y1) 
#print "max_y: ", max_data_y

xscale = ''
yscale = ''

flot_data = {
    "options": 
    {
        "title"  : "Experimental &#961;",
        #"xscale" : "log",
        #"yscale" : "log",
        #"xmin"   : min_x,
        #"xmax"   : max_x,
        "ymin"   : min_y1,
        "ymax"   : max_y1,
        "xlabel" : "&#952;",
        "ylabel" : "&#961;(Exp.)",
        "legend" :
        {            
            "position"            : "ne",
            "margin"              : [-70, -1],
            "backgroundColor"     : "null",
            "labelBoxBorderColor" : "#000000",
         },
        "grid" : 
        {
            "backgroundColor" : "#ffffff",
            "margin"    : 
                              { 
                                  "top"    : 0, 
                                  "left"   : 0,
                                  "bottom" : 0,
                                  "right"  : 70
                              }
        },
    },
    "data": [ ]
}

for i in range(num_exp):
    data_label = "Data" + str(i+1)
    current_set_data = {
        "points"  : { "show"      : "true", 
                      "radius"    : 2,
                      "errorbars" : "y", 
                      "yerr"      : { "lowerCap" : "-",
                                      "upperCap" : "-", 
                                      "show"     : "true", 
                                      #"color"    : "red",
                                      "radius"   : 3 
                                  }
                  },
        "label"  : data_label,
        "data"   : output_plot_2d_exp[i]
    }
    flot_data['data'].append(current_set_data)


for i in range(num_exp):
    fit_label = "Fit" + str(i+1)
    current_set_fit = {
        "lines"  : { "show" : "true" },
        #"label"  : fit_label,
        "data"   : output_plot_2d_fit_upper[i]
    }
    flot_data['data'].append(current_set_fit)

for i in range(num_exp):
    fit_label = "Fit" + str(i+1)
    current_set_fit = {
        "lines"  : { "show" : "true" },
        #"label"  : fit_label,
        "data"   : output_plot_2d_fit_lower[i]
    }
    flot_data['data'].append(current_set_fit)


##########################################################################

outputmd_arr_files = []
namdlog = str(base_dir) + "/" + 'rotdif_log.out'
#namdlog = str(base_dir) + "/" + 'output'

if os.path.exists(namdlog):
    outputmd_arr_files.append(namdlog)


output_res[ 'outputrotdif' ] = outputmd_arr_files

#output_res[ 'fit_plot' ] = flot_data

## BOKEH PLOT ########################################
plot = figure()
plot.circle([0,1,2,3,4],[3,6,7,9,14])
#plot.circle(xFupArr, yFupArr)

script, div = components(plot)

output_res[ 'bokehid' ] = script
#####################################################
print json.dumps( output_res )
