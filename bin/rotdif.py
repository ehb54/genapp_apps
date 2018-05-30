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
from matplotlib import cm, colors

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
    LabelArr     = []
    Surface3d    = []
    Exp3d        = []
    
    for root, dirs, files in os.walk('./'):
        for name in files:
            filename = os.path.join(root, name)
            
            if '2d_fit_upper' in filename:
                FitArr_upper.append(filename)

            if '2d_fit_lower' in filename:
                FitArr_lower.append(filename)
                
            if '2d_exp_' in filename:
                ExpArr.append(filename)
                
            if '2d_label_' in filename:
                LabelArr.append(filename) 
                
            if '3d_surface_' in filename:
                Surface3d.append(filename)
            
            if '3d_exp_points_' in filename:
                Exp3d.append(filename)
    ExpArr.sort()
    FitArr_upper.sort()
    FitArr_lower.sort()
    LabelArr.sort()
    Surface3d.sort()
    Exp3d.sort()

    return ExpArr, FitArr_upper, FitArr_lower, LabelArr, Surface3d, Exp3d

################################################################################
def GetData(filename):
    file = open(filename, 'rU')
    data = file.readlines()
    file.close()
        
    xData = []
    yData = []
    eData = []
    tData = []
    
    for line in data:
        xData.append(re.split(r'[\s]*', line)[0])
        yData.append(re.split(r'[\s]*', line)[1])
        if '2d_exp_' in filename:
            eData.append(re.split(r'[\s]*', line)[2])
            
            tooltipArr = re.split(r'[\s]*', line)[-3:]
            tooltipStr = ""
            for ch in range(len(tooltipArr)):
                tooltipStr += tooltipArr[ch] + " "  
            tooltipStr.rstrip() 
            tData.append(tooltipStr)

    return xData, yData, eData, tData

##########################################################    
def GetData_3d(filename):
    file = open(filename, 'rU')
    data = file.readlines()
    file.close()
        
    xData = []
    yData = []
    zData = []
       
    for line in data:
        xData.append(re.split(r'[\s]*', line)[0])
        yData.append(re.split(r'[\s]*', line)[1])
        zData.append(re.split(r'[\s]*', line)[2])

    return xData, yData, zData

################################################################################
def GetLabels(filename):
    file = open(filename, 'rU')
    data = file.readlines()
    file.close()
        
    Labels = []
    
    for line in data:
        Labels.append(line)
       
    return Labels

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


## Clean Up ######################################################
for CleanUp in glob.glob('./2d_*'):                            
    #print CleanUp
    #exit()
    #if not CleanUp.endswith(Coorfilename):
    os.remove(CleanUp)

for CleanUp in glob.glob('./3d_*'):                            
    os.remove(CleanUp)


for CleanUp in glob.glob('./*.txt'): 
    print CleanUp, Relaxfilename
    #exit()
    if not CleanUp.endswith(Relaxfilename):
        os.remove(CleanUp)

for CleanUp in glob.glob('./*.out'): 
    os.remove(CleanUp)

for CleanUp in glob.glob('./*.pdb'):
    if not CleanUp.endswith(Pdbfilename):
        os.remove(CleanUp)
    
## UDP messaging ##################################################
UDP_IP = json_variables['_udphost']
UDP_PORT = json_variables['_udpport']
sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP

socket_dict={}
socket_dict['_uuid'] = json_variables['_uuid']


base_dir = json_variables[ '_base_directory' ]

optimization_method_list = json_variables[ 'optimization_method' ]

ExecutableFileLocationRotdif = '/opt/genapp/rotdif/bin/rotdif-1.1.jar'
######################################################################
ProcessToCallRotdif = []
ProcessToCallRotdif.append('/usr/bin/java')
ProcessToCallRotdif.append('-jar')
#ProcessToCallRotdif.append('rotdif-1.1.jar')
ProcessToCallRotdif.append(ExecutableFileLocationRotdif)

ProcessToCallRotdif.append('-nogui')

#if 'rundyn' not in json_variables:
#    ProcessToCallRotdif.append('-nodynamics')

#if 'runelmpred' in json_variables:
#    ProcessToCallRotdif.append('-elm')

#if 'runelmdoc' in json_variables:
#    ProcessToCallRotdif.append('-dock')

#ProcessToCallRotdif.append('-temp')
#ProcessToCallRotdif.append(json_variables['temperature'])

#ProcessToCallRotdif.append('-out')
#ProcessToCallRotdif.append(json_variables['outfile'])

#if 'axes' in json_variables:
#    ProcessToCallRotdif.append('-axes')

#ProcessToCallRotdif.append('-axesl')
#ProcessToCallRotdif.append(json_variables['axeslength'])


ProcessToCallRotdif.append('-pdb')
ProcessToCallRotdif.append(Pdbfilename)

if optimization_method_list == 'robust':
    ProcessToCallRotdif.append('-robust')

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
output_plot_2d_rexp      = []
labels                   = []

output_plot_3d_surface   = []
output_plot_3d_expp      = []

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
    output_plot_2d_rexp.append([])
    output_plot_3d_surface.append([])
    output_plot_3d_expp.append([])
for i in range(num_fit_upper):
    output_plot_2d_fit_upper.append([])
for i in range(num_fit_lower):
    output_plot_2d_fit_lower.append([])


#Exp - providing numbers of Exp, Fit and Labels sets ARE the same ...
for i in range(num_exp):
    print "Filename/Label/LabelArr: " + FileNameArr()[0][i] + " / " + FileNameArr()[3][i] 

    xExpArr = GetData(FileNameArr()[0][i])[0]
    yExpArr = GetData(FileNameArr()[0][i])[1]
    eExpArr = GetData(FileNameArr()[0][i])[2]   
    rExpArr = GetData(FileNameArr()[0][i])[3]   

    xFupArr = GetData(FileNameArr()[1][i])[0]
    yFupArr = GetData(FileNameArr()[1][i])[1]
    eFupArr = GetData(FileNameArr()[1][i])[2]   

    xFloArr = GetData(FileNameArr()[2][i])[0]
    yFloArr = GetData(FileNameArr()[2][i])[1]
    eFloArr = GetData(FileNameArr()[2][i])[2] 

    labels.append( GetLabels(FileNameArr()[3][i]) ) 

    xExpArr_3d = GetData_3d(FileNameArr()[4][i])[0]
    yExpArr_3d = GetData_3d(FileNameArr()[4][i])[1]
    zExpArr_3d = GetData_3d(FileNameArr()[4][i])[2]   

    xExpArr_3d_expp = GetData_3d(FileNameArr()[5][i])[0]
    yExpArr_3d_expp = GetData_3d(FileNameArr()[5][i])[1]
    zExpArr_3d_expp = GetData_3d(FileNameArr()[5][i])[2]   

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
        output_plot_2d_rexp[i].append([rExpArr[j]])  
    for j in range(numpoints_fup):
        output_plot_2d_fit_upper[i].append([xFupArr[j],yFupArr[j]])     
    for j in range(numpoints_fup):
        output_plot_2d_fit_lower[i].append([xFloArr[j],yFloArr[j]])  

    output_plot_3d_surface[i].append(xExpArr_3d)
    output_plot_3d_surface[i].append(yExpArr_3d)
    output_plot_3d_surface[i].append(zExpArr_3d)  

    output_plot_3d_expp[i].append(xExpArr_3d_expp)
    output_plot_3d_expp[i].append(yExpArr_3d_expp)
    output_plot_3d_expp[i].append(zExpArr_3d_expp)  
   

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
            "margin"              : [-140, -1],
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
                                  "right"  : 140
                              }
        },
    },
    "data": [ ]
}

## COLORS from Set1 ColorMap ###################################
colorArr = []
MaxNumColors = 100
colorIncr = int( round(MaxNumColors/num_exp, 0) - 1)
if colorIncr < 0:
    MaxNumColors = num_exp
    colorIncr = 1

cmap = cm.get_cmap('Set1', MaxNumColors)   

for i in range(cmap.N):
    rgb = cmap(i)[:3] # will return rgba, we take only first 3 so we get rgb
    colorArr.append(colors.rgb2hex(rgb))

################################################################

#colorArr = ['red', 'green', 'blue', 'orange', 'cyan']

## 3D with PLOTLY.JS ############################################################
data_3d = {
    "layout" : {
        "scene": {
            "xaxis" : { 
                "title"           : "Theta (Deg)",
                "backgroundcolor" : "rgb(200, 200, 230)",
                "gridcolor"       : "rgb(128, 128, 128)",
                "showbackground"  : "true",
                "zerolinecolor"   : "rgb(128, 128, 128)"
            },
            "yaxis" : { 
                "title" : "Phi (Deg)",
                "backgroundcolor" : "rgb(230, 200,230)",
                "gridcolor"       : "rgb(128, 128, 128)",
                "showbackground"  : "true",
                "zerolinecolor"   : "rgb(128, 128, 128)",
                "range"           : [-179, 180]
            },
            "zaxis" : { 
                "title" : "Rho",
                "backgroundcolor" : "rgb(230, 230,200)",
                "gridcolor"       : "rgb(128, 128, 128)",
                "showbackground"  : "true",
                "zerolinecolor"   : "rgb(128, 128, 128)"
            },
            "camera": {
                "center" : { "x": 0, "y": 0, "z": 0 }, 
                "eye"    : { "x": 2, "y": 2, "z": 1 }, 
                "up"     : { "x": 0, "y": 0, "z": 1 }
            }
        },
        "showlegend" : "false",
        "autosize" : "false",
	"width" : 550,
	"height": 550,
	"margin": {
	 "l": 0,
	 "r": 0,
	 "b": 0,
	 "t": 0,
	 "pad": 4
	},
    },
    "data"   : []
}

for i in range(num_exp):
   # print output_plot_3d_surface[i][0]
   # exit();
    current_set_surf = {
        "type"    :  "mesh3d",
        "opacity" :  0.6,
        "x"       :  output_plot_3d_surface[i][0],
        "y"       :  output_plot_3d_surface[i][1],
        "z"       :  output_plot_3d_surface[i][2],
        "color"   :  colorArr[ i*colorIncr ]
    }
    data_3d['data'].append(current_set_surf)

for i in range(num_exp):
   # print output_plot_3d_expp[i][0]
   # exit();
    current_set_3d_expp = {
        "type"    :  "scatter3d",
        "mode"    :  "markers",
        "marker"  : {
            "color"   : colorArr[ i*colorIncr ],
            "size"    : 2,
            "symbol"  : 'square',
            "line"    :  {
                "color"  : "rgb(0,0,0)",
		"width"  : 1.5 
            },
            "opacity" : 1.0
        },
        "opacity" :  1.0,
        "x"       :  output_plot_3d_expp[i][0],
        "y"       :  output_plot_3d_expp[i][1],
        "z"       :  output_plot_3d_expp[i][2],
        "color"   :  colorArr[ i*colorIncr ]
    }
    data_3d['data'].append(current_set_3d_expp)

### 2D ###########################################################
for i in range(num_exp):
    #data_label = "Data" + str(i+1)
    data_label = labels[i]
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
        "data"   : output_plot_2d_exp[i],
        "tooltips"  : output_plot_2d_rexp[i],
        "color"  : colorArr[ i*colorIncr ]
    }
    flot_data['data'].append(current_set_data)


for i in range(num_exp):
    fit_label = "Fit" + str(i+1)
    current_set_fit = {
        "lines"  : { "show" : "true"  },
        #"label"  : fit_label,
        "data"   : output_plot_2d_fit_upper[i],
        "color"  : colorArr[ i*colorIncr ]
    }
    flot_data['data'].append(current_set_fit)
  

for i in range(num_exp):
    fit_label = "Fit" + str(i+1)
    current_set_fit = {
        "lines"  : { "show" : "true" },
        #"label"  : fit_label,
        "data"   : output_plot_2d_fit_lower[i],
         "color"  : colorArr[ i*colorIncr ]
    }
    flot_data['data'].append(current_set_fit)



##########################################################################

outputmd_arr_files = []
namdlog = str(base_dir) + "/" + 'rotdif_log.out'
#namdlog = str(base_dir) + "/" + 'output'

if os.path.exists(namdlog):
    outputmd_arr_files.append(namdlog)


output_res[ 'outputrotdif' ] = outputmd_arr_files
output_res[ 'fit_plot' ] = flot_data
output_res[ 'plot_3d' ]  = data_3d
#####################################################
print json.dumps( output_res )
