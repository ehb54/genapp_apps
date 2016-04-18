#!/usr/bin/python

import os
import re
import shutil
import sys
import subprocess
import json
import cStringIO

from StringIO import StringIO
from math import exp, expm1, log10, log, log1p

if (len(sys.argv) < 1):
    print "\{\"error\":\"module called with no arguments\"\}\n";
    #print 'exiting now'


def GetData(filename):
    file = open(filename, 'rU')
    data = file.readlines()
    file.close()
    
    DataStrArr = []

    for line in data:
        DataStrArr.append(line)

    FileExtensionStr = os.path.splitext(filename)[1]
    LineWithNumberOfDatapoint = DataStrArr[1]
    NumberOfDatapoints = int(re.split('=', LineWithNumberOfDatapoint)[1])

    xDataStr = []
    yDataStr = []
    wDataStr = []
    
    xDataFlt = []
    yDataFlt = []
    wDataFlt = []

    Contrast = float(re.split(r'=', DataStrArr[2])[1])
    
    for i in range(NumberOfDatapoints):
        xDataStr.append(0)
        yDataStr.append(0)
        wDataStr.append(0)

    for i in range(3, NumberOfDatapoints + 3):
        xDataStr[i - 3] = re.split(r'[\s]*', DataStrArr[i])[1]
        yDataStr[i - 3] = re.split(r'[\s]*', DataStrArr[i])[2]
        wDataStr[i - 3] = re.split(r'[\s]*', DataStrArr[i])[3]

    for item in xDataStr:
        xDataFlt.append(float(item))

    for item in yDataStr:
        yDataFlt.append(float(item))
        
    for item in wDataStr:
        wDataFlt.append(float(item))
    
    return xDataFlt, yDataFlt, wDataFlt, Contrast

##### END of GetData #############################################


json_variables = " "

argv_io_string = StringIO(sys.argv[1])
json_variables = json.load(argv_io_string)

data_location = json_variables['data_location']
sample_location = json_variables['sample_location']
data_set_location = json_variables['data_set_location']

qmin = json_variables['qmin']
qmax = json_variables['qmax']

param_file = json_variables['nano_tags_model_list_box']

try:
    shutil.rmtree('./.data/')
except:
    pass
os.mkdir('./.data/')

try:
    shutil.rmtree('./data/')
except:
    pass
os.mkdir('./data/')

for i in range(len(data_set_location)):
    shutil.move(data_set_location[i], "./data")


#############################################

DataStr = data_location[0]
#DataStr = 'Example.card'

SamplesStr = sample_location[0]
#SamplesStr = 'Example.dat'

ParametersStr = param_file[0]
#ParametersStr = 'ParametersBefore.mcp'

MinQStr = qmin
MaxQStr = qmax
FittingRoutine = 0
FittingRoutineArgument2 = 0
NumberOfSteps = 0
ResolutionFileName = 'N/A'
PrintCorrelationInt = 0
PDBStr = 'N/A'
ChiSquareFractile = 0
FittingRoutineArgument3 = 0 

Compilerflags = ['-O3', '-lm']

# Compile C-module and run
SourceFileLocation = os.path.join("/home/alexey/WillItFit/Source", "WillItFit.c")
ExecutableFileLocation = os.path.join("./WillItFit.com")

CompilerArguments = ["/usr/bin/gcc", SourceFileLocation, "-o", ExecutableFileLocation] + Compilerflags
Compilationresult = subprocess.call(CompilerArguments)


##############################################

#print CompilerArguments

ProcessToCall = []
ProcessToCall.append(ExecutableFileLocation)
ProcessToCall.append('-c=%s' % DataStr)
ProcessToCall.append('-s=%s' % SamplesStr)
ProcessToCall.append('-p=%s' % ParametersStr)
ProcessToCall.append('-n=%s' % MinQStr)
ProcessToCall.append('-x=%s' % MaxQStr)
ProcessToCall.append('-r=%d' % FittingRoutine)
ProcessToCall.append('-y=%d' % FittingRoutineArgument2)
ProcessToCall.append('-t=%d' % NumberOfSteps)
ProcessToCall.append('-e=%s' % ResolutionFileName)
ProcessToCall.append('-o=%d' % PrintCorrelationInt)
ProcessToCall.append('-d=%s' % PDBStr)
ProcessToCall.append('-h=%s' % ChiSquareFractile)
ProcessToCall.append('-a=%d' % FittingRoutineArgument3)


#print ProcessToCall

#args = "/home/alexey/WillItFit/For_genapp/a.out 4 10".split()
#print args

Process = subprocess.Popen(ProcessToCall)
Process.wait()


#fo = open("./Diff_q", "r")

#Results = []
#program_res = fo.readlines()
#for line in program_res:
#    Results.append(line)



##############################################

output_res = {}

base_dir = json_variables[ '_base_directory' ]
ResultsFileLocation = str(base_dir) + "/Results.wif"
DataFile1 = str(base_dir) + "/.data/dat1.dat"
DataFitFile1 = str(base_dir) + "/.data/fit1s.dat"

output_res[ 'outputfile_results' ] = str(ResultsFileLocation)
output_res[ 'outputfile_dat1' ] = str(DataFile1)
output_res[ 'outputfile_fit1' ] = str(DataFitFile1)
#output_res['output'] = 'NO'


#### PLOT  ############################################

DataArr = []
FitArr = [] 

for Filename in os.listdir('./.data/'):
    if os.path.splitext(Filename)[1] == '.dat':
        ShortName = os.path.basename(Filename)
        
        if ShortName[0:3] == 'fit':
            FitArr.append(os.path.join('.data/', ShortName))
            
        if ShortName[0:3] == 'dat':
            if ShortName[0:4] != 'data':
                DataArr.append(os.path.join('.data/', ShortName))
                    
NumberOfSets = len(DataArr)
                    
DataArr.sort()
FitArr.sort()

xData = []
yData = []
wData = []

xFit = []
yFit = []

ContrastData = []
ContrastFit = []

output_plot_fit = []
output_plot_data = []
min_data_x = []
min_data_y = []
max_data_x = []
max_data_y = []

for i in range(NumberOfSets):
    xData.append([])
    yData.append([])
    wData.append([])
    
    xFit.append([])
    yFit.append([])
    output_plot_fit.append([])
    output_plot_data.append([])
 

for i in range(NumberOfSets):
    xDataArr = GetData(DataArr[i])[0]
    yDataArr = GetData(DataArr[i])[1]
    wDataArr = GetData(DataArr[i])[2]
    
    #xDataArr_log = [log1p(ii) for ii in xDataArr]
    #yDataArr_log = [log1p(ii) for ii in yDataArr]

    #print yDataArr

    min_data_x.append( min(ii for ii in xDataArr ) )
    min_data_y.append( min(ii for ii in yDataArr if ii > 0 ) )
    max_data_x.append( max(ii for ii in xDataArr ) )
    max_data_y.append( max(ii for ii in yDataArr if ii > 0 ) )

    ContrastData.append(GetData(DataArr[i])[3])
    NumberOfPoints = len(xDataArr)
    
    for j in range(NumberOfPoints):
        if yDataArr[j] > 0:
            #xData[i].append(xDataArr[j])
            #yData[i].append(yDataArr[j])
            #wData[i].append(wDataArr[j]) 
            output_plot_data[i].append([xDataArr[j],yDataArr[j]])
        
            
    xFitArr = GetData(FitArr[i])[0]
    yFitArr = GetData(FitArr[i])[1]
            
    ContrastFit.append(GetData(DataArr[i])[3])
    NumberOfPoints = len(xFitArr)
            
    for j in range(NumberOfPoints):
        if yFitArr[j] > 0:
            #xFit[i].append(xFitArr[j])
            #yFit[i].append(yFitArr[j])
            output_plot_fit[i].append([xFitArr[j],yFitArr[j]])


############# print output_plot ###########################

min_x = min(min_data_x)
min_y = min(min_data_y)
max_x = max(max_data_x)
max_y = max(max_data_y)

#print "max_y: ", max_data_y

flot_data = {
    "options": 
    {
        "xscale" : "log",
        "yscale" : "log",
        "xmin"   : min_x,
        "xmax"   : max_x,
        "ymin"   : min_y,
        "ymax"   : max_y,
        "xlabel" : "q (1/A)",
        "ylabel" : "I(q)",
        "legend" : 
        {
            "backgroundOpacity"   : 0.2,
            "labelBoxBorderColor" : "black",
            "position"            : "ne",
            "margin"              : [-80, 0]
        },
        
    },
    "data": [ ]
}


for i in range(NumberOfSets):
    data_label = "Data" + str(i+1)
    current_set_data = {
        "points"  : { "show" : "true" },
        "label"  : data_label,
        "data"   : output_plot_data[i]
    }
    flot_data['data'].append(current_set_data)

for i in range(NumberOfSets):
    fit_label = "Fit" + str(i+1)
    current_set_fit = {
        "lines"  : { "show" : "true" },
        "label"  : fit_label,
        "data"   : output_plot_fit[i]
    }
    flot_data['data'].append(current_set_fit)

### END PLOT ################################################

output_res[ 'fit_plot' ] = flot_data

print json.dumps( output_res )

#os.remove(ExecutableFileLocation)
