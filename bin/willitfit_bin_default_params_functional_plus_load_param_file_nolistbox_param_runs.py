#!/usr/bin/python

import os
import re
import glob
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
import string


from StringIO import StringIO
#from math import exp, expm1, log10, log, log1p
from subprocess import Popen, PIPE, STDOUT

if (len(sys.argv) < 1):
    print "\{\"error\":\"module called with no arguments\"\}\n";
    #print 'exiting now'
##################################################################
#def handler(pid, signum, frame):
#    #kill(pid, signum)
#    kill(pid, 9)

#### Get Data #####################################################
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

## Function for copying results to appropriate directories
def PrintResultsToCardfileDirectory():
    DataArr = []
    FitArr = []
    
    for root, dirs, files in os.walk('./'):
        for name in files:
            Filename = os.path.join(root, name)
            
            if os.path.splitext(Filename)[1] == '.dat':
                ShortName = os.path.basename(Filename)
                
                if ShortName[0:3] == 'fit':
                    FitArr.append(os.path.join('.data/', ShortName))
                    
                if ShortName[0:3] == 'dat':
                    DataArr.append(os.path.join('.data/', ShortName))
    
    for file in FitArr:
        try:
            shutil.copy(file, ResultsDirectory)
        except:
            pass
    
    for file in DataArr:
        try:
            shutil.copy(file, ResultsDirectory)
        except:
            pass
    
    try:
        shutil.copy(os.path.join('.data/', 'CovarianceMatrix.dat'), ResultsDirectory)
    except:
        pass
    
    try:
        shutil.copy('ParametersAfter.mcp', os.path.join(ResultsDirectory, 'LastFit.par'))
    except:
        pass

    try:
        shutil.copy('Results.wif', ResultsDirectory)
    except:
        pass

############# End of Copy Results ################################################
## Function for clearing directory of old data-files and parameter-files
def ClearOldData():
    try:
        shutil.rmtree('./.data/')
    except:
        pass



## End of Clear ####################
def GetChisquare():
    file = open(os.path.join('./.data', 'ReturnMessage.mcp'), 'rU')
    Chisquare = file.readlines()[0]
    file.close()
    
    return Chisquare

def GetReturnMessage():
    file = open(os.path.join('./.data', 'ReturnMessage.mcp'), 'rU')
    ReturnMessage = file.readline()
    file.close()
    
    return ReturnMessage

#############################################################################



#### MAIN ####################################################################

output_res = {}

#output_res[ 'progress_output' ] = 0 

json_variables = " "


InitialDirectoryStr = os.path.abspath(os.path.dirname(sys.argv[0]))
InputDir = InitialDirectoryStr + '/willitfit_source/Source'

argv_io_string = StringIO(sys.argv[1])
json_variables = json.load(argv_io_string)

#####################################################

data_location = json_variables['data_location']
sample_location = json_variables['sample_location']
data_set_location = json_variables['data_set_location']

qmin = json_variables['qmin']
qmax = json_variables['qmax']
ChiSquareFractile = 0.0

algoritnm_list = json_variables['algorithm_list_box']


###################################################################
#PathtoProject = json_variables['_base_directory']
#print PathtoProject
#exit()

## UDP messaging ##################################################
UDP_IP = json_variables['_udphost']
UDP_PORT = json_variables['_udpport']
sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP

socket_dict={}
socket_dict['_uuid'] = json_variables['_uuid']
###################################################################

ClearOldData()
os.mkdir('./.data/')

try:
    shutil.rmtree('./data/')
except:
    pass
os.mkdir('./data/')

for i in range(len(data_set_location)):
    #shutil.move(data_set_location[i], "./data")
    shutil.copy(data_set_location[i], "./data")

###### LOGIC  #######################################

DataStr_init = data_location[0]
#DataStr = 'Example.card'
datafile = open(DataStr_init, 'rU')
DataLines = datafile.readlines()
datafile.close()

CorrectedCorfile = 'Corr_' + os.path.basename(DataStr_init)
DataStr_corr = open(CorrectedCorfile,'w')
filename_base = ''
str_temp = ''

for li in range(len(DataLines)):
    if '.rad' in DataLines[li]:
        LineArr = re.split(r'[\s]*', DataLines[li])
        if '/' in LineArr[0]:
            LineArr_sp = re.split('/', LineArr[0])
            filename_base = LineArr_sp[-1]
        else:
            filename_base = DataLines[li].rstrip()
        print >> DataStr_corr, filename_base
        continue

    print >> DataStr_corr, DataLines[li].rstrip()

DataStr_corr.close()
DataStr = json_variables['_base_directory'] + '/' + CorrectedCorfile


SamplesStr = sample_location[0]
#SamplesStr = 'Example.dat'


if algoritnm_list == 'comp_model':
    FittingRoutine = 0
    FittingRoutineArgument2 = 0
    FittingRoutineArgument3 = 0
elif algoritnm_list == 'lm':
    FittingRoutine = 1
    FittingRoutineArgument2 = 0
    FittingRoutineArgument3 = 0
elif algoritnm_list == 'grid_lm':
    FittingRoutine = 2
    FittingRoutineArgument2 = int(json_variables['number_of_cycles_grid_lm'])
    FittingRoutineArgument3 = 0
elif algoritnm_list == 'swarm':
    FittingRoutine = 5
    FittingRoutineArgument2 = int(json_variables['number_of_flights_swarm'])
    FittingRoutineArgument3 = int(json_variables['number_of_agents_swarm'])
elif algoritnm_list == 'genetic':
    FittingRoutine = 6
    FittingRoutineArgument2 = int(json_variables['number_of_individuals_genetic'])
    FittingRoutineArgument3 = int(json_variables['number_of_generations_genetic'])
elif algoritnm_list == 'profile_likelihood':
    FittingRoutine = 7
    ChiSquareFractile = float(json_variables['fractile_prof_likelihood'])
    FittingRoutineArgument2 = 0
    FittingRoutineArgument3 = int(json_variables['number_of_cycles_prof_likelihood'])
else:
    FittingRoutine = 8
    ChiSquareFractile = float(json_variables['fractile_plh'])
    FittingRoutineArgument2 = int(json_variables['id_of_parameters_plh']) 
    FittingRoutineArgument3 = int(json_variables['number_of_cycles_plh'])

if 'include_res_effects' in json_variables:      
    NumberOfSteps = int(json_variables['number_of_steps'])
    resolution_location = json_variables['res_file_location']
    pdb_location = json_variables['pdb_file_location']
    ResolutionFileName = resolution_location[0]
    PDBStr = pdb_location[0]
else:
    NumberOfSteps = 0
    ResolutionFileName = 'N/A'
    PDBStr = 'N/A'

if 'print_covariance_matrix_lm' in json_variables:
    PrintCorrelationInt = 1
else:
    PrintCorrelationInt = 0


#############################################

MinQStr = qmin
MaxQStr = qmax

Cardfilename = os.path.basename(DataStr)
ResultsDirectory = './' + Cardfilename + '-results/'

try:
    shutil.rmtree(ResultsDirectory)
except:
    pass

try:
    #ResultsDirectory = './' + Cardfilename + '-results/'
    os.mkdir(ResultsDirectory)
except:
    pass


if FittingRoutine == 7 or FittingRoutine == 8:
    try:
        os.mkdir('ProfileLikelihood')
    except:
        pass


### Create header for specific model #########

#ModelFunctionInt = event.GetSelection()
model_list = json_variables['model_list_box']

ModelList = []

ModelList.append(['AnisotropicCoreShellMicelles','anis_core_shell_micelles'])
ModelList.append(['EllipticCylinder','ellip_cyl'])
ModelList.append(['MembraneProteinInNanodiscWithCurvedBilayer','membrane_prot_nano'])
ModelList.append(['MonodisperseCoreShellMicelle','mono_core_micelle'])
ModelList.append(['Spheres','mono_spheres'])
ModelList.append(['Nanodisc','nano'])
ModelList.append(['NanodiscWithEllipticCylinderAsMembraneProtein','nano_ellip_cyl'])
ModelList.append(['NanodiscWithCholesterol','nano_chol'])
ModelList.append(['NanodiscWithEllipsoidalEndcaps','nano_bilayer'])
ModelList.append(['NanodiscWithTags','nano_tags'])
ModelList.append(['NanodiscWithTagsAndEllipsoidalEndcaps','nano_tags_bilayer'])
ModelList.append(['PDB','pdb'])
ModelList.append(['PeptideDiscs','peptide_discs'])
ModelList.append(['PolydisperseCoreShellMicelles','poly_core_shell_micelles'])
ModelList.append(['PolydispersePeptideDiscs','poly_peptide_discs'])
ModelList.append(['PolydispersePeptideDiscsWithFreePeptides','poly_peptide_discs_free_peptides'])
ModelList.append(['PolydisperseSpheres','poly_spheres'])
ModelList.append(['PopulationOfAnisotropicNanodiscs','pop_aniso_nano'])
ModelList.append(['SingleLorentzian','single_lorentzian'])
ModelList.append(['SingleShellLiposome','single_liposome'])
ModelList.append(['TripleShellLiposome','triple_liposome'])
ModelList.append(['TripleShellLiposomeWMLandSQ','triple_liposome_landsq'])


for model in ModelList:
    if model_list == model[1]:
        ModelPath = model[0]

if model_list == 'choose_model':
    output_res['error'] = 'No model has been selected... Choose a model!'
    print json.dumps( output_res )
    exit()  

    
#HeaderFile = open(os.path.join(InputDir, 'Auxillary', 'ModelLocation.h'), "w+")

HeaderFile = open(os.path.join(json_variables['_base_directory'], 'ModelLocation.h'), "w+")
HeaderFile.write('#include "' + os.path.join(InputDir, 'Models', ModelPath, 'ModelInfo.h') + '"')
HeaderFile.close()


#### Parameters ##############################################################
#param_id = 'param_choice_file_' + model_list + '_checkbox'
#parameter_list = json_variables[ param_id]

#if parameter_list == 'choose_params':
#    output_res['error'] = 'No parameters have been selected... Choose parameters!'
#    print json.dumps( output_res )
#    exit()  


#if param_id not in json_variables:
for File in os.listdir(os.path.join(InputDir, 'Models', ModelPath)):
    if os.path.splitext(File)[1] == '.par':
        ParameterFileName = File
        ParameterFileFound = True

        
PathToParameterFile = os.path.join(os.path.join(InputDir, 'Models', ModelPath, ParameterFileName))


file_par = open(PathToParameterFile, 'rU')
ParametersArr = file_par.readlines()
file_par.close()

ParameterMin = []
ParameterValues = []
ParameterMax = []
ParameterOnOff = []
ParameterNames = []


IdList = []
Labels = ['Min: ','Value: ','Max: ','Fit?: ','Parameter: ']
NoFreeParameters = True

for line in ParametersArr:
    LineArr = re.split(r'[\s]*', line)
    del LineArr[-1]
    Fields = len(LineArr)

    for fieldscurr in range(Fields):
        id_str = ModelPath + Labels[fieldscurr] + LineArr[-1]
    
    
        if len(IdList) > 1:
            j = 0
            for idcurr in range(len(IdList)):
                if IdList[idcurr] == id_str:
                    j = j + 1
                    id_str  = ModelPath + Labels[fieldscurr] + LineArr[-1] + '_' + str(j)

        IdList.append(id_str)
        new_id_str = id_str.replace(" ", "")
        new_id_str1 = new_id_str.replace(":", "")
        new_id_str2 = new_id_str1.replace("?", "")
        
        if (Labels[fieldscurr] == 'Fit?: '):
            if new_id_str2 in json_variables:
                ParameterOnOff.append('0')
                NoFreeParameters = False
            else:
                ParameterOnOff.append('1')

        elif (Labels[fieldscurr] == 'Min: '):
            ParameterMin.append(json_variables[ new_id_str2 ])
                
        elif (Labels[fieldscurr] == 'Value: '):
            ParameterValues.append(json_variables[ new_id_str2 ])
                
        elif (Labels[fieldscurr] == 'Max: '):
            ParameterMax.append(json_variables[ new_id_str2 ])
                
        else: 
            ParameterNames.append(json_variables[ new_id_str2 ])
                    

file_par_dummy = open('ParametersBefore.mcp','w')

i = 0
for line in ParameterValues:
    LineToWrite = ParameterMin[i] + '   ' + \
                  ParameterValues[i] + '   ' + \
                  ParameterMax[i] + '   ' + \
                  ParameterOnOff[i] + '   ' + \
                  ParameterNames[i] + '\n'
    
    file_par_dummy.write(LineToWrite)
    
    i = i + 1
    
file_par_dummy.close()

if NoFreeParameters and (FittingRoutine != 0):
    output_res['error'] = 'This fitting routine needs free parameters...'
    print json.dumps( output_res )
    exit() 

ParametersStr = 'ParametersBefore.mcp'

#else: 
#    param_file = 'param_choice_file_' + model_list
#    ParametersStr = json_variables[ param_file ][0]
#    #print ParametersStr


try:
    os.remove('ParametersAfter.mcp')
except:
    pass

############## Compile C-module and run ################
#Compilerflags = ['-O3', '-lm']
#PathtoHeader = json_variables['_base_directory'] + '/ModelLocation.h'

Abspath0 =  os.path.join(InputDir, "Includes0.h")
Abspath2 =  os.path.join(InputDir, "Includes2.h")

shutil.copy(Abspath0, json_variables['_base_directory'])
shutil.copy(Abspath2, json_variables['_base_directory'])


# Process Inputs0.h
filedata0 = None
with open('Includes0.h', 'r') as file0 :
  filedata0 = file0.read()

filedata0 = filedata0.replace('PATH', InputDir)

with open('Includes0.h', 'w') as file0:
  file0.write(filedata0)

# Process Inputs2.h
filedata2 = None
with open('Includes2.h', 'r') as file2 :
  filedata2 = file2.read()

filedata2 = filedata2.replace('PATH', InputDir)

with open('Includes2.h', 'w') as file2:
  file2.write(filedata2)

######################################################

New_Abspath0 =  os.path.abspath(os.path.dirname('Includes0.h')) + '/Includes0.h'
Abspath1 =  os.path.abspath(os.path.dirname('ModelLocation.h')) + '/ModelLocation.h'
New_Abspath2 =  os.path.abspath(os.path.dirname('Includes2.h')) + '/Includes2.h'

Compilerflags = ['-O3', '-lm', '-include', New_Abspath0, '-include', Abspath1, '-include', New_Abspath2]

SourceFileLocation = os.path.join(InputDir, "WillItFit.c")
ExecutableFileLocation = os.path.join("./WillItFit.com")

CompilerPath = "/usr/bin/gcc"
CompilerArguments = [CompilerPath, SourceFileLocation, "-o", ExecutableFileLocation] + Compilerflags

Compilationresult = subprocess.call(CompilerArguments)

if Compilationresult != 0:
    output_res['error'] = "The compilation of WillItFit failed. \nThe system tried to use " + CompilerPath + " with the following arguments: \n" + Compilerflags[0] + " " + Compilerflags[1] + ". \nContact the developers."
    print json.dumps( output_res )
    exit()


#######################################################

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

socket_dict["_textarea"] = 'Starting Calculations...'
socket_dict["progress_output"] =  str(0.0)
socket_dict["progress_text"] =  'Fitting Progress: 0%'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
    #time.sleep(0.01)

### Regular PIPE -stdout - BUFFERED output form child #####################################################
#Process = subprocess.Popen(ProcessToCall,stdout=PIPE,bufsize=0)

#while True:
#    output = Process.stdout.readline()
#    if not output and Process.poll() is not None:
#        break
#    if output:
#        if "Progress" not in output: 
#            socket_dict["_textarea"] = output#.strip()
#        else:
#            OutArr = re.split(r'[\s]*', output)
#            del OutArr[-1]
#            socket_dict['progress_output'] = OutArr[-1]
#            socket_dict['progress_text'] = 'Fitting Progress: ' + str(int(float(OutArr[-1])*100.0)) + '%'
#        if socket_dict:
#            doc_string = json.dumps(socket_dict)
#            sock.sendto(doc_string,(UDP_IP,UDP_PORT))

#### PTY Version - pseudo-terminal, UNDUFFERED output#####################################################
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
                socket_dict['progress_output'] = OutArr[-1]
                socket_dict['progress_text'] = 'Fitting Progress: ' + str(int(float(OutArr[-1])*100.0)) + '%'
            if socket_dict:
                doc_string = json.dumps(socket_dict)
                sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                #time.sleep(0.01)
    elif Process.poll() is not None:  
        break

os.close(slave)
os.close(master)

#atexit.register(Process.terminate())
  
#Process.wait()
############################################################################################################




# Display concluding message and new parameters
try:
    #PrintParameters('ParametersAfter.mcp')
    PrintResultsToCardfileDirectory()
    Chisquare = GetChisquare()
except:
    ReturnMessage = GetReturnMessage()


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

    #print xDataArr

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
            output_plot_data[i].append([xDataArr[j],yDataArr[j],wDataArr[j]])
        
            
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

xscale = ''
yscale = ''

if json_variables['plot_axis_scheme'] == 'log_log':
    xscale = 'log'
    yscale = 'log'
if json_variables['plot_axis_scheme'] == 'log_lin':
    xscale = 'log'
    yscale = ''

flot_data = {
    "options": 
    {
        "title"  : "Data/Fit Plot",
        "xscale" : xscale,
        "yscale" : yscale,
        "xmin"   : min_x,
        "xmax"   : max_x,
        "ymin"   : min_y,
        "ymax"   : max_y,
        "xlabel" : "q/[1/&#8491;]",
        "ylabel" : "I(q)/[1/cm]",
        "legend" :
        {            
            "position"            : "ne",
            "margin"              : [5, 5],
            "backgroundColor"     : "#DCDCDC",
            "labelBoxBorderColor" : "#000000"
            #"labelFormatter"      : "legendFormatter"
            #"container"           : "true"
            #"backgroundOpacity"   : 0
                         
        },
        "grid" : 
        {
            "backgroundColor" : "#ffffff",

        },
    },
    "data": [ ]
}


for i in range(NumberOfSets):
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


## Making archive of results ##############################
archive_name = 'archived_results.zip'
subprocess.call(["zip","-FS", "-r", archive_name, ResultsDirectory])

### OUTPUT JSON ###########################################

outputfilesarr_data = []
outputfilesarr_fit = []

base_dir = json_variables[ '_base_directory' ]
ResultsFileLocation = str(base_dir) + '/' + ResultsDirectory + "Results.wif"
output_res[ 'outputfile_results' ] = str(ResultsFileLocation)

for i in range(NumberOfSets):
    DataFile = str(base_dir) + '/' + ResultsDirectory + "dat" + str(i+1) + ".dat"
    outputfilesarr_data.append(DataFile)
    
for i in range(NumberOfSets):
    FitFile = str(base_dir) + '/' + ResultsDirectory + "fit" + str(i+1) + "s.dat"
    outputfilesarr_fit.append(FitFile)
    

output_res[ 'outputfiles_data' ] = outputfilesarr_data
output_res[ 'outputfiles_fit' ] = outputfilesarr_fit

output_res[ 'outputfile_parameters' ] = str(base_dir) + '/' + ResultsDirectory + "LastFit.par"

if PrintCorrelationInt == 1:
    output_res[ 'outputfile_covariance' ] = str(base_dir) + '/' + ResultsDirectory + "CovarianceMatrix.dat"

output_res[ 'chi_sqr' ] = Chisquare

output_res[ 'res_arch' ] = str(base_dir) + '/' + archive_name
output_res[ 'fit_plot' ] = flot_data

output_res[ 'progress_output' ] = str(1.0)
output_res[ 'progress_text' ] = 'Fitting Progress: 100%' 

print json.dumps( output_res )

os.remove(ExecutableFileLocation)
ClearOldData()
