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

json_variables = " "

InputDir = '/home/alexey/WillItFit/Source'

argv_io_string = StringIO(sys.argv[1])
json_variables = json.load(argv_io_string)

data_location = json_variables['data_location']
sample_location = json_variables['sample_location']
data_set_location = json_variables['data_set_location']

qmin = json_variables['qmin']
qmax = json_variables['qmax']
ChiSquareFractile = 0.0

algoritnm_list = json_variables['algorithm_list_box']


ClearOldData()
os.mkdir('./.data/')

try:
    shutil.rmtree('./data/')
except:
    pass
os.mkdir('./data/')

for i in range(len(data_set_location)):
    shutil.move(data_set_location[i], "./data")


###### LOGIC  #######################################

DataStr = data_location[0]
#DataStr = 'Example.card'

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
#FittingRoutine = 0
#FittingRoutineArgument2 = 0
#NumberOfSteps = 0
#ResolutionFileName = 'N/A'
#PrintCorrelationInt = 0
#PDBStr = 'N/A'
#ChiSquareFractile = 0
#FittingRoutineArgument3 = 0 

Cardfilename = os.path.basename(DataStr)
try:
    ResultsDirectory = './' + Cardfilename + '-results/'
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

if model_list == 'anis_core_shell_micelles':
    ModelPath = 'AnisotropicCoreShellMicelles'
    #param_file = json_variables['anis_core_shell_micelles_model_list_box']

elif model_list == 'ellip_cyl':
    ModelPath = 'EllipticCylinder'
    #param_file = json_variables['ellip_cyl_model_list_box']

elif model_list == 'nano':
    ModelPath = 'Nanodisc'
    #param_file = json_variables['nano_model_list_box']

elif model_list == 'nano_ellip_cyl':
    ModelPath = 'NanodiscWithEllipticCylinderAsMembraneProtein'
    #param_file = json_variables['nano_ellip_cyl_model_list_box']

elif model_list == 'nano_chol':
    ModelPath = 'NanodiscWithCholesterol'
    #param_file = json_variables['nano_chol_model_list_box']

elif model_list == 'nano_bilayer':
    ModelPath = 'NanodiscWithEllipsoidalEndcaps'
    #param_file = json_variables['nano_bilayer_model_list_box']

elif model_list == 'nano_tags':
    ModelPath = 'NanodiscWithTags'
    #param_file = json_variables['nano_bilayer_model_list_box']

else: #model_list == 'nano_tags_bilayer'
    ModelPath = 'NanodiscWithTagsAndEllipsoidalEndcaps'
    #param_file = json_variables['nano_tags_bilayer_model_list_box']


#ParametersStr = param_file[0]

HeaderFile = open(os.path.join(InputDir, 'Auxillary', 'ModelLocation.h'), "w+")
HeaderFile.write('#include "' + os.path.join(InputDir, 'Models', ModelPath, 'ModelInfo.h') + '"')
HeaderFile.close()

for File in os.listdir(os.path.join(InputDir, 'Models', ModelPath)):
    if os.path.splitext(File)[1] == '.par':
        ParameterFileName = File
        ParameterFileFound = True

PathToParameterFile = os.path.join(os.path.join(InputDir, 'Models', ModelPath, ParameterFileName))
#CreateParameters(PathToParameterFile)
#PrintParameters(PathToParameterFile)


file_par = open(PathToParameterFile, 'rU')
ParametersArr = file_par.readlines()
file_par.close()

ParameterMin = []
ParameterValues = []
ParameterMax = []
ParameterOnOff = []
ParameterNames = []


IdList = []
Labels = ['Min: ','Value: ','Max: ','Fit?: ','Name: ']

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
            #print json_variables[ new_id_str2 ]
            
            

# Build dummy parameter file
file_par_dummy = open('ParametersBefore.mcp','w')

i = 0
#OnOffToPrint = []
NoFreeParameters = True


for line in ParameterValues:
    #if self.Checkboxes[i].GetValue():
    #    OnOffToPrint.append('0')
    #    NoFreeParameters = False
    #else:
    #    OnOffToPrint.append('1')
    
    LineToWrite = ParameterMin[i] + '   ' + \
                  ParameterValues[i] + '   ' + \
                  ParameterMax[i] + '   ' + \
                  ParameterOnOff[i] + '   ' + \
                  ParameterNames[i] + '\n'
    
    file_par_dummy.write(LineToWrite)
    
    i = i + 1

file_par_dummy.close()


ParametersStr = 'ParametersBefore.mcp'

try:
    os.remove('ParametersAfter.mcp')
except:
    pass

############## Compile C-module and run ################
Compilerflags = ['-O3', '-lm']

SourceFileLocation = os.path.join(InputDir, "WillItFit.c")
ExecutableFileLocation = os.path.join("./WillItFit.com")

CompilerArguments = ["/usr/bin/gcc", SourceFileLocation, "-o", ExecutableFileLocation] + Compilerflags
Compilationresult = subprocess.call(CompilerArguments)

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

Process = subprocess.Popen(ProcessToCall)
Process.wait()

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


### OUTPUT JSON ###########################################

output_res = {}

outputfilesarr_data = []
outputfilesarr_fit = []

base_dir = json_variables[ '_base_directory' ]
ResultsFileLocation = str(base_dir) + "/Results.wif"
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
output_res[ 'fit_plot' ] = flot_data

print json.dumps( output_res )

#os.remove(ExecutableFileLocation)
ClearOldData()
