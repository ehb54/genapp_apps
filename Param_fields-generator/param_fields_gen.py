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

###################################################################
InputDir = '/home/alexey/WillItFit/Source'
ModelList = []

ModelList.append(['AnisotropicCoreShellMicelles','anis_core_shell_micelles'])
ModelList.append(['EllipticCylinder','ellip_cyl'])
ModelList.append(['Nanodisc','nano'])
ModelList.append(['NanodiscWithEllipticCylinderAsMembraneProtein','nano_ellip_cyl'])
ModelList.append(['NanodiscWithCholesterol','nano_chol'])
ModelList.append(['NanodiscWithEllipsoidalEndcaps','nano_bilayer'])
ModelList.append(['NanodiscWithTagsAndEllipsoidalEndcaps','nano_tags_bilayer'])
ModelList.append(['NanodiscWithTags','nano_tags'])

NumberOfModels = len(ModelList)

Labels = ['Min: ','Value: ','Max: ','Fit?: ','Name: ']

for ModelPath in ModelList:
    for File in os.listdir(os.path.join(InputDir, 'Models', ModelPath[0])):
        if os.path.splitext(File)[1] == '.par':
            ParameterFileName = File
            
    PathToParameterFile = os.path.join(os.path.join(InputDir, 'Models', ModelPath[0], ParameterFileName))

    file_par = open(PathToParameterFile, 'rU')
    ParametersArr = file_par.readlines()
    file_par.close()
    
    #print '{\n "role":"input", \n "id":"labels", \n "type":"label", \n "label":"Min: Value: Max:  Fit?: Name:  ", \n "repeat":"model_list_box:' + ModelPath[1]  + '"\n}, \n'

### INDIVIDUAL (ALL) FIELDS ##################################
    i = 0
    IdList = []
    for line in ParametersArr:
        LineArr = re.split(r'[\s]*', line)
        del LineArr[-1]
        Fields = len(LineArr)
        i = i + 1
        for fieldscurr in range(Fields):
            id_str = ModelPath[0] + Labels[fieldscurr] + LineArr[-1]
            
            if len(IdList) > 1:
                j = 0
                for idcurr in range(len(IdList)):
                    if IdList[idcurr] == id_str:
                        j = j + 1
                        id_str  = ModelPath[0] + Labels[fieldscurr] + LineArr[-1] + '_' + str(j)
                        
            IdList.append(id_str)
            new_id_str = id_str.replace(" ", "")
            new_id_str1 = new_id_str.replace(":", "")
            new_id_str2 = new_id_str1.replace("?", "")
            
            if (Labels[fieldscurr] == 'Fit?: '):
                type = 'checkbox'
                print '{\n "role":"input", \n "id":"' + new_id_str2 + '", \n "label":"' + Labels[fieldscurr] + '", \n "type":"' + type + '", \n "checked":"false", \n "repeat":"model_list_box:' + ModelPath[1]  + '", \n "norow":"true"\n},\n' 
            else: 
                type = 'text'
                NoRow = 'true'
                Size = '3'
                Label_add = ''
                
                if Labels[fieldscurr] == 'Name: ':
                    NoRow = 'false'
                    Size = '17'
                    #type = 'label'
                    type = 'text'
                    Label_add = str(i) + ': '
                
                print '{\n "role":"input", \n "id":"' + new_id_str2 + '", \n "label":"' + Labels[fieldscurr] + '", \n "type":"' + type + '", \n "default":"'+ Label_add + LineArr[fieldscurr] + '", \n "size":"' + Size + '", \n "repeat":"model_list_box:' + ModelPath[1]  + '", \n "norow":"' + NoRow + '"\n},\n' 
