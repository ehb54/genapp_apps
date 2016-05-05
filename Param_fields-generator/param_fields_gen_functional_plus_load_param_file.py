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


NumberOfModels = len(ModelList)

Labels = ['Min: ','Value: ','Max: ','Fit?: ','Name: ']

#out_str = ""
out_str_ids = ""

for ModelPath in ModelList:
    for File in os.listdir(os.path.join(InputDir, 'Models', ModelPath[0])):
        if os.path.splitext(File)[1] == '.par':
            ParameterFileName = File
            
    PathToParameterFile = os.path.join(os.path.join(InputDir, 'Models', ModelPath[0], ParameterFileName))

    file_par = open(PathToParameterFile, 'rU')
    ParametersArr = file_par.readlines()
    file_par.close()
    

### PER MODEL ################################################
    ##print '{\n "role":"input", \n "id":"param_choice_' + ModelPath[1] +'", \n "label":"Parameters: ", \n "type":"listbox", \n "values":"Choose Parameters...~choose_params~Load Model Paramters~default~Load Parameter File~file", \n "repeater":"yes", \n "repeat":"model_list_box:' + ModelPath[1] + '"\n},\n'

    ##print '{\n "role":"input", \n "id":"param_choice_file_' + ModelPath[1] +'", \n "label":"File: ", \n "type":"file", \n "repeat":"param_choice_' + ModelPath[1]  + ':file"\n},\n'
    ##print '{\n "role":"input", \n "id":"param_choice_file_' + ModelPath[1] +'", \n "label":"File: ", \n "type":"file", \n "setinputfromfile":"whitespaceseparated", \n "setinputfromfileids":"models_ids",  \n "repeat":"model_list_box:' + ModelPath[1]  + '"\n},\n'

    
    Param_ids = ""
    out_str = ""
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
        
            Param_ids = Param_ids  + new_id_str2 + ","
            
            if (Labels[fieldscurr] == 'Fit?: '):
                type = 'checkbox'
                
                ##print '{\n "role":"input", \n "id":"' + new_id_str2 + '", \n "label":"' + Labels[fieldscurr] + '", \n "type":"' + type + '", \n "checked":"false", \n "repeat":"param_choice_' + ModelPath[1]  + ':default", \n "norow":"true"\n},\n' 
                #print '{\n "role":"input", \n "id":"' + new_id_str2 + '", \n "label":"' + Labels[fieldscurr] + '", \n "type":"' + type + '", \n "checked":"false", \n "repeat":"model_list_box:' + ModelPath[1]  + '", \n "norow":"true"\n},\n'
                out_str = out_str + '{\n "role":"input", \n "id":"' + new_id_str2 + '", \n "label":"' + Labels[fieldscurr] + '", \n "type":"' + type + '", \n "checked":"false", \n "repeat":"model_list_box:' + ModelPath[1]  + '", \n "norow":"true"\n},\n'
            else: 
                type = 'float'
                NoRow = 'true'
                Size = '3'
                Label_add = ''
                readonly = 'false'
                
                if Labels[fieldscurr] == 'Name: ':
                    type = 'text'
                    NoRow = 'false'
                    Size = '17'
                    #type = 'label'
                    type = 'text'
                    Label_add = str(i) + ':'
                    readonly = 'true'
                                
                ##print '{\n "role":"input", \n "id":"' + new_id_str2 + '", \n "label":"' + Labels[fieldscurr] + '", \n "type":"' + type + '", \n "default":"'+ Label_add + LineArr[fieldscurr] + '", \n "size":"' + Size + '", \n "repeat":"param_choice_' + ModelPath[1]  + ':default", \n "norow":"' + NoRow + '", \n "readonly":"' + readonly + '"\n},\n' 
                #print '{\n "role":"input", \n "id":"' + new_id_str2 + '", \n "label":"' + Labels[fieldscurr] + '", \n "type":"' + type + '", \n "default":"'+ Label_add + LineArr[fieldscurr] + '", \n "size":"' + Size + '", \n "repeat":"model_list_box:' + ModelPath[1]  + '", \n "norow":"' + NoRow + '", \n "readonly":"' + readonly + '"\n},\n' 
                out_str = out_str + '{\n "role":"input", \n "id":"' + new_id_str2 + '", \n "label":"' + Labels[fieldscurr] + '", \n "type":"' + type + '", \n "default":"'+ Label_add + LineArr[fieldscurr] + '", \n "size":"' + Size + '", \n "repeat":"model_list_box:' + ModelPath[1]  + '", \n "norow":"' + NoRow + '", \n "readonly":"' + readonly + '"\n},\n'
#############################################################

    Param_ids = Param_ids[:-1]
    print '{\n "role":"input", \n "id":"param_choice_file_' + ModelPath[1] +'", \n "label":"File: ", \n "type":"file", \n "setinputfromfile":"whitespaceseparated", \n "setinputfromfileids":"' + Param_ids + '",  \n "repeat":"model_list_box:' + ModelPath[1]  + '"\n},\n'
    print out_str
    

   
