#!/usr/bin/python

import os
import re
import shutil
import sys
import subprocess
import json
import cStringIO
import string
from string import lowercase
from itertools import groupby

from StringIO import StringIO
from math import exp, expm1, log10, log, log1p


PathToParameterFile = sys.argv[1]

file_par = open(PathToParameterFile, 'rU')
ParametersArr = file_par.readlines()
file_par.close()

Num_of_lines = len(ParametersArr)    
ParametersArr_new = []

for li in range(Num_of_lines):
    #if li == 0 or li == Num_of_lines - 1:
    #    continue
    
    LineArr = re.split(r'[\s]*', ParametersArr[li])
    del LineArr[-1]
        
    if LineArr[0] != 'ATOM':
        continue
        
    if LineArr[2].startswith('H'):
        continue

    ParametersArr_new.append(ParametersArr[li])
#######################################################
        
Num_of_lines_new = len(ParametersArr_new)  

half_molecule = 0
LastLineArr = re.split(r'[\s]*', ParametersArr_new[-1])

found_chainId_Lett = re.match(r'[a-zA-Z]+', str(LastLineArr[4]))
found_resNum_Int5  = re.match(r'[0-9]+', str(LastLineArr[5]))
found_resnum_Int4  = re.match(r'[0-9]+', str(LastLineArr[4]))

atom_number = 0
Residue_reset = 0
Divider = []
f = open('temp.pdb','w+')  # temporary .pdb

for li in range(Num_of_lines_new):
    
    LineArr = re.split(r'[\s]*', ParametersArr_new[li])
    atom_number += 1
    
    Fields = len(LineArr)
    Spaces = [(len(list(cpart))) for c,cpart in groupby(ParametersArr_new[li]) if c == ' ']
    Counts = len(Spaces)
    
    #################################################

    # Needed Aliases
    if LineArr[3] == 'HIS':
        LineArr[3] = 'HSD'

    #################################################
    # Atom number
    if len(LineArr[1]) > len(str(atom_number)):
        LineArr[1] = ' ' * (len(LineArr[1]) - len(str(atom_number))) + str(atom_number)
    elif len(LineArr[1]) < len(str(atom_number)):
        Spaces[0] -= len(str(atom_number)) - len(LineArr[1])
        LineArr[1] = str(atom_number)
    else:
        LineArr[1] = str(atom_number)

    # Residue number (reset after TER - provided Protein chains/segments follow each other)  

    if found_resNum_Int5 and found_chainId_Lett:
        current_residue_number = int(LineArr[5]) - Residue_reset
        
        if current_residue_number <= 0:
            current_residue_number = int(LineArr[5])
            
        if len(LineArr[5]) > len(str(current_residue_number)):
            LineArr[5] = ' ' * (len(LineArr[5]) - len(str(current_residue_number))) + str(current_residue_number)
        elif len(LineArr[5]) < len(str(current_residue_number)):
            Spaces[4] -= len(str(current_residue_number)) - len(LineArr[5])
            LineArr[5] = str(current_residue_number)
        else:
            LineArr[5] = str(current_residue_number)

    else: 
        if found_resnum_Int4:
            current_residue_number = int(LineArr[4]) - Residue_reset
            
            if current_residue_number <= 0:
                current_residue_number = int(LineArr[4])
                
            if len(LineArr[4]) > len(str(current_residue_number)):
                LineArr[4] = ' ' * (len(LineArr[4]) - len(str(current_residue_number))) + str(current_residue_number)
            elif len(LineArr[4]) < len(str(current_residue_number)):
                Spaces[3] -= len(str(current_residue_number)) - len(LineArr[4])
                LineArr[4] = str(current_residue_number)
            else:
                LineArr[4] = str(current_residue_number)


    # Print
    for i in range(Counts):
        print >> f, LineArr[i] + ' ' * (Spaces[i] - 1),
    else:
        print >> f

    # Chain Divider    
    next = li + 1
    
    if found_resNum_Int5 and found_chainId_Lett:
        if next < Num_of_lines_new:
            LineArr_next = re.split(r'[\s]*', ParametersArr_new[next])
            del LineArr_next[-1]
            
            if LineArr_next[4] != LineArr[4]:
                atom_number = 0
                Residue_reset = int(LineArr_next[5]) - 1
                #Divider.append(next)
                print >> f, 'TER'
        #else:
        #    print >> f, 'TER'

    #else:
    #    if found_resnum_Int4:
    #        if next < Num_of_lines_new:
    #            LineArr_next = re.split(r'[\s]*', ParametersArr_new[next])
    #            del LineArr_next[-1]
    #            
    #            if int(LineArr[4]) == half_molecule and int(LineArr_next[4]) == int(LineArr[4]) + 1:
    #                atom_number = 0
    #                Residue_reset = int(LineArr[4])
    #                Divider = next
    #                print >> f, 'TER'
    #        else:
    #            print >> f, 'TER'
            
f.close()
##########################################################

file_temp = open('temp.pdb', 'rU')
#TempArr = file_temp.readlines()
TempArr = file_temp.read().splitlines()
file_temp.close()

NumLinesTemp = len(TempArr)    

num = 1
filename = 'pro' + str(num) + '.pdb'
tf = open(filename, 'w+')
for li in range(NumLinesTemp):
    if 'TER' in TempArr[li]:
        num += 1
        filename = 'pro' + str(num) + '.pdb'
        tf = open(filename, 'w+')
        continue

    print >> tf, TempArr[li]
