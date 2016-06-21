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

#for line in ParametersArr:
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
atom_number = 0

for li in range(Num_of_lines_new):
    
    LineArr = re.split(r'[\s]*', ParametersArr_new[li])
    atom_number += 1
    
    Fields = len(LineArr)
    Spaces = [(len(list(cpart))) for c,cpart in groupby(ParametersArr_new[li]) if c == ' ']
    Counts = len(Spaces)
    
    # DNA ################################
    if LineArr[3] == 'C':
        LineArr[3] = 'CYT'
        Spaces[2] -= 2
    if LineArr[3] == 'DC':
        LineArr[3] = 'CYT'
        Spaces[2] -= 1
        
    if LineArr[3] == 'G':
        LineArr[3] = 'GUA'
        Spaces[2] -= 2
    if LineArr[3] == 'DG':
        LineArr[3] = 'GUA'
        Spaces[2] -= 1

    if LineArr[3] == 'A':
        LineArr[3] = 'ADE'
        Spaces[2] -= 2
    if LineArr[3] == 'DA':
        LineArr[3] = 'ADE'
        Spaces[2] -= 1

    if LineArr[3] == 'T':
        LineArr[3] = 'THY'
        Spaces[2] -= 2
    if LineArr[3] == 'DT':
        LineArr[3] = 'THY'
        Spaces[2] -= 1

    if LineArr[2].endswith('*'):
        new = list(LineArr[2])
        new[-1] = "'"
        LineArr[2] = ''.join(new)
    #################################################   

    if len(LineArr[1]) > len(str(atom_number)):
        LineArr[1] = str(atom_number) + ' ' * (len(LineArr[1]) - len(str(atom_number)))
    else:
        LineArr[1] = str(atom_number)

    
    for i in range(Counts):
        print LineArr[i] + ' ' * (Spaces[i] - 1),
    else:
        print
    
    next = li + 1
    if next < Num_of_lines_new:
        
        LineArr_next = re.split(r'[\s]*', ParametersArr_new[next])
        del LineArr_next[-1]
        
        if LineArr_next[4] != LineArr[4]:
            print 'TER'
                        
    else:
        print 'TER'
