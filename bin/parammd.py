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

from itertools import groupby


from StringIO import StringIO
#from math import exp, expm1, log10, log, log1p
from subprocess import Popen, PIPE, STDOUT

if (len(sys.argv) < 1):
    print "\{\"error\":\"module called with no arguments\"\}\n";

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

#output_res[ 'progress_output' ] = 0 

json_variables = " "


InitialDirectoryStr = os.path.abspath(os.path.dirname(sys.argv[0]))
#InputDir = InitialDirectoryStr + '/willitfit_source/Source'

argv_io_string = StringIO(sys.argv[1])
json_variables = json.load(argv_io_string)

#####################################################

pdb_location = json_variables['pdb_location']
PdbStr = pdb_location[0]
Pdbfilename = os.path.basename(PdbStr)


## Clean Up ######################################################
for CleanUp in glob.glob('./*.*'):
    print CleanUp
    #exit()
    if not CleanUp.endswith(Pdbfilename):    
        os.remove(CleanUp)

## UDP messaging ##################################################
UDP_IP = json_variables['_udphost']
UDP_PORT = json_variables['_udpport']
sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP

socket_dict={}
socket_dict['_uuid'] = json_variables['_uuid']
###################################################################
try:
    shutil.rmtree('./toppar')
except:
    pass

TopparLocation = InitialDirectoryStr + '/script_toppar/toppar'
TopparLocationStr = InitialDirectoryStr + '/script_toppar/toppar.str'

shutil.copytree(TopparLocation, os.path.join(json_variables['_base_directory'],'toppar'))
shutil.copy(TopparLocationStr, json_variables['_base_directory'])

#### PDB upload ##################################################


file_pdb = open(PdbStr, 'rU')
PdbLines = file_pdb.readlines()
file_pdb.close()

NumLinesPdb = len(PdbLines)  
DnaList = ['C','G','A','T','DC','DG','DA','DT','CYT','GUA','ADE','THY']
ProteinList = ['ALA','CYS','ASP','GLU','PHE','GLY','HIS','ILE','LYS','LEU','MET','HSD','HSE','HSP','ASN','PRO','GLN','ARG','SER','THR','SEC','VAL','TRP','TYR']

CaseStudy = ''
ResidueList = []
Coordinates_x = []
Coordinates_y = []
Coordinates_z = []

for li in range(NumLinesPdb):
    LineArr = re.split(r'[\s]*', PdbLines[li])
    del LineArr[-1]
        
    if LineArr[0] != 'ATOM':
        continue
        
    if LineArr[2].startswith('H'):
        continue
        
    ResidueList.append(str(LineArr[3]))

    found_chainId_Lett = re.match(r'[a-zA-Z]+', str(LineArr[4]))
    found_resNum_Int5  = re.match(r'[0-9]+', str(LineArr[5]))
    found_resnum_Int4  = re.match(r'[0-9]+', str(LineArr[4]))
    
    if found_resNum_Int5 and found_chainId_Lett:
        Dots_6 = [(len(list(cpart))) for c,cpart in groupby(LineArr[6]) if c == '.']
        Counts_6 = len(Dots_6)
        Dots_7 = [(len(list(cpart))) for c,cpart in groupby(LineArr[7]) if c == '.']
        Counts_7 = len(Dots_7)
        if Counts_6 == 1 and Counts_7 == 2:
            Coordinates_x.append(float(LineArr[6]))

            i = 0
            cutindex = 0
            while i < len(str(LineArr[7])):
                if str(LineArr[7])[i] == '.':
                    cutindex = i+4
                    break
                i += 1
            #print LineArr, str(LineArr[7])[:cutindex], str(LineArr[7])[cutindex:-1]
            pos_y = str(LineArr[7])[:cutindex]
            pos_z = str(LineArr[7])[cutindex:-1]
            Coordinates_y.append(float(pos_y))
            Coordinates_z.append(float(pos_z))

        elif Counts_6 == 2 and Counts_7 == 1:
            i = 0
            cutindex = 0
            while i < len(str(LineArr[6])):
                if str(LineArr[6])[i] == '.':
                    cutindex = i+4
                    break
                i += 1
            #print LineArr, str(LineArr[7])[:cutindex], str(LineArr[7])[cutindex:-1]
            pos_x = str(LineArr[6])[:cutindex]
            pos_y = str(LineArr[6])[cutindex:-1]
            
            Coordinates_x.append(float(pos_x))
            Coordinates_y.append(float(pos_y))
            Coordinates_z.append(float(LineArr[8]))
        else:    
            Coordinates_x.append(float(LineArr[6]))
            Coordinates_y.append(float(LineArr[7])) 
            Coordinates_z.append(float(LineArr[8]))
        
    else:
        if found_resnum_Int4:
            Coordinates_x.append(float(LineArr[5]))
            Coordinates_y.append(float(LineArr[6])) 
            Coordinates_z.append(float(LineArr[7]))        
    
       
if any(word in ResidueList for word in DnaList):
    CaseStudy = 'dna' 
elif any(word in ResidueList for word in ProteinList):
    CaseStudy = 'protein' 
elif any(word in ResidueList for word in DnaList):
    if any(word in ResidueList for word in ProteinList):
        CaseStudy = 'nucleoprotein'

#print CaseStudy
#exit()

## Determine MINMAX of the molecule #############################
Max_x =  max(Coordinates_x)
Min_x =  min(Coordinates_x)
Max_y =  max(Coordinates_y)
Min_y =  min(Coordinates_y)
Max_z =  max(Coordinates_z)
Min_z =  min(Coordinates_z)

Diff_x = Max_x - Min_x
Diff_y = Max_y - Min_y
Diff_z = Max_z - Min_z


if 'add_waterbox' in json_variables:
    molsize_x = json_variables['size_x']
    molsize_y = json_variables['size_y']
    molsize_z = json_variables['size_z']

    if float(molsize_x) < Diff_x:
        #error_str = "Error: X_component of the periodic box is less than moleclular size! Size of the molecule is: {X, Y, Z} => {"+str(Diff_x)+", "+str(Diff_y)+", "+str(Diff_z)+"}..."
        error_str = "<p> Error: X_component of the periodic box is less than moleclular size! </p> <p> Size of the molecule is: {X, Y, Z} => {"+str(Diff_x)+", "+str(Diff_y)+", "+str(Diff_z)+"}... </p>"
        message_box(error_str,'warning.png')
        exit()
    if float(molsize_y) < Diff_y:
        error_str = "<p> Error: Y_component of the periodic box is less than moleclular size! </p> <p> Size of the molecule is: {X, Y, Z} => {"+str(Diff_x)+", "+str(Diff_y)+", "+str(Diff_z)+"}... </p>"
        message_box(error_str,'warning.png')
        exit()
    if float(molsize_z) < Diff_z:
        error_str = "<p> Error: Z_component of the periodic box is less than moleclular size! </p> <p> Size of the molecule is: {X, Y, Z} => {"+str(Diff_x)+", "+str(Diff_y)+", "+str(Diff_z)+"}... </p>"
        message_box(error_str,'warning.png')
        exit()


default_pbsize = 50

if 'add_ions' not in json_variables:
    if 'md_options' in json_variables:
        warning_str = "<p> Error:  </p> System needs to be solvated and ions have to be added before MD simulation run! <p> </p>"
        message_box(warning_str,'warning.png')
        exit()

if 'md_options' not in json_variables:
    if 'analysis_options_evolutions' in json_variables:
        warning_str = "<p> Error:  </p> MD simulation has to be run before Trajectory Analysis! <p> </p>"
        message_box(warning_str,'warning.png')
        exit()


#### PDB transformations #########################################

ProcessToCall = []
ExecutableFileLocation = InitialDirectoryStr + '/c36a6_svn132/exec/gnu/charmm'

if CaseStudy == 'dna':
    ScriptPdbParsingDna = InitialDirectoryStr + '/script_toppar/format_pdb_dna.py'
    
    ProcessPdbDna = []
    ProcessPdbDna.append(ScriptPdbParsingDna)
    ProcessPdbDna.append(PdbStr)
    p1 = subprocess.Popen(ProcessPdbDna)
    p1.wait()

    # Form Charmm prepper process ########################
    ScriptLocation = InitialDirectoryStr + '/script_toppar/pdbreader_general_na.inp'
    #ExecutableFileLocation = InitialDirectoryStr + '/c36a6_svn132/exec/gnu/charmm'
    
    ProcessToCall.append(ExecutableFileLocation)
    ProcessToCall.append('-i')
    ProcessToCall.append(ScriptLocation)
    
elif CaseStudy == 'protein':
    ScriptPdbParsingProtein = InitialDirectoryStr + '/script_toppar/format_pdb_protein.py'
    
    ProcessPdbProtein = []
    ProcessPdbProtein.append(ScriptPdbParsingProtein)
    ProcessPdbProtein.append(PdbStr)
    p1 = subprocess.Popen(ProcessPdbProtein)
    p1.wait()
    
    numseg = len(glob.glob('pro*.pdb'))

    # TER PATCHES TO INCLUDE
    # DISULFIDE PATCH TO INCLUDE

    # Form Charmm prepper process ########################
    ScriptLocation = InitialDirectoryStr + '/script_toppar/pdbreader_general_protein.inp'
    
    # Edit pdbreader_charmm template - number of chains####
    filedata0 = None
    with open(ScriptLocation, 'r') as file0:
        filedata0 = file0.read()
    filedata0 = filedata0.replace('NUMSEG', str(numseg))
    with open('charmm_prot.inp', 'w') as file0:
        file0.write(filedata0)
    
    #ExecutableFileLocation = InitialDirectoryStr + '/c36a6_svn132/exec/gnu/charmm'
    
    ProcessToCall.append(ExecutableFileLocation)
    ProcessToCall.append('-i')
    ProcessToCall.append('charmm_prot.inp')

else:
    print "Molecules other than DNA, proteins, or their complexes are not currently supported!"
    exit()


## RUN CHARMM ##########################################################
socket_dict["_textarea"] = 'Starting Calculations...\n\n'
socket_dict["progress_output"] =  str(0.0)
#socket_dict["progress_text"] =  'Fitting Progress: 0%'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))


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
                #socket_dict['progress_output'] = OutArr[-1]
                #socket_dict['progress_text'] = 'Fitting Progress: ' + str(int(float(OutArr[-1])*100.0)) + '%'
            if socket_dict:
                doc_string = json.dumps(socket_dict)
                sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                time.sleep(0.005)
    elif Process.poll() is not None:  
        break

Process.wait()
os.close(slave)
os.close(master)

socket_dict["_textarea"] = '\n Parameterization Finished...\n\n'
if socket_dict:
    doc_string = json.dumps(socket_dict)
    sock.sendto(doc_string,(UDP_IP,UDP_PORT))


## WATER BOX creation ######################################################################

if 'add_waterbox' in json_variables:
    ProcessToCallWaterbox = []
    ScriptLocationWater = InitialDirectoryStr + '/script_toppar/step2.1_waterbox.inp'

    # Edit pdbreader_charmm template - number of chains####
    filedata0 = None
    with open(ScriptLocationWater, 'r') as file0:
        filedata0 = file0.read()
    filedata0 = filedata0.replace('CELL_X', str(molsize_x))
    filedata0 = filedata0.replace('CELL_Y', str(molsize_y))
    filedata0 = filedata0.replace('CELL_Z', str(molsize_z))

    with open('charmm_waterbox.inp', 'w') as file0:
        file0.write(filedata0)

    ProcessToCallWaterbox.append(ExecutableFileLocation)
    ProcessToCallWaterbox.append('-i')
    ProcessToCallWaterbox.append('charmm_waterbox.inp')

    ## RUN CHARMM WATERBOX ##########################################################
    socket_dict["_textarea"] = '\n Adding Waterbox...\n\n'
    socket_dict["progress_output"] =  str(0.0)
    #socket_dict["progress_text"] =  'Fitting Progress: 0%'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))

        ProcessWaterbox = subprocess.Popen(ProcessToCallWaterbox)
 
#    master_waterbox, slave_waterbox = pty.openpty()
#    ProcessWaterbox = subprocess.Popen(ProcessToCallWaterbox,stdout=slave_waterbox,stdin=PIPE,bufsize=0,close_fds=True)

#    stdout_waterbox = os.fdopen(master_waterbox, 'r', 0)
    #timeout = .04 # seconds


#    while True:
#        #output = Process.stdout.readline()
#        #signal.signal(signal.SIGTERM, functools.partial(handler,Process.pid))
#
#        ready, _, _ = select.select([master_waterbox], [], [], timeout)
#        if ready:
#            output = stdout_waterbox.readline()
#            if not output:
#                break
#            if output:
#                if "Progress" not in output: 
#                    socket_dict["_textarea"] = output#.strip()
#                else:
#                    OutArr = re.split(r'[\s]*', output)
#                    del OutArr[-1]
#                    #socket_dict['progress_output'] = OutArr[-1]
#                    #socket_dict['progress_text'] = 'Fitting Progress: ' + str(int(float(OutArr[-1])*100.0)) + '%'
#                if socket_dict:
#                    doc_string = json.dumps(socket_dict)
#                    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
#                    time.sleep(0.002)
#        elif ProcessWaterbox.poll() is not None:  
#            break

    ProcessWaterbox.wait()
 #   os.close(slave_waterbox)
 #   os.close(master_waterbox)

    socket_dict["_textarea"] = '\n Waterbox Added...\n\n'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))


    ## IF NO IONS WERE ADDED - GENERATE SOLVATED SYSTEM ###########################
    
    if 'add_ions' not in json_variables:
        ProcessToCallSolvsys = []
        ScriptLocationSolvsys = InitialDirectoryStr + '/script_toppar/step2_solvator_no_ions.inp'

        ProcessToCallSolvsys.append(ExecutableFileLocation)
        ProcessToCallSolvsys.append('-i')
        ProcessToCallSolvsys.append(ScriptLocationSolvsys)

        ## RUN CHARMM GENERATE SOLVATED SYS WITH IONS ###############################################
        socket_dict["_textarea"] = '\n Generating Solvated System without Ions...\n\n'
        socket_dict["progress_output"] =  str(0.0)
        #socket_dict["progress_text"] =  'Fitting Progress: 0%'
        if socket_dict:
            doc_string = json.dumps(socket_dict)
            sock.sendto(doc_string,(UDP_IP,UDP_PORT))

        ProcessSolvsys = subprocess.Popen(ProcessToCallSolvsys)

#        master_solvsys, slave_solvsys = pty.openpty()
#        ProcessSolvsys = subprocess.Popen(ProcessToCallSolvsys,stdout=slave_solvsys,stdin=PIPE,bufsize=0,close_fds=True)

#        stdout_solvsys = os.fdopen(master_solvsys, 'r', 0)
        #timeout = .04 # seconds

#        while True:
#            ready, _, _ = select.select([master_solvsys], [], [], timeout)
#            if ready:
#                output = stdout_solvsys.readline()
#                if not output:
#                    break
#                if output:
#                    if "Progress" not in output: 
#                        socket_dict["_textarea"] = output#.strip()
#                    else:
#                        OutArr = re.split(r'[\s]*', output)
#                        del OutArr[-1]
#                        #socket_dict['progress_output'] = OutArr[-1]
#                        #socket_dict['progress_text'] = 'Fitting Progress: ' + str(int(float(OutArr[-1])*100.0)) + '%'
#                    if socket_dict:
#                        doc_string = json.dumps(socket_dict)
#                        sock.sendto(doc_string,(UDP_IP,UDP_PORT))
#                        time.sleep(0.002)
#            elif ProcessSolvsys.poll() is not None:  
#                break

        ProcessSolvsys.wait()
#        os.close(slave_solvsys)
#        os.close(master_solvsys)

        socket_dict["_textarea"] = '\n Solvated System Generated...\n\n'
        if socket_dict:
            doc_string = json.dumps(socket_dict)
            sock.sendto(doc_string,(UDP_IP,UDP_PORT))   
    

## ADD IONS  ######################################################################

if 'add_ions' in json_variables:
    ProcessToCallAddions = []
    ScriptLocationIons = InitialDirectoryStr + '/script_toppar/step2.2_ions.inp'

    CrystalImageLocationStr = InitialDirectoryStr + '/script_toppar/crystal_image.str'
    shutil.copy(CrystalImageLocationStr, json_variables['_base_directory'])

    ionic_conc = json_variables['ion_conc']
    ionic_type = json_variables['ion_type']
    

    # Edit pdbreader_charmm template####
    filedata0 = None
    with open(ScriptLocationIons, 'r') as file0:
        filedata0 = file0.read()
    filedata0 = filedata0.replace('IONCONC', str(ionic_conc))
    filedata0 = filedata0.replace('COUNTERION', str(ionic_type))

    with open('charmm_addions.inp', 'w') as file0:
        file0.write(filedata0)

    ProcessToCallAddions.append(ExecutableFileLocation)
    ProcessToCallAddions.append('-i')
    ProcessToCallAddions.append('charmm_addions.inp')

    ## RUN CHARMM ADDIONS ##########################################################
    socket_dict["_textarea"] = '\n Adding Ions...\n\n'
    socket_dict["progress_output"] =  str(0.0)
    #socket_dict["progress_text"] =  'Fitting Progress: 0%'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))

    ProcessAddions = subprocess.Popen(ProcessToCallAddions)

#    master_addions, slave_addions = pty.openpty()
#    ProcessAddions = subprocess.Popen(ProcessToCallAddions,stdout=slave_addions,stdin=PIPE,bufsize=0,close_fds=True)



#    stdout_addions = os.fdopen(master_addions, 'r', 0)
    #timeout = .04 # seconds


#    while True:
#        ready, _, _ = select.select([master_addions], [], [], timeout)
#        if ready:
#            output = stdout_addions.readline()
#            if not output:
#                break
#            if output:
#                if "Progress" not in output: 
#                    socket_dict["_textarea"] = output#.strip()
#                else:
#                    OutArr = re.split(r'[\s]*', output)
#                    del OutArr[-1]
#                    #socket_dict['progress_output'] = OutArr[-1]
#                    #socket_dict['progress_text'] = 'Fitting Progress: ' + str(int(float(OutArr[-1])*100.0)) + '%'
#                if socket_dict:
#                    doc_string = json.dumps(socket_dict)
#                    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
#                    time.sleep(0.002)
#        elif ProcessAddions.poll() is not None:  
#            break

    ProcessAddions.wait()
 #   os.close(slave_addions)
 #   os.close(master_addions)

    socket_dict["_textarea"] = '\n Ions Have Been Added...\n\n'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))


    #### GENERATION OF SOLVATED SYSTEM WITH IONS################################################
    ProcessToCallSolvsysions = []
    ScriptLocationSolvsysions = InitialDirectoryStr + '/script_toppar/step2_solvator.inp'

    ProcessToCallSolvsysions.append(ExecutableFileLocation)
    ProcessToCallSolvsysions.append('-i')
    ProcessToCallSolvsysions.append(ScriptLocationSolvsysions)

    ## RUN CHARMM GENERATE SOLVATED SYS WITH IONS ###############################################
    socket_dict["_textarea"] = '\n Generating Solvated System with Ions...\n\n'
    socket_dict["progress_output"] =  str(0.0)
    #socket_dict["progress_text"] =  'Fitting Progress: 0%'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))

    ProcessSolvsysions = subprocess.Popen(ProcessToCallSolvsysions)

#    master_solvsysions, slave_solvsysions = pty.openpty()
#    ProcessSolvsysions = subprocess.Popen(ProcessToCallSolvsysions,stdout=slave_solvsysions,stdin=PIPE,bufsize=0,close_fds=True)

#    stdout_solvsysions = os.fdopen(master_solvsysions, 'r', 0)
    #timeout = .04 # seconds

#    while True:
#        ready, _, _ = select.select([master_solvsysions], [], [], timeout)
#        if ready:
#            output = stdout_solvsysions.readline()
#            if not output:
#                break
#            if output:
#                if "Progress" not in output: 
#                    socket_dict["_textarea"] = output#.strip()
#                else:
#                    OutArr = re.split(r'[\s]*', output)
#                    del OutArr[-1]
#                    #socket_dict['progress_output'] = OutArr[-1]
#                    #socket_dict['progress_text'] = 'Fitting Progress: ' + str(int(float(OutArr[-1])*100.0)) + '%'
#                if socket_dict:
#                    doc_string = json.dumps(socket_dict)
#                    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
#                    time.sleep(0.002)
#        elif ProcessSolvsysions.poll() is not None:  
#            break

    ProcessSolvsysions.wait()
#    os.close(slave_solvsysions)
#    os.close(master_solvsysions)

    socket_dict["_textarea"] = '\n Solvated System Generated...\n\n'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))   

################################################################################################
########### NAMD Inputs (minimisation for now)  ################################################
ExecutableFileLocationNamd = InitialDirectoryStr + '/namd_bin/NAMD_2.11_Linux-x86_64/namd2'

if 'min_options' in json_variables:
    ScriptLocation_namdmin  = InitialDirectoryStr + '/namd_templates/namd_template_whole_min.in'
    #ExecutableFileLocationNamd = InitialDirectoryStr + '/namd_bin/NAMD_2.11_Linux-x86_64/namd2'
    ProcessToCallNamdMin = []

    numsteps = json_variables['mm_min_num_steps']
    coor_min = ''
    psf_min = ''

    if 'add_waterbox' in json_variables:
        coor_min = 'step2_solvator.pdb'
        psf_min =  'step2_solvator.xplor.psf'
    else:
        coor_min = 'step1_pdbreader.pdb'
        psf_min =  'step1_pdbreader.xplor.psf'
      
    if CaseStudy == 'dna':
        # Edit pdbreader_charmm template - number of chains####
        filedata0 = None
        with open(ScriptLocation_namdmin, 'r') as file0:
            filedata0 = file0.read()
        filedata0 = filedata0.replace('PAR_BIOMOL', 'par_all36_na_for_namd.prm')
        filedata0 = filedata0.replace('NUM_STEPS', str(numsteps))
        filedata0 = filedata0.replace('COOR_MIN', str(coor_min))
        filedata0 = filedata0.replace('PSF_MIN', str(psf_min))
        
        if 'add_waterbox' in json_variables:
            filedata0 = filedata0.replace('CELL_X', str(molsize_x))
            filedata0 = filedata0.replace('CELL_Y', str(molsize_y))
            filedata0 = filedata0.replace('CELL_Z', str(molsize_z))
        else:
            filedata0 = filedata0.replace('CELL_X', str(default_pbsize))
            filedata0 = filedata0.replace('CELL_Y', str(default_pbsize))
            filedata0 = filedata0.replace('CELL_Z', str(default_pbsize))

        with open('namd_dna_min.inp', 'w') as file0:
            file0.write(filedata0)
                
        ProcessToCallNamdMin.append(ExecutableFileLocationNamd)
        ProcessToCallNamdMin.append('namd_dna_min.inp')

    if CaseStudy == 'protein':
        # Edit pdbreader_charmm template - number of chains####
        filedata0 = None
        with open(ScriptLocation_namdmin, 'r') as file0:
            filedata0 = file0.read()
        filedata0 = filedata0.replace('PAR_BIOMOL', 'par_all36_prot_for_namd.prm')
        filedata0 = filedata0.replace('NUM_STEPS', str(numsteps))
        filedata0 = filedata0.replace('COOR_MIN', str(coor_min))
        filedata0 = filedata0.replace('PSF_MIN', str(psf_min))
        
        if 'add_waterbox' in json_variables:
            filedata0 = filedata0.replace('CELL_X', str(molsize_x))
            filedata0 = filedata0.replace('CELL_Y', str(molsize_y))
            filedata0 = filedata0.replace('CELL_Z', str(molsize_z))
        else:
            filedata0 = filedata0.replace('CELL_X', str(default_pbsize))
            filedata0 = filedata0.replace('CELL_Y', str(default_pbsize))
            filedata0 = filedata0.replace('CELL_Z', str(default_pbsize))

        with open('namd_prot_min.inp', 'w') as file0:
            file0.write(filedata0)

        ProcessToCallNamdMin.append(ExecutableFileLocationNamd)
        ProcessToCallNamdMin.append('namd_prot_min.inp')
    
   
    ## RUN NAMD ##########################################################
    socket_dict["_textarea"] = 'Starting Minimization...\n\n'
    socket_dict["progress_output"] =  str(0.0)
    socket_dict["progress_text"] =  'Minimization Progress: 0%'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))
        
        
    master_namdmin, slave_namdmin = pty.openpty()
    ProcessNamdmin = subprocess.Popen(ProcessToCallNamdMin,stdout=slave_namdmin,stdin=PIPE,bufsize=0,close_fds=True)
        
    stdout_namdmin = os.fdopen(master_namdmin, 'r', 0)
        
    while True:
        ready, _, _ = select.select([master_namdmin], [], [], timeout)
        if ready:
            output = stdout_namdmin.readline()
            if not output:
                break
            if output:
                socket_dict["_textarea"] = output#.strip()
                if "ENERGY:" in output:
                    OutArr = re.split(r'[\s]*', output)
                    if OutArr[0] == 'ENERGY:':
                        socket_dict['progress_output'] = float(OutArr[1])/float(numsteps)
                        socket_dict['progress_text'] = 'Minimization Progress: ' + str(int ((float(OutArr[1])/float(numsteps))*100.0) ) + '%'
                        
                if socket_dict:
                    doc_string = json.dumps(socket_dict)
                    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                    #time.sleep(0.002)
        elif ProcessNamdmin.poll() is not None:  
            break
                  
    ProcessNamdmin.wait()
    os.close(slave_namdmin)
    os.close(master_namdmin)
                                
    socket_dict["_textarea"] = '\nMinimization Finished...\n\n'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))

    
    output_res[ 'progress_output' ] = str(1.0)
    output_res[ 'progress_text' ] = 'Minimization Progress: 100%' 


########## MD ##################################################################
if 'md_options' in json_variables:
    ScriptLocation_namdmd  = InitialDirectoryStr + '/namd_templates/namd_template_equil.in'
    #ExecutableFileLocationNamd = InitialDirectoryStr + '/namd_bin/NAMD_2.11_Linux-x86_64/namd2'
    ProcessToCallNamdMd = []

    numsteps_md = json_variables['md_num_steps']
    temperature = json_variables['temperature']
    pressure = json_variables['pressure']
    output_frequency = json_variables['output_frequency']
    
    coor_md = ''
    psf_md = 'step2_solvator.xplor.psf'

    if 'min_options' in json_variables: 
        coor_md = 'namd_out_whole_min.coor'
    else:
        coor_md = 'step2_solvator.pdb'

    if CaseStudy == 'dna':
        # Edit pdbreader_charmm template - number of chains####
        filedata0 = None
        with open(ScriptLocation_namdmd, 'r') as file0:
            filedata0 = file0.read()
        filedata0 = filedata0.replace('PAR_BIOMOL', 'par_all36_na_for_namd.prm')
        filedata0 = filedata0.replace('STPEPSMD', str(numsteps_md))
        filedata0 = filedata0.replace('COOR_MD', str(coor_md))
        filedata0 = filedata0.replace('PSF_MD', str(psf_md))
        filedata0 = filedata0.replace('TEMP', str(temperature))
        filedata0 = filedata0.replace('PRESSURE', str(pressure))
        filedata0 = filedata0.replace('OUTFREQ', str(output_frequency))
        
        if 'add_waterbox' in json_variables:
            filedata0 = filedata0.replace('CELL_X', str(molsize_x))
            filedata0 = filedata0.replace('CELL_Y', str(molsize_y))
            filedata0 = filedata0.replace('CELL_Z', str(molsize_z))
        else:
            filedata0 = filedata0.replace('CELL_X', str(default_pbsize))
            filedata0 = filedata0.replace('CELL_Y', str(default_pbsize))
            filedata0 = filedata0.replace('CELL_Z', str(default_pbsize))

        with open('namd_dna_md.inp', 'w') as file0:
            file0.write(filedata0)
                
        ProcessToCallNamdMd.append(ExecutableFileLocationNamd)
        ProcessToCallNamdMd.append('namd_dna_md.inp')

    if CaseStudy == 'protein':
        # Edit pdbreader_charmm template - number of chains####
        filedata0 = None
        with open(ScriptLocation_namdmd, 'r') as file0:
            filedata0 = file0.read()
        filedata0 = filedata0.replace('PAR_BIOMOL', 'par_all36_prot_for_namd.prm')
        filedata0 = filedata0.replace('STPEPSMD', str(numsteps_md))
        filedata0 = filedata0.replace('COOR_MD', str(coor_md))
        filedata0 = filedata0.replace('PSF_MD', str(psf_md))
        filedata0 = filedata0.replace('TEMP', str(temperature))
        filedata0 = filedata0.replace('PRESSURE', str(pressure))
        filedata0 = filedata0.replace('OUTFREQ', str(output_frequency)) 
        
        if 'add_waterbox' in json_variables:
            filedata0 = filedata0.replace('CELL_X', str(molsize_x))
            filedata0 = filedata0.replace('CELL_Y', str(molsize_y))
            filedata0 = filedata0.replace('CELL_Z', str(molsize_z))
        else:
            filedata0 = filedata0.replace('CELL_X', str(default_pbsize))
            filedata0 = filedata0.replace('CELL_Y', str(default_pbsize))
            filedata0 = filedata0.replace('CELL_Z', str(default_pbsize))

        with open('namd_prot_md.inp', 'w') as file0:
            file0.write(filedata0)

        ProcessToCallNamdMd.append(ExecutableFileLocationNamd)
        ProcessToCallNamdMd.append('namd_prot_md.inp')
    
    ## RUN NAMD MD  ##########################################################
    socket_dict["_textarea"] = 'Starting MD Simulation...\n\n'
    socket_dict["progress_output"] =  str(0.0)
    socket_dict["progress_text"] =  'MD Simulation Progress: 0%'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))
        
        
    master_namdmd, slave_namdmd = pty.openpty()
    ProcessNamdmd = subprocess.Popen(ProcessToCallNamdMd,stdout=slave_namdmd,stdin=PIPE,bufsize=0,close_fds=True)
        
    stdout_namdmd = os.fdopen(master_namdmd, 'r', 0)
    namdmd_log = open('namdmd_log.out','w')
    temp_plot  = open('temperature_plot.dat','w')
    totenergy_plot  = open('totalenergy_plot.dat','w')
    time_temperature_array = []
    time_array = []
    temperature_array = []
    totenergy_array = []
    time_totenergy_array = []

    error_string_md = ''

    while True:
        ready, _, _ = select.select([master_namdmd], [], [], timeout)
        if ready:
            output = stdout_namdmd.readline()
            if not output:
                break
            if output:
                socket_dict["_textarea"] = output#.strip()
                print >> namdmd_log, output.rstrip()
                
                if "ENERGY:" in output:
                    OutArr = re.split(r'[\s]*', output)
                    if OutArr[0] == 'ENERGY:':
                        socket_dict['progress_output'] = float(OutArr[1])/float(numsteps_md)
                        socket_dict['progress_text'] = 'MD Simulation Progress: ' + str(int ((float(OutArr[1])/float(numsteps_md))*100.0) ) + '%'
                        # TEMPERATURE
                        time_temperature_array.append([float(OutArr[1]),float(OutArr[12])])
                        time_array.append(float(OutArr[1]))
                        temperature_array.append(float(OutArr[12]))
                        temperature_string = str(float(OutArr[1])) + "\t" + str(float(OutArr[12]))
                        print >> temp_plot, temperature_string
                        # TOT_ENERGY
                        time_totenergy_array.append([float(OutArr[1]),float(OutArr[11])])
                        totenergy_array.append(float(OutArr[11]))
                        totenergy_string = str(float(OutArr[1])) + "\t" + str(float(OutArr[11]))
                        print >> totenergy_plot, totenergy_string
               
                if "ERROR:" in output:
                    socket_dict["_textarea"] = output#.strip()
                    error_string_md += "<p>" + output + "</p>"
                    
                if socket_dict:
                    doc_string = json.dumps(socket_dict)
                    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                    #time.sleep(0.002)
        elif ProcessNamdmd.poll() is not None:  
            break
                  
    ProcessNamdmd.wait()
    os.close(slave_namdmd)
    os.close(master_namdmd)
                                
    socket_dict["_textarea"] = '\nMD Simulation Completed...\n\n'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))

        
    output_res[ 'progress_output' ] = str(1.0)
    output_res[ 'progress_text' ] = 'MD Simulation Progress: 100%' 

    if error_string_md:
        warning_str = "<p> Error:  </p> MD simulation terminated with the following error(s): <p> </p>" + error_string_md + "<p> Exiting... </p>"
        message_box(warning_str,'warning.png')
        output_res[ 'error' ] = 'MD simulation crashed. See the log file...'
        base_dir = json_variables[ '_base_directory' ]

        output_basename = ''
        outpdb_view = ''
        
        if 'add_waterbox' in json_variables:
            output_basename = 'step2_solvator'
            outpdb_view = output_basename
            if 'min_options' in json_variables:
                shutil.copy('namd_out_whole_min.coor', 'namd_out_whole_min.pdb')
                outpdb_view = 'namd_out_whole_min'
        else:
            output_basename = 'step1_pdbreader'
            outpdb_view = output_basename

        outpdb = str(base_dir) + "/" + output_basename + ".pdb"
        outcrd = str(base_dir) + "/" + output_basename + ".crd"
        outpsf = str(base_dir) + "/" + output_basename + ".psf"
        outxplorpsf = str(base_dir) + "/" + output_basename + ".xplor.psf"
        
        outpdb_atomic_view = str(base_dir) + "/" + outpdb_view + ".pdb"
        
        output_res[ 'outputpdb' ] = str(outpdb)
        output_res[ 'outputcrd' ] = str(outcrd)
        output_res[ 'outputpsf' ] = str(outpsf)
        output_res[ 'outputxplorpsf' ] = str(outxplorpsf)
        
        outputmin_arr_files = []
        outputmin_arr_files.append(str(base_dir) + "/" + 'namd_out_whole_min' + ".coor")
        #outputmin_arr_files.append(str(base_dir) + "/" + 'namd_out_whole_min' + ".vel")

        outputmd_arr_files = []
        
        dcd_path =  str(base_dir) + "/" + "namd_equil.dcd"
        vel_path =  str(base_dir) + "/" + "namd_out_equil.restart.vel"
        coor_path = str(base_dir) + "/" + "namd_out_equil.restart.coor"
        xsc_path =  str(base_dir) + "/" + "namd_out_equil.restart.xsc"
        xst_path =  str(base_dir) + "/" + "namd_out_equil.xst"

        if os.path.exists(dcd_path):
            outputmd_arr_files.append(dcd_path)
        if os.path.exists(vel_path):
            outputmd_arr_files.append(vel_path)
        if os.path.exists(coor_path):
            outputmd_arr_files.append(coor_path)
        if os.path.exists(xsc_path):
            outputmd_arr_files.append(xsc_path)
        if os.path.exists(xst_path):
            outputmd_arr_files.append(xst_path)
        #outputmd_arr_files.append(str(base_dir) + "/" + "namd_out_equil.vel")
        #outputmd_arr_files.append(str(base_dir) + "/" + "namd_out_equil.coor")
        #outputmd_arr_files.append(str(base_dir) + "/" + "namd_out_equil.xsc")
        #outputmd_arr_files.append(str(base_dir) + "/" + "namd_out_equil.xst")
        outputmd_arr_files.append(str(base_dir) + "/" + "namdmd_log.out")

        if 'md_options' in json_variables:
            output_res[ 'outputmd' ] = outputmd_arr_files
 
        if 'min_options' in json_variables:
            output_res[ 'outputmin' ] = outputmin_arr_files

        output_res[ 'outputpdb_view' ] = str(outpdb_atomic_view)

        print json.dumps( output_res )
        exit()


#################################################################
base_dir = json_variables[ '_base_directory' ]

output_basename = ''
outpdb_view = ''

if 'add_waterbox' in json_variables:
    output_basename = 'step2_solvator'
    outpdb_view = output_basename
    if 'min_options' in json_variables:
        shutil.copy('namd_out_whole_min.coor', 'namd_out_whole_min.pdb')
        outpdb_view = 'namd_out_whole_min'
else:
    output_basename = 'step1_pdbreader'
    outpdb_view = output_basename

outpdb = str(base_dir) + "/" + output_basename + ".pdb"
outcrd = str(base_dir) + "/" + output_basename + ".crd"
outpsf = str(base_dir) + "/" + output_basename + ".psf"
outxplorpsf = str(base_dir) + "/" + output_basename + ".xplor.psf"

outpdb_atomic_view = str(base_dir) + "/" + outpdb_view + ".pdb"

output_res[ 'outputpdb' ] = str(outpdb)
output_res[ 'outputcrd' ] = str(outcrd)
output_res[ 'outputpsf' ] = str(outpsf)
output_res[ 'outputxplorpsf' ] = str(outxplorpsf)

outputmin_arr_files = []
outputmin_arr_files.append(str(base_dir) + "/" + 'namd_out_whole_min' + ".coor")
#outputmin_arr_files.append(str(base_dir) + "/" + 'namd_out_whole_min' + ".vel")

outputmd_arr_files = []
outputmd_arr_files.append(str(base_dir) + "/" + "namd_equil.dcd")
outputmd_arr_files.append(str(base_dir) + "/" + "namd_out_equil.vel")
outputmd_arr_files.append(str(base_dir) + "/" + "namd_out_equil.coor")
outputmd_arr_files.append(str(base_dir) + "/" + "namd_out_equil.xsc")
outputmd_arr_files.append(str(base_dir) + "/" + "namd_out_equil.xst")
outputmd_arr_files.append(str(base_dir) + "/" + "namdmd_log.out")

if 'md_options' in json_variables:
    output_res[ 'outputmd' ] = outputmd_arr_files
 
if 'min_options' in json_variables:
    output_res[ 'outputmin' ] = outputmin_arr_files

output_res[ 'outputpdb_view' ] = str(outpdb_atomic_view)


### SHOW VIDEO #########################################################

if 'movie_show' in json_variables:
    socket_dict["_textarea"] = '\nGenerating Movie for the Simulation Trajectory...\n\n'
    socket_dict["progress_output"] =  str(0.0)
    socket_dict["progress_text"] =  'Movie Generation Progress: 0%'
    
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))
    
    ScriptLocationMovie = ''
    ScriptLocationVMD = ''

    if json_variables[ 'movie_choice' ] == 'withsolvent':    
        ScriptLocationMovie = InitialDirectoryStr + '/vmd_script/pov_to_png_with_solvent.sh'
        ScriptLocationVMD = InitialDirectoryStr + '/vmd_script/vmd_movie_maker.tcl'
        shutil.copy(ScriptLocationVMD, json_variables['_base_directory'])
    if json_variables[ 'movie_choice' ] == 'nosolvent':    
        ScriptLocationMovie = InitialDirectoryStr + '/vmd_script/pov_to_png_no_solvent.sh'
        ScriptLocationVMD = InitialDirectoryStr + '/vmd_script/vmd_movie_maker.tcl'
        ScriptLocationStripWater = InitialDirectoryStr + '/vmd_script/vmd_strip_water.tcl'
        shutil.copy(ScriptLocationVMD, json_variables['_base_directory'])
        shutil.copy(ScriptLocationStripWater, json_variables['_base_directory'])
    
    moviename = "out_proc_" + json_variables['_uuid']     
    ProcessVideo = []
    ProcessVideo.append(ScriptLocationMovie)
    ProcessVideo.append(moviename)

    master_povproc, slave_povproc = pty.openpty()
    ProcessCreateMovie = subprocess.Popen(ProcessVideo,stdout=slave_povproc,stdin=PIPE,bufsize=0,close_fds=True)
    
    stdout_povproc = os.fdopen(master_povproc, 'r', 0)
       
    while True:
        ready, _, _ = select.select([master_povproc], [], [], timeout)
        if ready:
            output = stdout_povproc.readline()
            if not output:
                break
            if output:
                socket_dict["_textarea"] = output#.strip()
                #socket_dict["_textarea"] = ""
                if "Processing file" in output:
                    OutArr = re.split(r'[\s]*', output)
                    socket_dict['progress_output'] = float(OutArr[3])/float(OutArr[5])
                    socket_dict['progress_text'] = 'Movie Generation Progress: ' + str(int ((float(OutArr[3])/float(OutArr[5]))*100.0) ) + '%'
                    
                if socket_dict:
                    doc_string = json.dumps(socket_dict)
                    sock.sendto(doc_string,(UDP_IP,UDP_PORT))
                    #time.sleep(0.002)
        elif ProcessCreateMovie.poll() is not None:  
            break
                  
    ProcessCreateMovie.wait()
    os.close(slave_povproc)
    os.close(master_povproc)

    socket_dict["_textarea"] = '\nMovie Generated...\n\n'
    if socket_dict:
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))

    output_res[ 'progress_output' ] = str(1.0)
    output_res[ 'progress_text' ] = 'Movie Generation Progress: 100%' 
    
    output_res[ 'vid' ] = str(base_dir) + "/" + moviename
    
#### OUTPUT PLOTS #################################################
#### TEMPERATURE PLOT #######################################################
if 'analysis_options_evolutions_temperature' in json_variables:
    
    min_x = min(time_array)
    min_y = min(temperature_array)
    max_x = max(time_array)
    max_y = max(temperature_array)

    flot_data_temperature = {
        "options": 
        {
            #"xscale" : "log",
            #"yscale" : "log",
            "xmin"   : min_x,
            "xmax"   : max_x,
            "ymin"   : min_y,
            "ymax"   : max_y,
            "xlabel" : "Time, steps",
            "ylabel" : "T(K)",
            "legend" : 
            {
                "backgroundOpacity"   : 0.2,
                "labelBoxBorderColor" : "black",
                "position"            : "ne",
                "margin"              : [0, -22]
            },
        },
        "data": [ ]
    }
    
    data_label_temperature = "Temperature"
    current_set_data_temperature = {
        "points"  : { "show" : "true" },
        "lines"  :  { "show" : "true" },
        "label"  : data_label_temperature,
        "data"   : time_temperature_array
    }
    flot_data_temperature['data'].append(current_set_data_temperature)
    
    output_res[ 'temperature_plot' ] = flot_data_temperature

## ENERGY PLOT #######################################################
if 'analysis_options_evolutions_totenergy' in json_variables:
    
    min_x = min(time_array)
    min_y = min(totenergy_array)
    max_x = max(time_array)
    max_y = max(totenergy_array)

    flot_data_totenergy = {
        "options": 
        {
            #"xscale" : "log",
            #"yscale" : "log",
            "xmin"   : min_x,
            "xmax"   : max_x,
            "ymin"   : min_y,
            "ymax"   : max_y,
            "xlabel" : "Time, steps",
            "ylabel" : "Energy (kcal/mol)",
            "legend" : 
            {
                "backgroundOpacity"   : 0.2,
                "labelBoxBorderColor" : "black",
                "position"            : "ne",
                "margin"              : [0, -22]
            },
        },
        "data": [ ]
    }
    
    data_label_totenergy = "Total Energy"
    current_set_data_totenergy = {
        "points"  : { "show" : "true" },
        "lines"  :  { "show" : "true" },
        "label"  : data_label_totenergy,
        "data"   : time_totenergy_array
    }
    flot_data_totenergy['data'].append(current_set_data_totenergy)
    
    output_res[ 'totenergy_plot' ] = flot_data_totenergy

### Analysis Plots results ######################################

outputanalysis_arr_files = []
outputanalysis_arr_files.append(str(base_dir) + "/" + "temperature_plot.dat")
outputanalysis_arr_files.append(str(base_dir) + "/" + "totalenergy_plot.dat")

output_res[ 'outputanalysis' ] = outputanalysis_arr_files

##################################################################

print json.dumps( output_res )
