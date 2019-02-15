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
import pandas as pd
import numpy as np
from itertools import groupby
from matplotlib import cm, colors
from StringIO import StringIO
from subprocess import Popen, PIPE, STDOUT

#import our functions
from run_rotdif_elmdock import rotdif
from plot_2d_3d import plot_2d_3d
from plot_exp_elmdock import exp_plot 
from plot_vec import vec_plot
from plot_iso import iso_plot
from plot_axi import axi_plot
from plot_ani import ani_plot
from plot_diso import diso_plot
from plot_daxi import daxi_plot
from plot_dani import dani_plot
from plot_chi2 import chi2_plot
from save_elm import save_elm
from save_dock_elmdock import save_dock
from indicator_chain import dock
from StringIO import StringIO

if __name__ == "__main__":
    color_list=[ '#1f77b4','#ff7f0e','#2ca02c','#d62728', '#9467bd',\
'#8c564b', '#e377c2', '#7f7f7f', '#bcbd22','#17becf']
    json_variables = " " 
    InitialDirectoryStr = os.path.abspath(os.path.dirname(sys.argv[0]))
    argv_io_string = StringIO(sys.argv[1])
    json_variables = json.load(argv_io_string)
    relax_location = json_variables['relax_location']
    relax_loc = relax_location[0]
    to_dock = dock(relax_loc)
    output_res, base_dir, dyna_flag, elm_flag, elmdock_flag = rotdif(to_dock)
    exp_keys = exp_plot(color_list, dyna_flag, elm_flag, elmdock_flag,relax_loc)[1]
    outputmd_arr_files = []
    log_out = [str(base_dir) + "/" + 'elmdock_log.out']
    if dyna_flag == True:
        output_res[ 'diso_plot' ] = diso_plot(color_list, exp_keys, to_dock, dyna_flag, elm_flag, elmdock_flag)
        output_res[ 'daxi_plot' ] = daxi_plot(color_list, exp_keys, to_dock, dyna_flag, elm_flag, elmdock_flag)
        output_res[ 'dani_plot' ] = dani_plot(color_list,exp_keys, to_dock, dyna_flag, elm_flag, elmdock_flag)
 
    if elm_flag == True:
        save_elm(dyna_flag, elm_flag, elmdock_flag)
        elm_out = [str(base_dir) + "/" + 'ELM_prediction']
        output_res[ 'elm_out' ] = elm_out    

    if elmdock_flag == True:
        save_dock(dyna_flag, elm_flag, elmdock_flag)
        dock_out = [str(base_dir) + "/" + 'ELMDOCK']     
        output_res[ 'elmdock_out' ] = dock_out
        dock_pdb = [str(base_dir) + "/" + 'out_dock.pdb']
        output_res[ 'pdb'] = dock_pdb
        view_pdb = str(base_dir) + "/" + 'out_dock.pdb'
        output_res['outputpdb'] = { "file" : view_pdb, "script" : "ribbon" }
        
        #user_pdb = [str(base_dir)+"/"+'diUb_AB.pdb']
    output_res[ 'outputrotdif' ] = log_out
    print (json.dumps(output_res))
