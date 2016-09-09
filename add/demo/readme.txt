How to use:

NAMDrunner requires 4 files:

1) Coordinate File: 
   - in min/ folder this is a step2_solvator.pdb
   - in md/  folder this is a namd_out_whole_min.coor

(extension can be anything, but the file must be in .pdb format)

2) PSF file: 
   Protein Structure File in .xplor format (a format understood to NAMD)
   - step2_solvator.xplor.psf in min/ and md/ folders

3) Parameter File:
   a single .zip archive which reflect the structure of the toppar directory used in Input NAMD Script
   E.g., if the "parameters" section in the input script looks like 
    	 -----------------
	 parameters  toppar/par_all36_prot_for_namd.prm
	 parameters  toppar/toppar_water_ions_for_namd.str
	 -----------------
   than a .zip parameter file should have the following structure:
   	
  	toppar/
        toppar/par_all36_prot_for_namd.prm
        toppar/toppar_water_ions_for_namd.str

4) Input NAMD script:
   Samples of minimization (namd_prot_min.inp) and MD run (namd_prot_md.inp) input scripts are, respectively, in
   min/ and md/ folders
	
	 

