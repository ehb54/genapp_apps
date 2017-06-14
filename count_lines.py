import os, string
cfiles = []

sassie_web_bin_exclude = ['junk.out', 'run_0', 'run_2', 'debug_sct_analyze', 'bak.sct_analyze', 'input_and_testing_files', 'min3.pdb', 'old_sascalc', 'p20a.mrc', 'sct_analyze.old', 'temp.inp']

for root, dirs, files in os.walk('bin'):
	for file in files:
		#if file.endswith('.c'):
		if file not in sassie_web_bin_exclude:
			if root not in sassie_web_bin_exclude:	
				cfiles.append(os.path.join(root, file))

for root, dirs, files in os.walk('modules'):
	for file in files:
		#if file.endswith('.c'):
		if file not in sassie_web_bin_exclude:
			if root not in sassie_web_bin_exclude:	
				cfiles.append(os.path.join(root, file))

lines = 0

exclude_list = ["txt", "dcd", "log", "iq", "pdb", "ans", "stats", "psf", "in", "data", "mc", "swp"]

for file in cfiles:
	output = os.popen('wc ' + file ).readlines()
	empty_output = os.popen("grep -c '^$' " + file).readlines()
#	print output[0]
	this_line = string.split(output[0])
#	print 'this_line = ', this_line
#	print output[0]
	if string.split(this_line[-1],'.')[-1] not in exclude_list:
		print this_line
		print string.split(this_line[-1],'.')[-1]
		lines += int(this_line[0]) - int(empty_output[0])
#		print this_line[0]

print 'lines = ',lines
