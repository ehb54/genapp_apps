import glob
import os 
import sys

python = sys.executable

for name in glob.glob('bin/*'):
    if os.path.isfile(name):
        with open(name) as input_file:
            lines = input_file.readlines()

        lines[0] = '#!'+python+'\n'

        with open(name, 'w') as output_file:
            for line in lines:
                output_file.write(line)

