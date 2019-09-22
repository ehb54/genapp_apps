#!/usr/bin/python
import json, sys, StringIO

if __name__=='__main__':

	argv_io_string = StringIO.StringIO(sys.argv[1])
	json_variables = json.load(argv_io_string)

	output = {} 

        if isinstance( json_variables, dict ) and \
           'm' in json_variables and \
           'c' in json_variables :
                try:
	                mass = float(json_variables['m'])
                except ValueError:
                        output['notice'] = "Expected JSON input variables 'm' and/or 'c' not found or incorrect type"
                try:
 	                speed_of_light = float(json_variables['c'])
                except ValueError:
                        output['notice'] = "Expected JSON input variables 'm' and/or 'c' not found or incorrect type"

	        import mass_energy
	
	        output['e'] = mass_energy.einstein(mass,speed_of_light)
        else:
                output['notice'] = "Expected JSON input variables 'm' and/or 'c' not found or incorrect type"

        output['_textarea'] = "JSON output from executable:\n" + json.dumps( output, indent=4 ) + "\n\n";
        output['_textarea'] += "JSON input to executable:\n" + json.dumps( json_variables, indent=4 ) + "\n";

	print json.dumps(output)
		
