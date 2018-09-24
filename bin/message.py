#!/usr/bin/python
import json, sys, StringIO, socket, time

def message_box(text,icon):
        _message = {}
        _message['icon'] = icon
        _message['text'] = text
        
        UDP_IP = json_variables['_udphost']
        UDP_PORT = json_variables['_udpport']
        sock = socket.socket(socket.AF_INET, # Internet
        socket.SOCK_DGRAM) # UDP
        socket_dict = {}
        socket_dict['_uuid'] = json_variables['_uuid']
        socket_dict['_message'] = _message
        doc_string = json.dumps(socket_dict)
        sock.sendto(doc_string,(UDP_IP,UDP_PORT))
        
        return

if __name__=='__main__':

	argv_io_string = StringIO.StringIO(sys.argv[1])
	json_variables = json.load(argv_io_string)

	output = {} 

        message_box( "Here is my message" , 'toast.png' )

        time.sleep( 3 )
        
        output[ 'result' ] = "message sent 3 seconds ago"

        output['_textarea'] = "JSON output from executable:\n" + json.dumps( output, indent=4 ) + "\n\n";
        output['_textarea'] += "JSON input to executable:\n" + json.dumps( json_variables, indent=4 ) + "\n";

	print json.dumps(output)
		
