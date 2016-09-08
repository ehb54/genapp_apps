echo do not run this
exit

# useful commands to use on your instance
nova reboot ${OS_PROJECT_NAME}-api-U-1
nova suspend ${OS_PROJECT_NAME}-api-U-1
nova stop ${OS_PROJECT_NAME}-api-U-1

# when you are done with an instance
# delete the instance
nove delete ${OS_PROJECT_NAME}-api-U-1

# disassociate the IP address from the instance
nova floating-ip-disassociate ${OS_PROJECT_NAME}-api-U-1 149.165.170.87

# disconnect the router from the gateway
neutron router-gateway-clear ${OS_PROJECT_NAME}-api-router

# delete the subnet from the router
neutron router-interface-delete ${OS_PROJECT_NAME}-api-router ${OS_PROJECT_NAME}-api-subnet1

# delete the router
neutron router-delete ${OS_PROJECT_NAME}-api-router

# these commands do not have an equivalent GUI operation and can only be performed
# via the command line clients.  When using the GUI, security groups must be associated
# with an instance when it is created and/or booted.
nova add-secgroup ${OS_PROJECT_NAME}-api-U-1 global-ssh
nova remove-secgroup ${OS_PROJECT_NAME}-api-U-1 global-ssh
