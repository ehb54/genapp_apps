echo do not run this
exit

# steps that need to be done only once
# create a security group that will enable inbound ping & ssh
nova secgroup-create global-ssh "ssh & icmp enabled"
nova secgroup-add-rule global-ssh tcp 22 22 0.0.0.0/0
nova secgroup-add-rule global-ssh icmp -1 -1 0.0.0.0/0

# upload an ssh key if you already have one...
# assuming you want to upload id_rsa & id_rsa.pub
cd ~/.ssh
nova keypair-add --pub-key id_rsa.pub id_rsa

# ...otherwise, create a new key if you prefer...
cd ~/.ssh
ssh-keygen -b 2048 -t rsa -f ${OS_PROJECT_NAME}-api-key -P ""
# and upload the key to nova
nova keypair-add --pub-key ${OS_PROJECT_NAME}-api-key.pub ${OS_PROJECT_NAME}-api-key

# start building your OpenStack cyberinfrastructure

# create your personal network
neutron net-create ${OS_PROJECT_NAME}-api

# create a subnet within your personal network space
neutron subnet-create ${OS_PROJECT_NAME}-api 10.0.0.0/24 --name ${OS_PROJECT_NAME}-api-subnet1

# create a router
neutron router-create ${OS_PROJECT_NAME}-api-router

# connect your subnet to the router
neutron router-interface-add ${OS_PROJECT_NAME}-api-router ${OS_PROJECT_NAME}-api-subnet1    # names instead of UUIDs

# connect your router to the gateway named "public"
neutron router-gateway-set ${OS_PROJECT_NAME}-api-router public                    # not sure public will work

# end of things you'll probably only do once

### - stuff i did -

   34  nova secgroup-create global-ssh "ssh & icmp enabled"

   35  nova secgroup-add-rule global-ssh tcp 22 22 0.0.0.0/0
   36  nova secgroup-add-rule global-ssh icmp -1 -1 0.0.0.0/0

   37  nova secgroup-create global_http "http+ssh+icmp"

   40  nova secgroup-add-rule global_http tcp 22 22 0.0.0.0/0
   42  nova secgroup-add-rule global_http icmp -1 -1 0.0.0.0/0
   44  nova secgroup-add-rule global_http tcp 80 80 0.0.0.0/0

   38  nova secgroup-create global_https "https+ssh+icmp"

   41  nova secgroup-add-rule global_https tcp 22 22 0.0.0.0/0
   43  nova secgroup-add-rule global_https icmp -1 -1 0.0.0.0/0
   45  nova secgroup-add-rule global_https tcp 80 80 0.0.0.0/0
   46  nova secgroup-add-rule global_https tcp 443 443 0.0.0.0/0

   50  nova secgroup-list-rules global-ssh
   51  nova secgroup-list-rules global_http
   52  nova secgroup-list-rules global_https

   53  neutron security-group-list
