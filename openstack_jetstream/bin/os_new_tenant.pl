#!/usr/bin/perl

$notes = 
"usage          : $0 flavor image securitygroups ip name

flavor         : m1.tiny m1.small ...
image          : id (try $ glance image-list) [ atmosphere ubuntu 14.04 currently is d1126e09-9e87-4f88-914f-fa42ba256c68 ]
securitygroups : global-ssh global-http global-https
ip             : ip defined in dashboard
name           : will be prepended with \${OS_PROJECT_NAME}-api-
";

$OS_PROJECT_NAME = $ENV{ 'OS_PROJECT_NAME' } || die "no OS_PROJECT_NAME, did you source openrc.sh?\n";
die "OS_PROJECT_NAME is empty, check your openrc.sh and resource\n" if !length( $OS_PROJECT_NAME );

$flavor    = shift || die $notes;
$image     = shift || die $notes;
$secgroups = shift || die $notes;
$ip        = shift || die $notes;
$name      = shift || die $notes;

# Things you'll probably do once per running instance

$uname = "${OS_PROJECT_NAME}-api-$name";

$cmd = 
"
# create and boot an instance

nova boot $uname \\
  --flavor $flavor \\
  --image $image \\
  --key-name i73 \\
  --security-groups $secgroups \\
  --nic net-name=${OS_PROJECT_NAME}-api

# create an IP address for that instance
nova floating-ip-create public

# associate that IP address with that instance
nova floating-ip-associate $uname $ip
"
;

print $cmd;


