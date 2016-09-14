<?php

$notes = 
    "\n" .
    "--------------------\n" .
    "\n" .
    "usage: $argv[0] number of nodes\n" .
    "starts nodes and returns ip addresses\n" .
    "\n";

$appconfig = "__appconfig__";
$appconfig = "appconfig.json";

if ( !isset( $argv[ 1 ] ) ) {
    echo $notes;
    exit;
}

if ( NULL == ( $json = json_decode( file_get_contents( $appconfig ) ) ) ) {
    echo "error: appconfig could not be decoded\n";
    exit;
}
   
if ( !$json->resources ) {
    echo "error: resources not defined in appconfig\n";
    exit;
}

if ( !$json->resources->oscluster ) {
    echo "error: resources:oscluster not defined in appconfig\n";
    exit;
}
   
if ( !$json->resources->oscluster->properties ) {
    echo "error: resources:oscluster:properties not defined in appconfig\n";
    exit;
}

// -------------------- set up OS env --------------------

if ( !$json->resources->oscluster->properties->domain ) {
    echo "error: resources:oscluster:properties:domain not defined in appconfig\n";
    exit;
}

putenv( "OS_TENANT_DOMAIN_NAME=" . $json->resources->oscluster->properties->domain );
putenv( "OS_PROJECT_DOMAIN_NAME=" . $json->resources->oscluster->properties->domain );
putenv( "OS_USER_DOMAIN_NAME=" . $json->resources->oscluster->properties->domain );

if ( !$json->resources->oscluster->properties->project ) {
    echo "error: resources:oscluster:properties:project not defined in appconfig\n";
    exit;
}

putenv( "OS_TENANT_NAME=" . $json->resources->oscluster->properties->project );
putenv( "OS_PROJECT_NAME=" . $json->resources->oscluster->properties->project );

$project = $json->resources->oscluster->properties->project;

if ( !$json->resources->oscluster->properties->user ) {
    echo "error: resources:oscluster:properties:user not defined in appconfig\n";
    exit;
}

putenv( "OS_USERNAME=" . $json->resources->oscluster->properties->user );

if ( !$json->resources->oscluster->properties->password ) {
    echo "error: resources:oscluster:properties:password not defined in appconfig\n";
    exit;
}

putenv( "OS_PASSWORD=" . $json->resources->oscluster->properties->password );

if ( !$json->resources->oscluster->properties->api_version ) {
    echo "error: resources:oscluster:properties:api_version not defined in appconfig\n";
    exit;
}

putenv( "OS_IDENTITY_API_VERSION=" . $json->resources->oscluster->properties->api_version );

if ( !$json->resources->oscluster->properties->auth_url ) {
    echo "error: resources:oscluster:properties:auth_url not defined in appconfig\n";
    exit;
}

putenv( "OS_AUTH_URL=" . $json->resources->oscluster->properties->auth_url );

// -------------------- set up OS image info --------------------

if ( !$json->resources->oscluster->properties->flavor ) {
    echo "error: resources:oscluster:properties:flavor not defined in appconfig\n";
    exit;
}

$flavor = $json->resources->oscluster->properties->flavor;

if ( !$json->resources->oscluster->properties->baseimage ) {
    echo "error: resources:oscluster:properties:baseimage not defined in appconfig\n";
    exit;
}

$baseimage = $json->resources->oscluster->properties->baseimage;

if ( !$json->resources->oscluster->properties->key ) {
    echo "error: resources:oscluster:properties:key not defined in appconfig\n";
    exit;
}

$key = $json->resources->oscluster->properties->key;

if ( !$json->resources->oscluster->properties->secgroup ) {
    echo "error: resources:oscluster:properties:secgroup not defined in appconfig\n";
    exit;
}

$secgroup = $json->resources->oscluster->properties->secgroup;

echo `nova list`;

$cstrong = true;

$images = [];

// -------------------- boot instances --------------------

for ( $i = 0; $i < $argv[ 1 ]; ++$i ) {
    $name =  
        $json->resources->oscluster->properties->project . 
        "-run-" . bin2hex( openssl_random_pseudo_bytes ( 16, $cstrong ) );

    $image[] = $name;
    $cmd = "nova boot $name --flavor $flavor --image $baseimage --key-name $key --security-groups $secgroup --nic net-name=${project}-api";
    print "$cmd\n";
    print `$cmd`;
}

// -------------------- wait to become active --------------------

$isactive = [];
$ip = [];


do {
    $any_booting = false;
    foreach ( $image as $v ) {
        if ( array_key_exists( $v, $isactive ) ) {
            continue;
        }
        echo "checking $v\n";
        $cmd = "nova show $v";
        $results = `$cmd`;
        $resultsarray = split( "\n", $results );
        $status = array_values( preg_grep( "/ status  /", $resultsarray ) );

        echo "status: " . json_encode( $status, JSON_PRETTY_PRINT ) . "\n";
        $network = array_values( preg_grep( "/ network  /", $resultsarray ) );
        echo "network: " . json_encode( $network, JSON_PRETTY_PRINT ) . "\n";

        if ( $network ) {
            $nets = preg_split( '/\s+/', $network[ 0 ] );
            foreach ( $nets as $k2 => $v2 ) {
                print "nets[$k2]=$v2\n";
            }
            $ip[ $v ] = $nets[ 4 ];
        }

        if ( $status &&
             strpos( $status[ 0 ], "ACTIVE" ) ) {
            $isactive[ $v ] = 1;
        } else {
            $any_booting = true;
            echo "$v still booting\n";
        }
    }
} while( $any_booting );

echo "all active\n";

// provision ... (?)


?>
