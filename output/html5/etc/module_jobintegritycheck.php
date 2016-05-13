<?php

if ( !isset( $GLOBALS[ "modulejson" ] ) || !is_array( $GLOBALS[ "modulejson" ] ) ) {
   $GLOBALS[ "modulejson" ] = [];
}

$GLOBALS[ "modulejson" ][ "jobintegritycheck" ] = json_decode( '{"uniquedir":"on","label":"Jobintegritycheck","resource":"local","moduleid":"jobintegritycheck","executable":"jobintegritycheck","submitpolicy":"all","fields":[{"help":"Correct fixable errors","label":"Fix errors","type":"checkbox","id":"fixerrors","role":"input"},{"id":"jobintegrityreport","type":"html","label":"","role":"output"}],"docrootexecutable":"ajax/sys_config/sys_jobintegritycheck.php"}' );
?>
