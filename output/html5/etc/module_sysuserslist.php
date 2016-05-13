<?php

if ( !isset( $GLOBALS[ "modulejson" ] ) || !is_array( $GLOBALS[ "modulejson" ] ) ) {
   $GLOBALS[ "modulejson" ] = [];
}

$GLOBALS[ "modulejson" ][ "sysuserslist" ] = json_decode( '{"moduleid":"sysuserslist","label":"Sysuserslist","resource":"local","uniquedir":"on","submitpolicy":"all","docrootexecutable":"ajax/sys_config/sys_userslist.php","fields":[{"role":"output","id":"sysuserreport","label":"","type":"html"}],"executable":"sysuserslist"}' );
?>
