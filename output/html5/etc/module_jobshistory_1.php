<?php

if ( !isset( $GLOBALS[ "modulejson" ] ) || !is_array( $GLOBALS[ "modulejson" ] ) ) {
   $GLOBALS[ "modulejson" ] = [];
}

$GLOBALS[ "modulejson" ][ "jobshistory_1" ] = json_decode( '{"executable":"jobshistory_1","docrootexecutable":"util/jobs_history_web.php","fields":[{"required":"true","role":"input","id":"input1","type":"date","label":"Start Date"},{"role":"input","label":"End Date","type":"date","id":"input2","required":"true"},{"id":"jobshisreport","type":"html","label":"","role":"output"}],"submitpolicy":"all","uniquedir":"on","moduleid":"jobshistory_1","resource":"local","label":"JobsHistory_1"}' );
?>
