<?php

if ( !isset( $GLOBALS[ "modulejson" ] ) || !is_array( $GLOBALS[ "modulejson" ] ) ) {
   $GLOBALS[ "modulejson" ] = [];
}

$GLOBALS[ "modulejson" ][ "sys_file_manager" ] = json_decode( '{"fields":[{"id":"label1","posthline":"true","default":"header3","label":"Files","type":"label","role":"input"},{"id":"serverdate","type":"label","label":"Server date","role":"input","pull":"datetime","help":"This is the date and time from the file system on the server at the time you opened this window"},{"role":"input","id":"selectedfiles","type":"ftree","label":"User file tree"},{"values":"none~no~uncompressed tarball~tar~gzip tarball~gz~bzip2 tarball~bz2~xz tarball~xz~zipped~zip","default":"tar","type":"listbox","name":"compression","role":"input","id":"compression","label":"Compression type","help":"You can select the compression type here.  Select <b>none</b> for a list of individual file links"},{"multiple":"true","role":"output","id":"status","label":"","type":"html"},{"type":"file","label":"","id":"outfiles","multiple":"true","role":"output"}],"docrootexecutable":"ajax/sys_config/sys_file_manager_run.php","nofcrefresh":"true","executable":"sys_file_manager","label":"Files Manager","resource":"local","submit_label":"Download","moduleid":"sys_file_manager","nojobcontrol":"true","uniquedir":"true","noreset":"true","resetonload":"true","height":"65vh"}' );
?>
