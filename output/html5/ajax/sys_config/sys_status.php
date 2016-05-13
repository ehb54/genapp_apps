<?php
header('Content-type: application/json');
session_start(); 
$window = "";
if ( isset( $_REQUEST[ '_window' ] ) )
{
   $window = $_REQUEST[ '_window' ];
}

if ( !isset( $_SESSION[ $window ] ) )
{
   $_SESSION[ $window ] = array( "logon" => "", "project" => "" );
}

if ( isset( $_SESSION[ $window ][ 'project' ] ) )
{
  $results[ '_project' ] = $_SESSION[ $window ][ 'project' ];
} else {
  $results[ '_project' ] = "";
}
if ( isset( $_SESSION[ $window ][ 'logon' ] ) )
{ 
  $results[ '_logon' ] = $_SESSION[ $window ][ 'logon' ];
  if ( isset( $_REQUEST[ "_groups" ] ) ) {
      $app_json = json_decode( file_get_contents( "/share/apps/genapp/willitfit/appconfig.json" ) );
      if ( isset( $app_json->groups ) ) {
          $results[ "_groups" ] = $app_json->groups;
      } else {
          $results[ "_groups" ] = new stdClass();
      }
      $mongook = 1;
      try {
          $m = new MongoClient();
      } catch ( Exception $e ) {
          $results[ 'error' ] .= "Could not connect to the db " . $e->getMessage();
          $mongook = 0;
      }
      if ( $mongook ) {
          $coll = $m->willitfit->users;
          if ( $doc = $coll->findOne( array( "name" => $_SESSION[ $window ][ 'logon' ] ), array( "groups" => 1 ) ) ) {
              $results[ "_usergroups" ] = $doc[ "groups" ];
          } else {
              $results[ "_usergroups" ] = [];
          }
      }
  }
} else {
  $results[ '_logon' ] = "";
  $results[ '_project' ] = "";
}


echo (json_encode($results));
exit();
?>
