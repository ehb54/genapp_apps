<?php
header('Content-type: application/json');

session_start(); 
$results[ 'error' ] = "";
$results[ '_status' ] = 'complete';

$window = "";
if ( isset( $_REQUEST[ '_window' ] ) )
{
   $window = $_REQUEST[ '_window' ];
}
if ( !isset( $_SESSION[ $window ] ) )
{
   $_SESSION[ $window ] = array( "logon" => "", "project" => "" );
}

date_default_timezone_set( 'UTC' );
$now = new MongoDate();

if ( !isset( $_SESSION[ $window ][ 'logon' ] ) ||
     !isset( $_REQUEST[ '_logon' ] ) )
{
    $results[ '_logon' ] = "";
    $results[ 'error' ] .= "Not logged in. ";
    echo (json_encode($results));
    exit();
}

$appconfig = json_decode( file_get_contents( "/share/apps/genapp/willitfit/appconfig.json" ) );

if ( $_REQUEST[ '_logon' ] != $_SESSION[ $window ][ 'logon' ] )
{
   $savelogon = $_SESSION[ $window ][ 'logon' ];
   unset( $_SESSION[ $window ][ 'logon' ] );
   $results[ '_logon' ] = "";
   $results[ 'error'  ] = 'Possible security violation user mismatch. ';
   try {
      $m = new MongoClient();
   } catch ( Exception $e ) {
      $results[ 'error' ] .= "Could not connect to the db " . $e->getMessage();
      exit();
   }

   $coll = $m->willitfit->security;
   $insert[ 'requestuser' ] = $_REQUEST[ '_logon' ];
   $insert[ 'sessionuser' ] = $savelogon;
   $insert[ 'remoteip'    ] = isset( $_SERVER[ 'REMOTE_ADDR' ] ) ? $_SERVER[ 'REMOTE_ADDR' ] : "not from an ip";
   $insert[ 'when'        ] = $now;

   try {
      $coll->insert( $insert, array("j" => true ) );
   } catch(MongoCursorException $e) {
      $results[ 'error' ] .= "Error updating the database. " . $e->getMessage() ;
      exit();
   }

   require_once "../mail.php";
   // $json = json_decode( file_get_contents( "/share/apps/genapp/willitfit/appconfig.json" ) );

   mymail( $appconfig->mail->admin, 'security alert willitfit', "session timeout or possible security breach attempt on willitfit\n" .
           'requestuser: ' . $insert[ 'requestuser' ] . "\n" .
           'sessionuser: ' . $insert[ 'sessionuser' ] . "\n" .
           'remoteip:    ' . $insert[ 'remoteip' ] . "\n" .
           'when:        ' . date('Y-m-d H:i:s', $insert[ 'when' ]->sec) . " UTC\n" .
           '' );
   echo (json_encode($results));
   exit();
}

if ( !sizeof( $_REQUEST ) )
{
    $results[ 'error' ] = "PHP code received no \$_REQUEST?";
    echo (json_encode($results));
    exit();
}

try {
     $m = new MongoClient();
} catch ( Exception $e ) {
    $results[ 'error' ] = "Could not connect to the db " . $e->getMessage();
    echo (json_encode($results));
    exit();
}

$coll = $m->willitfit->users;

$results[ 'status' ] = "";

$do_update = 0;
if ( $doc = $coll->findOne( array( "name" => $_SESSION[ $window ][ 'logon' ] ) ) )
{
   if ( isset( $_REQUEST[ 'newproject' ] ) &&
        $_REQUEST[ 'newproject' ] == "on" )
   {
      if ( !preg_match( '/^[a-zA-Z0-9_]+$/', $_REQUEST[ 'newprojectname' ] ) )
      {
          $results[ "error" ] .= "Invalid new project name.  It must contain only letters, numbers and underscores";
      } else {
          $update[ '$push' ][ 'project' ] = array( 
                                                 $_REQUEST[ 'newprojectname' ] => array( 
                                                                                       'desc'    => $_REQUEST[ 'newprojectdesc' ], 
                                                                                       'created' => $now 
                                                                                       ) 
                                                 );
          $results[ 'status' ] .= "Adding project " . $_REQUEST[ 'newprojectname' ] . ". ";
          $results[ '_project' ] = $_REQUEST[ 'newprojectname' ];
          $results[ 'status' ] .= "Current project is now " . $results[ '_project' ] . ". ";
          $_SESSION[ $window ][ 'project' ] = $results[ '_project' ];
          $do_update = 1;
      }
   } else {
      if ( ( isset( $_REQUEST[ 'project' ] ) &&
            ( !isset( $_REQUEST[ '_project' ] ) ||
               $_REQUEST[ '_project' ] != $_REQUEST[ 'project' ] ) ) ||
           ( !isset( $_REQUEST[ 'project' ] ) && isset( $_REQUEST[ '_project' ] ) ) )
      {
         $results[ '_project' ] = $_REQUEST[ 'project' ];
         $results[ 'status' ] .= "Current project is now " . $results[ '_project' ] . ". ";
         $_SESSION[ $window ][ 'project' ] = $results[ '_project' ];
      }
   }

   if ( isset( $_REQUEST[ 'visible' ] ) &&
        $_REQUEST[ 'visible' ] == 'on' &&
        ( !isset( $doc[ 'visible' ] ) ||
          $doc[ 'visible' ] != 'on' ) )
   {
       $update[ '$set' ][ 'visible' ] = 'on';
       $results[ 'status' ] .= "Your user is now visible to other logged in users. ";
       $do_update = 1;
   }

   if ( ( !isset( $_REQUEST[ 'visible' ] ) ||
          $_REQUEST[ 'visible' ] != 'on' ) &&
        isset( $doc[ 'visible' ] ) &&
        $doc[ 'visible' ] == 'on' )
   {
       $update[ '$unset' ][ 'visible' ] = 1;
       $results[ 'status' ] .= "Your user is NOT visible to other logged in users. ";
       $do_update = 1;
   }

   if ( isset( $_REQUEST[ 'changepassword' ] ) &&
        $_REQUEST[ 'changepassword' ] == "on" )
   {
      $ok_to_update = 0;
      $pw = $_REQUEST[ 'password' ];

      if ( PHP_VERSION_ID < 50500 )
      {
         if ( crypt( $pw, $doc[ 'password' ]) == $doc[ 'password' ] )
         {
            $ok_to_update = 1;
         }
      } else {
         if ( password_verify ( $pw , $doc[ 'password' ] ) )
         {  
            $ok_to_update = 1;
         }
      }
      if ( $ok_to_update != 1 )
      {
         $results[ "error" ] .= "Current password incorrect. ";
      }

      if ( !is_string( $_REQUEST[ 'password1' ] ) || strlen( $_REQUEST[ 'password1' ] ) < 10 || strlen( $_REQUEST[ 'password1' ] ) > 100 )
      {
         $results[ "error" ] .= "Empty or invalid new password. ";
         $ok_to_update = 0;
      }

      if( $_REQUEST[ 'password1' ] != $_REQUEST[ 'password2' ] )
      {
         $results[ 'error' ] .= "Passwords do not match. ";
         $ok_to_update = 0;
      }

      if ( $ok_to_update == 1 )
      {
         if ( PHP_VERSION_ID < 50500 )
         {
            $pw = crypt( $_REQUEST[ 'password1' ] );
         } else {
            $pw = password_hash( $_REQUEST[ 'password1' ], PASSWORD_DEFAULT );
         }

         $update[ '$set' ][ 'password' ]           = $pw;
         $update[ '$set' ][ 'passwordchangewhen' ] = $now;
         $update[ '$set' ][ 'passwordchangeip'   ] = isset( $_SERVER[ 'REMOTE_ADDR' ] ) ? $_SERVER[ 'REMOTE_ADDR' ] : "not from an ip";

         $update[ '$unset' ][ 'lastfailedloginattempts' ] = 0;
         $update[ '$unset' ][ 'expiretime'              ] = 0;
         $update[ '$unset' ][ 'expiretimes'             ] = 0;

         require_once "../mail.php";
         date_default_timezone_set( 'UTC' );
         // $json = json_decode( file_get_contents( "/share/apps/genapp/willitfit/appconfig.json" ) );

         mymail( $doc[ 'email' ] , 'password change notice willitfit', "Your password has been changed\n" .
                 'remoteip:    ' . $update[ '$set' ][ 'passwordchangeip' ] . "\n" .
                 'when:        ' . date('Y-m-d H:i:s', $update[ '$set' ][ 'passwordchangewhen' ]->sec) . " UTC\n" .
                 "\n" . 'If you do not recognize this change please forward this email with a comment to ' . $appconfig->mail->admin . "\n" .
                 '' );

         $results[ 'status' ] .= "Changing password. ";
         $do_update = 1;
      }
   }

   if ( isset( $_REQUEST[ 'changeemail' ] ) &&
        $_REQUEST[ 'changeemail' ] == "on" )
   {
      $ok_to_update = 0;

      $email1 = filter_var( $_REQUEST[ 'email1' ], FILTER_SANITIZE_EMAIL );
      $email2 = filter_var( $_REQUEST[ 'email2' ], FILTER_SANITIZE_EMAIL );

      if ( $email1 == $email2 )
      {
         if ( $email1 == $doc[ 'email' ] )
         {
             $results[ 'error' ] .= "Email address change request but it is not changed. ";
         } else {
            $ok_to_update = 1;
         }
      } else {
         $results[ 'error' ] .= "Email addresses do not match. ";
      }

      if ( $ok_to_update == 1 )
      {
         $update[ '$set' ][ 'email' ]           = $email1;
         $update[ '$set' ][ 'emailchangewhen' ] = $now;
         $update[ '$set' ][ 'emailchangeip'   ] = $_SERVER[ 'REMOTE_ADDR' ];

         $update[ '$push' ][ 'previousemail'  ] = $doc[ 'email' ];

         require_once "../mail.php";
         date_default_timezone_set( 'UTC' );
         // $json = json_decode( file_get_contents( "/share/apps/genapp/willitfit/appconfig.json" ) );

         mymail( $doc[ 'email' ] , 'email change notice willitfit', "Your email address has been changed\n" .
                 'new email:   ' . $email1 . "\n" .
                 'remoteip:    ' . $update[ '$set' ][ 'emailchangeip' ] . "\n" .
                 'when:        ' . date('Y-m-d H:i:s', $update[ '$set' ][ 'emailchangewhen' ]->sec) . " UTC\n" .
                 "\n" . 'If you do not recognize this change please forward this email with a comment to ' . $appconfig->mail->admin . "\n" .
                 '' );

         admin_mail( "[willitfit][new email address] $email1", "User: " . $_REQUEST[ '_logon' ] . "\nEmail: $email1\n" );

         $results[ 'status' ] .= "Changing email address to ${email1}. ";
         $do_update = 1;
      }
   }

   // group membership, check appconfig for groups, then compare with user info and request to add, change or delete groups
   
   if ( isset( $appconfig->groups ) ) {
       // make array of valid user groups and those requested
       $user_groups           = [];
       $user_groups_requested = [];
       $set_groups            = [];

       
       foreach ( $appconfig->groups as $k => $v ) {
           
           if ( isset( $v->userconfig ) && $v->userconfig ) {
               
               $user_groups[ $k ] = "_setgroup_groups_$k";
               if ( isset( $_REQUEST[ $user_groups[ $k ] ] ) ) {
                   $user_groups_requested[ $k ] = 1;
                   $set_groups[]                = $k;
               }
           }
       }
       sort( $set_groups );

       // check doc for groups
       $user_current_groups = [];

       $org_groups = [];

       if ( isset( $doc[ 'groups' ] ) ) {
           
           // loop thru and push to $set_groups
           foreach ( $doc[ 'groups' ] as $k => $v ) {
               $org_groups[] = $v;
               
           }
       }

       sort( $org_groups );

       
       
       
       
       

       if ( $set_groups != $org_groups ) {
           
           $update[ '$set' ][ 'groups' ] = $set_groups;
           $results[ 'status' ] .= "Updating group membership. ";
           $results[ '_usergroups' ] = $set_groups;
           $do_update = 1;
       } else {
           
       }
   }

   if ( $do_update )
   {
      try {
        $coll->update( array( "name" => $_SESSION[ $window ][ 'logon' ] ), 
                       $update, array("j" => true )  );
      } catch(MongoCursorException $e) {
         $results[ 'error' ]  .= "Error updating the database. " . $e->getMessage();
         $results[ 'status' ] .= "Unable to update user record. ";
         echo (json_encode($results));
         exit();
      }
      $results[ 'status' ] .= "Update ok.";
   } else {
      if ( strlen( $results[ 'status' ] ) == 0 )
      {
         $results[ 'status' ] .= "Nothing to update.";
      }
   }
} 

if ( strlen( trim( $results[ 'error' ] ) ) == 0 )
{
   unset( $results[ 'error' ] );
}

echo (json_encode($results));
exit();
?>
