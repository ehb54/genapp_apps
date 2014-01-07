<?php
header('Content-type: application/json');
$cmd = "/src/sassie2/bin/interact_2";
if ( !is_executable( $cmd ) )
{
    $responseVar = array("error" => "command not found or not executable" );
    echo (json_encode($responseVar));
    exit();
}

if ( sizeof( $_FILES ) )
{
# setup session
   do {
      $dir = uniqid( "/tmp/php/" );
   } while( file_exists( $dir ) );
   if ( !mkdir( $dir, 0777, true ) )
   {  
      $responseVar = array("error" => "could not create directory " . $dir );
      echo (json_encode($responseVar));
      exit();
   }
   foreach ( $_FILES as $k=>$v )
   {
      if ( is_array( $v[ 'error' ] ) )
      {
         foreach ( $v[ 'error' ] as $k1=>$v1 )
         {
            if ( $v[ 'error' ][ $k1 ] )
            {
               if ( is_string( $v[ 'name' ][ $k1 ] ) && !strlen( $v[ 'name' ][ $k1 ] ) )
               {
                  $responseVar = array("error" => "missing file input for identifier " . $k );
               } else {
                  $responseVar = array("error" => "error uploading file " . $v[ 'name' ][ $k1 ] . " (possibly too large and, if so, php.ini and/or httpd.conf could be adjusted on the server)" );
               }
               echo (json_encode($responseVar));
               exit();
            }
            error_log( "move_uploaded_file( " . $v[ 'tmp_name' ][ $k1 ] . ',' .  $dir . '/' . $v[ 'name' ][ $k1 ] . "\n", 3, "/var/tmp/my-errors.log");
            if ( !move_uploaded_file( $v[ 'tmp_name' ][ $k1 ], $dir . '/' . $v[ 'name' ][ $k1 ] ) )
            {
               $responseVar = array("error" => "could not move file " . $v[ 'name' ][ $k1 ] );
               echo (json_encode($responseVar));
               exit();
            }
            $_REQUEST[ $k ][] = $dir . '/' . $v[ 'name' ][ $k1 ];
         }
      } else {
         if ( $v[ 'error' ] )
         {
            if ( is_string( $v[ 'name' ] ) && !strlen( $v[ 'name' ] ) )
            {
               $responseVar = array("error" => "missing file input for identifier " . $k );
            } else {
               $responseVar = array("error" => "error uploading file " . $v[ 'name' ] . " (possibly too large and, if so, php.ini and/or httpd.conf could be adjusted on the server)" );
            }
            echo (json_encode($responseVar));
            exit();
         }
         error_log( "move_uploaded_file( " . $v[ 'tmp_name' ] . ',' .  $dir . '/' . $v[ 'name' ] . "\n", 3, "/var/tmp/my-errors.log");
         if ( !move_uploaded_file( $v[ 'tmp_name' ], $dir . '/' . $v[ 'name' ] ) )
         {
            $responseVar = array("error" => "could not move file " . $v[ 'name' ] );
            echo (json_encode($responseVar));
            exit();
         }
         $_REQUEST[ $k ][] = $dir . '/' . $v[ 'name' ];
      }
   }
}

$json = json_encode( $_REQUEST );
$json = str_replace( "'", "_", $json );
$cmd .= " '$json'";

$results = exec( $cmd );

// cleanup CURRENTLY DISABLED!
if ( sizeof( $_FILES ) )
{
   $files = new RecursiveIteratorIterator(
       new RecursiveDirectoryIterator($dir, RecursiveDirectoryIterator::SKIP_DOTS),
       RecursiveIteratorIterator::CHILD_FIRST
   );

   foreach ($files as $fileinfo) {
      $todo = ($fileinfo->isDir() ? 'rmdir' : 'unlink');
//      $todo( $fileinfo->getRealPath() );
   }
//   rmdir( $dir );
}
echo $results;
?>
