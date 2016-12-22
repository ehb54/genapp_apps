<?php

date_default_timezone_set("UTC");

# $testing = 1;

function post_processing( $json, $results ) {

    print "post_processing called\n";
    
    if ( isset( $results->error ) ) {
        $cmd = 'echo "'. addslashes( json_encode( $results ) ) . '" | mail -s "[namdrunner][performance results][ERROR]" emre@biochem.uthscsa.edu';
        `$cmd`;
        return;
    }

    # check for namdmd_log.out

    $runlogfile = "$json->_base_directory/namdmd_log.out";
    
    # print "runlog $runlogfile\n";

    $log = "namd_perf.txt";

    if ( FALSE === ( $runlog = file_get_contents( $runlogfile ) ) ) {
        fwrite( STDERR, "error reading $runlogfile\n" );
        file_put_contents( $log, date( DATE_ATOM ) . " error reading $runlogfile\n", FILE_APPEND );
        return;
    }

    $log_lines = preg_split( "/\\r\\n|\\r|\\n/", $runlog );

    $log_benchmark = preg_replace( '/^/', date( DATE_ATOM ) . ' ',  preg_grep( '/(benchmark|TIMING:)/i', $log_lines ) );

    if ( $GLOBALS[ 'do_perf' ] ) {
        $perffile = "$json->_base_directory/perftest.out";
        if ( FALSE === ( $perflog = file_get_contents( $perffile ) ) ) {
            fwrite( STDERR, "error reading $perffile\n" );
            file_put_contents( $log, date( DATE_ATOM ) . " error reading $perffile\n", FILE_APPEND );
        } else {
            $perf_lines = preg_split( "/\\r\\n|\\r|\\n/", $perflog );
            $log_perf = preg_replace( '/^/', date( DATE_ATOM ) . ' ',  $perf_lines );
            file_put_contents( $log, implode( "\n", $log_perf ) . "\n",  FILE_APPEND );
        }
    }            

    file_put_contents( $log, implode( "\n", $log_benchmark ) . "\n",  FILE_APPEND );

    $cmd = "tail -40 $log |" . ' mail -s "[namdrunner][performance results]" emre@biochem.uthscsa.edu';
    `$cmd`;
    return;
}

if ( isset( $testing ) ) {
    if ( isset( $argv[ 1 ] ) ) {
        $oscmd_file = $argv[ 1 ];
    } else {
        echo "needs arg\n";
        exit;
    }

    if ( FALSE === ( $oscmd = file_get_contents( $oscmd_file ) ) ) {
        echo "could not read file $oscmd_file\n";
        exit;
    }

    preg_match( 
        '/^ssh \S+ "(?:cd \S+; (\S+)) \'({.*})\'" (2> \S+)$/'
        , $oscmd
        , $matches
        );
#print_r( $matches );

    $appId        = $matches[ 1 ];
    $json_encoded = stripslashes( $matches[ 2 ] );
    $outredirect  = $matches[ 3 ];

# echo print_r( $json_encoded, true ) . "\n";

    $json = json_decode( $json_encoded );
    if ( $json === FALSE || $json === NULL ) {
        echo "error decoding json from $oscmd_file\n";
        exit;
    }

    post_processing( $json , null );
}
        

?>
