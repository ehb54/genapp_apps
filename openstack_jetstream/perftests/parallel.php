<?php

# https://github.com/reactphp/child-process

use React\EventLoop\Factory;
use React\ChildProcess\Process;

require __DIR__ . '/../vendor/autoload.php';

function prll_init() {
    global $prll_loop;
    global $prll_table;
    global $prll_sout;
    global $prll_eout;

    $prll_loop = Factory::create();
    $prll_table = [];
    $prll_sout = [];
    $prll_eout = [];
    
    `mkdir prll 2> /dev/null`;
}

function prll_run() {
    global $prll_loop;
    global $prll_table;
    global $prll_sout;
    global $prll_eout;

    $prll_loop->run();

    foreach ( array_keys( $prll_table ) as $key ) {
        if ( file_exists( "prll/$key.stdout" ) ) {
            $prll_sout[ $key ] = file_get_contents( "prll/$key.stdout" );
            unlink( "prll/$key.stdout" );
        }
        if ( file_exists( "prll/$key.stderr" ) ) {
            $prll_eout[ $key ] = file_get_contents( "prll/$key.stderr" );
            unlink( "prll/$key.stderr" );
        }
    }
}

function prll_add( $key, $cmd ) {
    global $prll_loop;
    global $prll_table;
    global $prll_sout;
    global $prll_eout;

    if ( file_exists( "prll/$key.stdout" ) ) {
        unlink( "prll/$key.stdout" );
    }
    if ( file_exists( "prll/$key.stderr" ) ) {
        unlink( "prll/$key.stderr" );
    }

    $cmd .= " > prll/$key.stdout 2> prll/$key.stderr";

    $prll_table[ $key ] = new Process( $cmd );
    $prll_table[ $key ]->start( $prll_loop );

    $prll_sout[ $key ] = "";
    $prll_eout[ $key ] = "";


#    $prll_table[ $key ]->stdout->on('data', function ($chunk) {
#        echo "chunk is $chunk\n";
#                                    });

#    $prll_table[ $key ]->stderr->on('data', function ($chunk) {
#        echo "chunk is $chunk\n";
#                                    });

    $prll_table[ $key ]->on('exit', function ($code) {
        echo 'EXIT with code ' . $code . PHP_EOL;
                            });

}

prll_init();

prll_add( "ls", "ls -l" );
prll_add( "du", "du -h" );

prll_run();

foreach ( array_keys( $prll_table ) as $key ) {
    echo "stdout $key " . $prll_sout[ $key ] . "\n";
    echo "stderr $key " . $prll_eout[ $key ] . "\n";
}

?>
