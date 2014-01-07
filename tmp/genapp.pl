#!/usr/bin/perl

$notes = "usage: $0
checks the files
note: env variable GENAPP must be defined
" ;

$gap = $ENV{ "GENAPP" } || die "$0: error env variable GENAPP must be defined\n";

require "$gap/etc/perl/genapp_util.pl";

check_files() || die "Errors found\n";

print "preliminary checks ok\n";

# test_get_replacements();
# test_get_cond_replacements();

my $error;
my $warn;
my $notice;
my $created;

my $ref_directives = {};
my $ref_menu       = {};
my $rplc           = {};

# $rplc{ "directives" } = start_json( $directives, $ref_directives );
$rplc_directives = start_json( $directives, $ref_directives );

while ( my ( $k, $v ) = each $rplc_directives )
{
    print "$k $v\n";
}
print "menu start_json\n";
# $rplc{ "menu" }       = start_json( $menu      , $ref_menu       );
$rplc_menu = start_json( $menu, $ref_menu );

foreach my $l ( keys %langs )
{
    print "processing language $l\n";
#    $rplc{ "directives" } = rewind_json( $ref_directives );
    $lang = get_lang_json( $l );
    for ( my $i = 0; $i < @{@$lang{ "assembly" }}; ++$i )
    {
        my $use    = $$lang{ "assembly" }[ $i ];
        my $freq   = $$use{ "frequency" };
        my $output = $$use{ "output" };
        my $inputs = $$use{ "inputs" };
        if ( !$freq )
        {
            $notice .= "language $l: no frequency defined for assembly step " . ( $i + 1 ) . " defaulting to 'once'\n";
            next;
        }
        if ( !$output )
        {
            $warn .= "language $l: no output defined for assembly step " . ( $i + 1 ) . "\n";
            next;
        }

        print "assembly freq:   $freq\n";
        print "assembly output: $output\n";
        if ( !@$inputs )
        {
            $warn .= "language $l: no inputs defined for assembly step " . ( $i + 1 ) . " with output $output\n";
            $freq = "once";
        }

        my $outdata;

        for ( my $i = 0; $i < @$inputs; ++$i )
        {
            while( my ( $k, $v ) = each %{$$inputs[ $i ]} )
            {
                print "processing: $k $v\n";
                my $f = "$gap/languages/$l/$k";
                if ( !-e $f )
                {
                    $error .= "language $l: assembly step " . ( $i + 1 ) . ": missing input file $f\n";
                    next;
                }
                my $fh;
                if ( ! open $fh, $f )
                {
                    $error .= "language $l: assembly step " . ( $i + 1 ) . ": error opening input file $f $!\n";
                    next;
                }
                my @l = <$fh>;
                close $fh;
# do replacements
                my $r = get_replacements( \(@l) );
                for ( my $i = 0; $i < @$r; ++$i )
                {
                    print "$f replacement $$r[ $i ]\n";
                }
                while ( my ( $k, $v ) = each $rplc_directives )
                {
                    print "s/__${k}__/${v}/g\n";
                    grep s/__${k}__/${v}/g, @l;
                }
# is freq menu:id
                if ( $v eq 'menu:id' )
                {
                    print "todo menu loop\n";
                    my @l_sav = @l;
                    $rplc_menu = start_json( $menu, $ref_menu );
#                    $rplc_menu = rewind_json( $ref_menu );
                    do {
                        @l = @l_sav;
                        while ( my ( $k, $v ) = each $rplc_menu )
                        {
                            print "s/__${k}__/${v}/g\n";
                            grep s/__${k}__/${v}/g, @l;
                        }
    print "adding to outdata------------------------------\n";
                        $outdata .= join '', @l;
                    } while( $rplc_menu = next_json( $ref_menu, "menu:id" ) );
                } else {
                    $outdata .= join '', @l;
                }
            }
        }
        my $fo = "output/$l/$output";
        mkdir_for_file( $fo );
        my $fh;
        if ( !open $fh, ">$fo" )
        {
            $error .= "language $l: assembly step " . ( $i + 1 ) . ": error opening output file $fo\n";
            next;
        }
        print $fh $outdata;
        close $fh;
        $created .= "$fo\n";
    }
}

print '-'x60 . "\nCreated:\n$created" . '-'x60 . "\n" if $created;
print '-'x60 . "\nNotices:\n$notice" . '-'x60 . "\n"  if $notice;
print '-'x60 . "\nWarnings:\n$warn" . '-'x60 . "\n"   if $warn;
print '-'x60 . "\nErrors:\n$error" . '-'x60 . "\n"    if $error;

