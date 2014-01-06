#!/usr/bin/perl

$gap = $ENV{ "GENAPP" } || die "$0: error env variable GENAPP must be defined\n";

require "$gap/etc/perl/genapp_util.pl";

$debug_rplc++;

my @l = (
#'<p><label for="input1">Center<input type="number" name="input1" id="input1" __~fields:default{ value="0"}__~fields:min{ min="0"}__~fields:max{ max="1e+50"}__~fields:required{ required}><span id="input1_msg" class="warning" > </span></label></p>',
#           '__~myrplcxy',
        '__~fields:default{ value="0"}',
#        '__~myrplcxy2{ max="__hi__"}',
#        '__~myrplc1{ max="__max__"}',
#        '__~myrplc2{ max="__max__"}__~myrplc3{ max="__max__"}',
#         '__~fields:max{ max="1e+50"}'
    );
my $r = get_cond_replacements( \@l );
while( my ( $k, $v ) = each %$r )
{
    print "$k => $v\n";
}

print "original lines:\n";
foreach my $k ( @l )
{
    print "$k\n";
}

    
