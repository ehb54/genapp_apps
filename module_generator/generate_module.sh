#!/bin/sh

./parser_mod_sorted_nolistboxes.pl quatest_tags_general_template_nested_rep_for_module_Gen.dat > quafit_test.json

cp quafit_test.json ../modules
