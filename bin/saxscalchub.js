#!/usr/bin/node

//  usage 
//  ./build_command_line_executable_from_json.js [input_from_ui]
//  data_file = data_table.json

var fs = require( 'fs' );
var dgram = require('dgram');

const util = require('util');
promisify = util.promisify;
const p_exec = promisify( require( 'child_process' ).exec ) ;
const exec = require( 'child_process' ).exec;
const yaml = require( 'node-yaml' );

const { PerformanceObserver, performance } = require('perf_hooks');

var input_from_ui = process.argv[ 2 ];

var req = JSON.parse( input_from_ui );

const modulename = "alphamultisaxshub";
var runlogfile = req._base_directory + "/" + modulename + "_" + req._base_directory.split('/').pop() + ".txt";
var runlogfile_2 = req._base_directory + "/" + modulename + "_" + req._base_directory.split('/').pop() + "_2.txt";

//var table = JSON.parse( fs.readFileSync( './data_table.json' ) );

var x, key, opt, pref, argm, pdb, pdbshort, expdata, expdatashort;

var runmode = "calc"; // if only pdb is given, "fit" if expdata is given optionally.

var upd_ip = req._udphost;
var udp_port = req._udpport;

//msg._uuid = req._uuid;

if ( "multi_model_pdb" in req ) {
   if  (!( "choose_all_models" in req )) {
       req.choose_first_model = "on"; 
   };
};

// Data structure of iq object for each method
// obj = { "data" : [plot_calc, plot_exp], "layout": {"title": , "xaxis": , "yaxis": } }
// plot_calc = { "x": [array], "y": [array], "error_y"(optional):[array]           } 
// change of x; iq_template.data[0].x = new array
// change of y; iq_template.data[0].y = new array


//Define Iq file suffix w/o and w/t experimental data
/*
  CRYSOL: Not sure of the contents in fit file... need to investigate
         "int" file = A line header(string) + q[1/A], Iq_calc in solution, Iq_calc in vacuo, Iq_solvent, Iq of border layer
         "fit" file = A line header(string) + q, Iq_exp, Some delta, Iq_calc
  FoXS  :
         "dat" file = Two line header(#) + q, Iq, error 
         "fit" file = Three line header(#) + q(from expdata), Iq_exp, error, Iq_calc
  Pepsi-SAXS: 
         "out" file =  A line header(#) + q, Iq_calc, Iat, Iev, Ihs
         "fit" file = A line header(#) + q(expdata), I_exp, dI_exp, Iq_fit
  SASTBX: It requires to read both files if expdata file used.
         "iq" file = A line header(#) + q, Iq_totoal, I_A, I_C, I_B
         "fit" file = A line header(#) + q(expdata), Iq_fit
*/

var iqfiles = {
   "EXPR" : {
              "calc":{
                    "filename": "expdatashort"
                    ,"header"  : 1    // number of header lines
                    ,"contents" : {
                              "q" : 1
                             ,"Iq": 2 // Second column for Iq
                             ,"Iq_error": 3
                     }
               }
              ,"fit":{
                    "filename": "expdatashort"
                    ,"header"  : 1    // number of header lines
                    ,"contents" : {
                              "q" : 1
                             ,"Iq": 2 // Second column for Iq
                             ,"Iq_error": 3
                     }
               }
             }
   ,"CRYSOL" : {
              "calc":{ 
                    "filename": "outfile_CRYSOL_int"
                    ,"header"  : 1    // number of header lines
                    ,"contents" : {
                              "q" : 1
                             ,"Iq": 2 // Second column for Iq 
                     }
               }
              ,"fit":{
                    "filename":  "outfile_CRYSOL_fit"
                    ,"header" : 1
                    ,"contents" : {
                              "q" : 1
                             ,"Iq": 4  
//                             ,"Iq_error": 3 
                     }
               }
             }
   ,"FoXS" : {
              "calc":{
                    "filename": "outfile_FoXS_dat1"
                    ,"header"  : 2
                    ,"contents" : {
                              "q" : 1
                             ,"Iq": 2 
//                             ,"Iq_error": 3
                      }
                     }
              ,"fit":{
                    "filename":  "outfile_FoXS_fit"
                    ,"header" : 3
                    ,"contents" : {
                              "q" : 1
                             ,"Iq": 4
//                             ,"Iq_error": 3
                     }
                    }
              }
   ,"Pepsi_SAXS" : {
              "calc":{
                    "filename": "outfile_Pepsi_SAXS_out"
                    ,"header"  : 1
                    ,"contents" : {
                              "q" : 1
                             ,"Iq": 2
//                             ,"Iq_error": 3
                      }
                     }
              ,"fit":{
                    "filename":  "outfile_Pepsi_SAXS_fit"
                    ,"header" : 1
                    ,"contents" : {
                              "q" : 1
                             ,"Iq": 4
                     }
                    } 
              }
   ,"SASTBX" : {
              "calc":{
                    "filename": "outfile_SASTBX_iq"
                    ,"header"  : 1
                    ,"contents" : {
                              "q" : 1
                             ,"Iq": 2
                      } 
                     }
              ,"fit":{
                    "filename":  "outfile_SASTBX_fit"
                    ,"header" : 1 
                    ,"contents" : {
                              "q" : 1
                             ,"Iq": 2
                     }
                    } 
              }
   ,"SasCalc" : {
              }
   ,"WAXSiS" : {
              }
   ,"SoftWAXS" : {
              }
   ,"AXES" : {
              }

};

var iq_obj = {
    "EXPR"   : {}
    ,"CRYSOL": {}
    ,"FoXS" : {}
    ,"Pepsi_SAXS" : {}
    ,"SASTBX" : {}
    ,"SasCalc" : {}
    ,"WAXSiS" : {}
    ,"SoftWAXS" : {}
    ,"AXES"     : {}
}  

var iq_obj_residual = { 
    "CRYSOL": {}
    ,"FoXS" : {}
    ,"Pepsi_SAXS" : {}
    ,"SASTBX" : {}
    ,"SasCalc" : {}
    ,"WAXSiS" : {}
    ,"SoftWAXS" : {}
    ,"AXES"     : {}
}

var color_table = {
    "EXPR": "rgb(0,0,0)" 
    ,"CRYSOL": "rgb(148,0,211)" //Violet
    ,"FoXS" : "rgb(255,0,0)" //red
    ,"Pepsi_SAXS" : "rgb(0,0,255)" //blue
    ,"SASTBX" : "rgb(255,255,0)" // yellow
    ,"SasCalc" : "rgb(0,255,0)" // green
    ,"WAXSiS" : "rgb(255,127,0)" // orange
    ,"SoftWAXS" : "rgb(75,0,130)" // indigo
    ,"AXES"     : "rgb(192,192,192)" // gray

}
var trace_marker =    {
                "x":
                []
                ,"y":
                []
                ,"error_y": {
                "array": []
                ,"visible":"false"
                ,"color": "rgb(0, 0, 0)"
                ,"thickness": 1.2
                ,"width": 5.5
                ,"opacity": 1
                }
                ,"name" : "Marker"
                ,"mode" : "markers"
                ,"marker" : {
                    "color" : "rgb(0, 0, 0)"
                    ,"size": 4
                }
};

var trace_linemarker = {
                "x":
                ["0.0001", "1", "10", "100"]
                ,"y":
                [1, 0.1, 0.01, 0.001]
                ,"error_y": {
                "array": [0, 0.07, 0.007, 0.0007]
                ,"visible":"false"
                }
                ,"name": "Line + Markers"
                ,"mode" : "lines+markers"
                ,"marker" : {
                    "color" : "rgb(0, 0, 0)"
                    ,"size": 8
                }
                ,"line" : {
                        "color" : "rgb(0, 0, 0)"
                        ,"width" : 1
                }
};

var trace_line =  {
                "x":
                []
                ,"y":
                []
                ,"error_y": {
                "array": []
                ,"visible":false
                }
                ,"xaxis": "x"
                ,"yaxis": "y"
//                ,"legendgroup" : ""
                ,"name" : "Lines"
                ,"mode" : "lines"
                ,"line" : {
                        "color" : "rgb(0, 0, 0)"
                        ,"width" : 2
                }
                ,"showlegend": true 
};

var sub_tracebar =  { 
                "x":
                []
                ,"y":
                []
                ,"text": []
                ,"textfont": {
                           "color": "#ffffff"    
                } 
                ,"textposition": "auto"
                
                ,"error_y": {
                          "array": []
                          ,"visible":false
                }
                ,"xaxis": "x2"
                ,"yaxis": "y2"
                ,"type" : "bar"
                ,"bargap" : 1.5
};

var samplelayout = {
        "data" :[]
        ,"layout" : {
            "title" : "Place holder for Plotly, not real data!!!! "
            ,"xaxis" : {
//                  "type": "log"
                  "title": "q[1/Angstrom]"
//                  "title": "&Mu;"
                  ,"autorange": "true"
            }
            ,"yaxis" : {
                  "type": "log"
                  ,"title": "Iq[A.U]"
//                  ,"range": [-3.9,0.5]
                  ,"autorange": "true"
            }
        }
    };

var sub_layout = {
        "data" :[]
        ,"layout" : {
            "title" : "Place holder for Plotly, not real data!!!! "
            ,"legend": {
                    "x": 0.87
                   ,"y": 1.0 
//                   ,"orientation": "h"
             }
            ,"xaxis" : {
//                  "type": "log"
                  "title": "q[1/Angstrom]"
//                  "title": "q[1/Å]"
                  ,"autorange": "true"
                  ,"domain": [0, 0.55]
            }
            ,"yaxis" : {
                  "type": "log"
                  ,"title": "Iq[A.U]"
//                  ,"range": [-3.9,0.5]
                  ,"autorange": "true"
                  ,"domain": [0.0, 0.5]
            }
            ,"xaxis2" : {
                  "domain": [0.60, 0.85]
            }
            ,"yaxis2" : {
                  "anchor": "x2"
                  ,"title": "Reduced chi-square"
            }
            ,"xaxis3" : {
                  "domain": [0.0, 0.55]
                  ,"autorange": "true"
                  ,"title": "q[1/Angstrom]"
            }
            ,"yaxis3" : {
                  "anchor": "x3"
                  ,"title": "(Icalc -Iexp)/Iexp[%]"
                  ,"titlefont": {
                    "size": 12 
                   } 
                  ,"domain": [0.7, 1.2]
            }
        }
    };
 
var iq_template =  { 
       "data" : [
                {
                "x": 
                ["0.0001", "1", "10", "100"]
                ,"y": 
                [1, 0.1, 0.01, 0.001]
                ,"error_y": {
                "array": [0, 0.07, 0.007, 0.0007]
                ,"visible":"true"
                }
                ,"mode" : "lines+markers"
                ,"marker" : {
                    "color" : "rgb(128, 0, 128)"
                    ,"size": 8
                }
                ,"line" : {
                        "color" : "rgb(128, 0, 128)"
                        ,"width" : 1
                }
            }
        ],
        "layout" : {
            "title" : "Place holder for Plotly, not real data!!!! "
            ,"xaxis" : {
                  "type": "log"
                  ,"title": "q"
            }
            ,"yaxis" : {
                  "type": "log"
                  ,"title": "Iq"
                  ,"range": [-3.9,0.5]
                  ,"autorange": "false"
            }
        }
    };


var angular_units = {
    "CRYSOL" : { "0" : "0", "1" : "1", "2" : "2", "3" : "3", "4" : "4" } 
    ,"FoXS"  : { "0" : "1", "1" : "2", "2" : "3", "3" : "2", "4" : "3" } 
    ,"Pepsi_SAXS" : { "0" : "0", "1" : "1", "2" : "2", "3" : "3", "4" : "4" }
/* Place holder for future 
    ,"SASTBX"  : [0,1,2,3,4]
    ,"SasCalc" : [0,1,2,3,4]
    ,"WAXSiS"  : [0,1,2,3,4]
    ,"SoftWAXS" : [0,1,2,3,4]
    ,"AXES"  : [0,1,2,3,4]
*/
};

var table = {
    "CRYSOL" :
    {
             "executable" : "crysol"
             ,"input" : { 
                     "pdb" : { 
                         "type" : "file"
                         ,"number_of_files" : 1
                         ,"required" : "true"
                         }
                     ,"experimental_curve" : {
                         "type" : "file"
                         ,"number_of_files" : 1000000 
                         ,"required" : "optional"
                         }
                  }

              ,"output": 
                  {
                      "stderr" : {   
                          "role"   : "output"
                          ,"id"    : "output_crysol_stderr"
                          ,"type" : "file"
                          ,"label" : "CRYSOL stderr file"
                          }
                      ,"stdout" : {   
                          "role"   : "output"
                          ,"id"    : "output_crysol_stdout"
                          ,"type" : "file"
                          ,"label" : "CRYSOL stdout file."                
                          }
                      ,"sav" : {
                          "role"   : "output"
                          ,"id"    : "output_crysol_sav"
                          ,"label" : "CRYSOL output containing the amplitudes among other necessary information to evaluate the intensities curves"
                          ,"type" : "file"
                          }
                      ,"log" : {
                          "role"   : "output"
                          ,"id"    : "output_crysol_log"
                          ,"type" : "file"
                          ,"label" : "CRYSOL output containing the screen output and the detailed warning messages."
                          }
                      ,"flm" : {
                          "role"   : "output"
                          ,"id"    : "output_crysol_flm"
                          ,"type" : "file"
                          ,"label" : "CRYSOL output Containing the multipole coefficients Flm which describe the particle envelope."
                          }
                      ,"int" : {
                          "role"   : "output"
                          ,"id"    : "output_crysol_int"
                          ,"type" : "file"
                          ,"label" : "CRYSOL output with Scattering profiles"
                          }
                      ,"fit" : {
                          "role"   : "output"
                          ,"id"    : "output_crysol_fit"
                          ,"type" : "file"
                          ,"label" : "Contains the fit to the experimental data in inverse angstroms."
                          }

                      ,"alm" : {
                          "role"   : "output"
                          ,"id"    : "output_crysol_alm"
                          ,"type" : "file"
                          ,"label" : "Sum of the scattering amplitudes for atoms, excluded volume and border for rigid body modelling later "
                          }
                  } 

              ,"options" : 
                  {
//                     "choose_all_models" : {
//                          "prefix_full" : {} 
//                          ,"prefix_short" : {}
//                          ,"type" : "integer"
//                          ,"info" : "Choose all MODELS if multi-frame PDB given"
//                          }
                      "choose_first_model" : {
                          "prefix_full" : " -nmr 1"
                          ,"prefix_short" : " -nmr 1"
                          ,"type" : "checkbox"
                          ,"info" : "Choose first model only"
                          }
                      ,"qmax" : {
                          "prefix_full" : "-sm"
                          ,"prefix_short" : "-sm"
                          ,"type" : "float"
                          ,"info" : "Maximum scattering vector in inverse angstroms (max = 1.0 A^-1)"
                          }
                      ,"number_of_points" : {
                          "prefix_full" : "-ns"
                          ,"prefix_short" : "-ns"
                      ,"type" : "integer"
                          ,"info" : "Number of points in the theoretical curve (max = 5000)"
                          }
                      ,"angular_units" : {
                          "prefix_full" : "-un"
                          ,"prefix_short" : "-un"
                          ,"type" : "listbox"
                          ,"min" : 1
                          ,"max" : 4
                          ,"default" : 1
                          ,"info" : "Rewrite here "
                          }
                      ,"constant_subtractions" : {
                          "prefix_full" : "-cst"
                          ,"prefix_short" : "-cst"
                          ,"type" : "checkbox"
                          ,"info" : "Constant subtraction. No value is required"
                          }
                      ,"explicit_hydrogen" : {
                          "prefix_full" : "-eh"
                          ,"prefix_short" : "-eh"
                          ,"type" : "checkbox"
                          ,"info" : "Account for explicit hydrogens. No value is required."
                          }
                      ,"contrast_hydration" : {
                          "prefix_full" : "-dro"
                          ,"prefix_short" : "-dro"
                          ,"type" : "float"
                          ,"info" : "Contrast of hydration shell (e/A^3)"
                          }
                      ,"maxharmonics" : {
                          "prefix_full" : "-lm"
                          ,"prefix_short" : "-lm"
                          ,"type" : "integer"
                          ,"min" : 1
                          ,"default" : 15
                          ,"max" : 50 
                          ,"info" : "Maximum number of harmonics "
                          }
                      ,"fibonacci_grid" : {
                          "prefix_full" : "-fb"
                          ,"prefix_short" : "-fb"
                          ,"type" : "integer"
                          ,"min" : 10
                          ,"default" : 17
                          ,"max" : 18
                          ,"info" : "Order of Fibonacci grid "
                          }
                      ,"solvation_density" : {
                          "prefix_full" : "-dns"
                          ,"prefix_short" : "-dns"
                          ,"type" : "float"
                          ,"info" : "Solvent density (e/A^3). Default value is the electron density of pure water."
                          }
                      ,"optional_outputs" : {
                          "prefix_full" : "-kp"
                          ,"prefix_short" : "-kp"
                          ,"type" : "checkbox"
                          ,"info" : "Creates optional output files. No value is required"
                          }
                      ,"write_error" : {
                          "prefix_full" : "-err"
                          ,"prefix_short" : "-err"
                          ,"type" : "checkbox"
                          ,"info" : "Write experimental errors to the .fit file (Y or N)."
                          }
                      ,"read_old_pdb" : {
                          "prefix_full" : "-old"
                          ,"prefix_short" : "-old"
                          ,"type" : "checkbox"
                          ,"info" : "Read a PDB file with an old atom naming (pre 2008). No value is required."
                          }
//                      ,"select_conformers" : {
//                          "prefix_full" : "-nmr"
//                          ,"prefix_short" : "-nmr"
//                          ,"type" : "integer"
//                          ,"info" : "Sequential number of a model or an NMR conformer to process."
//                          }
                      ,"chain_id" : {
                          "prefix_full" : "-cid"
                          ,"prefix_short" : "-cid"
                          ,"type" : "character"
                          ,"info" : "Compute theoretical scattering only for certain chain identifier(s)."
                          }
                  }
    }

    ,"FoXS" : 
       { 
           "executable" : "foxs"
           ,"input" : 
               {
                     "pdb"  : { 
                         "type" : "file"
                         ,"number_of_files" : "unlimited"
                         ,"required" : "true"
                         }
                     ,"experimental_curve" : {
                         "type" : "file"
                         ,"number_of_files" : "unlimited"
                         ,"required" : "true"
                         }
               }
           ,"output" :
               {
                      "stderr" : {
                          "role"   : "output"
                          ,"id"    : "output_FoXS_stderr"
                          ,"type" : "file"
                          ,"label" : "FoXS stderr file"
                          }
                      ,"stdout" : {
                          "role"   : "output"
                          ,"id"    : "output_FoXS_stdout"
                          ,"type" : "file"
                          ,"label" : "FoXS stdout file."                                                
                          }
                     ,"fit" : {
                          "role"   : "output"
                          ,"id"    : "output_FoXS_fit"
                          ,"type" : "file"
                          ,"label" : "Contains the fit to the experimental data in inverse angstroms."
                          }
                     ,"dat1" : {
                          "role"   : "output"
                          ,"id"    : "output_FoXS_dat1"
                          ,"type" : "file"
                          ,"label" : "Contains the prediction from input pdb "
                          }
                     ,"dat2" : {
                          "role"   : "output"
                          ,"id"    : "output_FoXS_dat2"
                          ,"type" : "file"
                          ,"label" : "Contains the intensities"
                          }
               }
           ,"options" :
               {
                     "choose_all_models" : {
                          "prefix_full" : "-m 3"
                          ,"prefix_short" : "-m 3"
                          ,"type" : "checkbox"
                          ,"info" : "Choose all MODELS if multi-frame PDB given"
                          }
                     ,"choose_first_model" : {
                          "prefix_full" : "-m 1 "
                          ,"prefix_short" : "-m 1 "
                          ,"type" : "checkbox"
                          ,"info" : "Choose first model only"
                          }
                     ,"qmax"  : {
                         "prefix_full" : "--max_q"
                         ,"prefix_short" : "-q"
                         ,"type" : "float"
                         ,"default" : 0.5
                         ,"info" : "In inverse Angstroms"
                         }
                     ,"number_of_points"  : {
                         "prefix_full" : "--profile_size"
                         ,"prefix_short" : "-s"
                         ,"type" : "integer"
                         ,"default" : 500 
                         ,"info" : "Number of points in the scattering curve (only applicable if experimental data is not provided)"
                         }
                     ,"angular_units"  : {
                         "prefix_full" : "--units"
                         ,"prefix_short" : "-u"
                         ,"type" : "listbox"
                         ,"default" : 1
                         ,"info" : "1 - unknown --> determine automatically (default), 2 - q values are in 1/A, 3 - q values are in 1/nm"
                         }
                      ,"constant_subtractions_FoXS" : {
                          "prefix_full" : "--background_q"
                          ,"prefix_short" : "-b"
                          ,"type" : "float"
                          ,"default" : 0
                          ,"info" : "background adjustment, not used by default. if enabled, recommended q value is 0.2"
                          }
                     ,"explicit_hydrogen"  : {
                         "prefix_full" : "--hydrogens"
                         ,"prefix_short" : "-h"
                         ,"type" : "checkbox"
                         ,"info" : "explicitly consider hydrogens in PDB files"
                         }
                     ,"min_c1"  : {
                         "prefix_full" : "--min_c1"
                         ,"prefix_short" : "--min_c1"
                         ,"type" : "float"
                         ,"default" : 0.99 
                         ,"info" : "min c1 value"
                         }
                     ,"max_c1"  : {
                         "prefix_full" : "--max_c1"
                         ,"prefix_short" : "--max_c1"
                         ,"type" : "float"
                         ,"default" : 1.05    
                         ,"info" : "max c1 value"
                         }
                     ,"min_c2"  : {
                         "prefix_full" : "--min_c2"
                         ,"prefix_short" : "--min_c2"
                         ,"type" : "float"
                         ,"default" : -2.00    
                         ,"info" : "min c2 value"
                         }
                     ,"max_c2"  : {
                         "prefix_full" : "--max_c2"
                         ,"prefix_short" : "--max_c2"
                         ,"type" : "float"
                         ,"default" : 4.00    
                         ,"info" : "max c2 value"
                         }
                     ,"fast_ca"  : {
                         "prefix_full" : "--residues"
                         ,"prefix_short" : "-r"
                         ,"type" : "checkbox"
                         ,"info" : "fast coarse grained calculation using CA atoms only"
                         }
                     ,"offset_FoXS"  : {
                         "prefix_full" : "--offset"
                         ,"prefix_short" : "--offset"
                         ,"type" : "checkbox"
                         ,"info" : "use offset in fitting"
                         }
                     ,"partial_profile"  : {
                         "prefix_full" : "--write-partial-profile"
                         ,"prefix_short" : "-p"
                         ,"type" : "checkbox"
                         ,"info" : "write partial profile file"
                         }
                     ,"multi_model_pdb_FoXS"  : {
                         "prefix_full" : "--multi-model-pdb"
                         ,"prefix_short" : "-m"
                         ,"type" : "integer"
                         ,"default" : 1
                         ,"info" : "1 - read the first MODEL only (default), 2 - read each MODEL into a separate, structure, 3 - read all models into a single structure"
                         }
                     ,"volatility_ratio"  : {
                         "prefix_full" : "--volatility_ratio"
                         ,"prefix_short" : "-v"
                         ,"type" : "checkbox"
                         ,"info" : "calculate volatility ratio score"
                         }

                     ,"score_log"  : {
                         "prefix_full" : "--score_log"
                         ,"prefix_short" : "-l"
                         ,"type" : "checkbox"
                         ,"info" : "use log(intensity) in fitting and scoring"
                         }

                     ,"gnuplot_script"  : {
                         "prefix_full" : "--gnuplot_script"
                         ,"prefix_short" : "-g"
                         ,"type" : "checkbox"
                         ,"info" : "print gnuplot script for gnuplot viewing"
                         }

                     ,"version"  : {
                         "prefix_full" : "--version"
                         ,"prefix_short" : "--version"
                         ,"type" : "checkbox"
                         ,"info" : "Version info"
                         }
               }
    }

    ,"Pepsi_SAXS" :
       {
           "executable" : "pepsisaxs"
           ,"input" :
               {
                     "pdb"  : {
                         "type" : "file"
                         ,"number_of_files" : 1
                         ,"required" : "true"
                         }
                     ,"experimental_curve" : {
                         "type" : "file"
                         ,"number_of_files" : 1 
                         ,"required" : "true"
                         }
               }
           ,"output" :
               {
                      "stderr" : {
                          "role"   : "output"
                          ,"id"    : "output_Pepsi_SAXS_stderr"
                          ,"type" : "file"
                          ,"label" : "FoXS stderr file"
                          }
                      ,"stdout" : {
                          "role"   : "output"
                          ,"id"    : "output_Pepsi_SAXS_stdout"
                          ,"type" : "file"
                          ,"label" : "FoXS stdout file."  
                          }
                      ,"log" : {
                          "role"   : "output"
                          ,"id"    : "output_Pepsi_SAXS_log"
                          ,"type" : "file"
                          ,"label" : "Pepsi-Saxs output Containing the screen output and the detailed warning messages."
                          }
                      ,"fit" : {
                          "role"   : "output"
                          ,"id"    : "output_Pepsi_SAXS_fit"
                          ,"type" : "file"
                          ,"label" : "Pepsi-Saxs output Containing the fit to the experimental data in inverse angstroms."
                          }
                      ,"out" : {
                          "role"   : "output"
                          ,"id"    : "output_Pepsi_SAXS_out"
                          ,"type" : "file"
                          ,"label" : "Pepsi-Saxs output Containing the predicted model."
                          }
               }
           ,"options" :
               {
                     "qmax"  : {
                         "prefix_full" : "--maximum_scattering_vector"
                         ,"prefix_short" : "-ms"
                         ,"type" : "float"
                         ,"max" : 1.0
                         ,"default" : 0.5
                         ,"min" : 0.01
                         ,"info" : "In inverse Angstroms (max = 1.0 {A^-1}) (only applicable if experimental data is not provided.)"
                         }
                     ,"number_of_points"  : {
                         "prefix_full" : "--number_of_points"
                         ,"prefix_short" : "-ns"
                         ,"type" : "integer"
                         ,"max" : 5000
                         ,"default" : 101
                         ,"min" : 10
                         ,"info" : "Number of points in the scattering curve (only applicable if experimental data is not provided), default 101, max 5000"
                         }
                     ,"angular_units"  : {
                         "prefix_full" : "--angular_units"
                         ,"prefix_short" : "-au"
                         ,"type" : "listbox"
                         ,"info" : "Same with crysol. Default is chosen automatically."
                         }
                      ,"constant_subtractions" : {
                          "prefix_full" : "--cst"
                          ,"prefix_short" : "-cst"
                          ,"type" : "checkbox"
                          ,"info" : "Substract a constant factor if experimental data contains a systematic error"
                          }
                     ,"explicit_hydrogen"  : {
                         "prefix_full" : "--hyd"
                         ,"prefix_short" : "-hyd"
                         ,"type" : "checkbox"
                         ,"info" : "Using explicit hydrogens, not recommended for standard residues"
                         }
                     ,"output_path"  : {
                         "prefix_full" : "--output"
                         ,"prefix_short" : "-o"
                         ,"type" : "character"
                         ,"info" : "Output scattering curve path"
                         }
                     ,"expansion_order"  : {
                         "prefix_full" : "--nCoeff"
                         ,"prefix_short" : "-n"
                         ,"type" : "integer"
                         ,"info" : "Number of coefficients in the expansion order of spherical harmonics"
                         }
                     ,"log_json"  : {
                         "prefix_full" : "--json"
                         ,"prefix_short" : "-json"
                         ,"type" : "checkbox"
                         ,"info" : "Output the log file in the JSON format"
                         }
                     ,"negative_contrast"  : {
                         "prefix_full" : "--neg"
                         ,"prefix_short" : "-neg"
                         ,"type" : "checkbox"
                         ,"info" : "Allowing negative contrast of the hydration shell upon fitting"
                         }
                     ,"fast_hydration"  : {
                         "prefix_full" : "--fast"
                         ,"prefix_short" : "-fast"
                         ,"type" : "checkbox"
                         ,"info" : "A coarser representation of the hydration shell"
                         }
                     ,"no_smearing"  : {
                         "prefix_full" : "--noSmearing"
                         ,"prefix_short" : "--noSmearing"
                         ,"type" : "checkbox"
                         ,"info" : "Disables the data smearing"
                         }
                     ,"dro"  : {
                         "prefix_full" : "--dro"
                         ,"prefix_short" : "--dro"
                         ,"type" : "float"
                         ,"default" : 5
                         ,"info" : "Hydration shell contrast (in % of the bulk value), default is 5%."
                         }
                     ,"fluc"  : {
                         "prefix_full" : "--fluc"
                         ,"prefix_short" : "--fluc"
                         ,"type" : "checkbox"
                         ,"info" : "Enable fluctuations"
                         }
                     ,"modes"  : {
                         "prefix_full" : "--modes"
                         ,"prefix_short" : "--modes"
                         ,"type" : "integer"
                         ,"min"        : 5
                         ,"default"    : 5
                         ,"max"        : 75
                         ,"info" : "Number of low frequency modes"
                         }
                     ,"cov_scaling"  : {
                         "prefix_full" : "--covScaling"
                         ,"prefix_short" : "--covScaling"
                         ,"type" : "float"
                         ,"default" : 1
                         ,"info" : "Scaling of covalent interactions"
                         }
                     ,"cutoff"  : {
                         "prefix_full" : "--cutoff"
                         ,"prefix_short" : "-c"
                         ,"type" : "float"
                         ,"default" : 5
                         ,"info" : "Interactions cutoff distance in angstroms, 5 by default"
                         }
                     ,"display_log"  : {
                         "prefix_full" : "--changelog"
                         ,"prefix_short" : "-log"
                         ,"type" : "none"
                         ,"info" : "Displays ChangeLog information and exits."
                         }
                     ,"version"  : {
                         "prefix_full" : "--version"
                         ,"prefix_short" : "--version"
                         ,"type" : "none"
                         ,"info" : "Displays version information and exits"
                         }
               }
    }
    ,"SASTBX" :
       {
           "executable" : "sastbx"
           ,"input" :
               {
                     "pdb" : {
                         "type" : "file"
                         ,"number_of_files" : 1
                         ,"required" : "true"
                         }
                     ,"experimental_curve" : {
                         "type" : "file"
                         ,"number_of_files" : 1000000
                         ,"required" : "false"
                         }
                     ,"pdblist" : {
                         "type" : "file"
                         ,"number_of_files" : 1
                         ,"required" : "false"
                         }
               }
           ,"output" :
               {
                      "stderr" : {
                          "role"   : "output"
                          ,"id"    : "output_SASTBX_stderr"
                          ,"type" : "file"
                          ,"label" : "SASTBX stderr file"
                          }
                      ,"stdout" : {
                          "role"   : "output"
                          ,"id"    : "output_SASTBX_stdout"
                          ,"type" : "file"
                          ,"label" : "SASTBX stdout file."
                          }
                      ,"iq" : {
                          "role"   : "output"
                          ,"id"    : "output_SASTBX_iq"
                          ,"type" : "file"
                          ,"label" : "SASTBX output I(q) file from input pdb by Debye model."
                          }
                      ,"json" : {
                          "role"   : "output"
                          ,"id"    : "output_SASTBX_json"
                          ,"type" : "file"
                          ,"label" : "SASTBX output I(q) file (JSON format)."
                          }
                      ,"fit" : {
                          "role"   : "output"
                          ,"id"    : "output_SASTBX_fit"
                          ,"type" : "file"
                          ,"label" : "SASTBX output I(q) file containing the fit to the experimental data"
                          }
               }
           ,"options" :
               {
                     "qmax"  : {
                         "prefix_full" : "q_stop="
                         ,"prefix_short" : "q_stop="
                         ,"type" : "float"
                         ,"default" : 0.5 
                         ,"info" : "Maximum q"
                         }
                     ,"qmin"  : {
                         "prefix_full" : "q_start="
                         ,"prefix_short" : "q_start="
                         ,"type" : "float"
                         ,"default" : 0.0
                         ,"info" : "minimum q"
                         }
                      ,"number_of_points" : {
                          "prefix_full" : "n_step="
                          ,"prefix_short" : "n_step="
                          ,"type" : "integer"
                          ,"default" : 51
                          ,"info" : "Number of points in the theoretical curve"
                          }

               }
    } 
    ,"sascalc_hub" :
       {
           "executable" : "sascalc_hub"
           ,"input" :
               {

               }
           ,"output" :
               {
                      "stderr" : {
                          "role"   : "output"
                          ,"id"    : "output_sascalc_hub_stderr"
                          ,"type" : "file"
                          ,"label" : "SasCalc stderr file"
                          }
                      ,"stdout" : {
                          "role"   : "output"
                          ,"id"    : "output_sascalc_hub_stdout"
                          ,"type" : "file"
                          ,"label" : "SasCalc stdout file."  
                          }
               }
           ,"options" :
               {

               }
    }
 
    ,"WAXSiS" :
       {
           "executable" : "waxsis"
           ,"input" :
               {

               }
           ,"output" :
               {
                      "stderr" : {
                          "role"   : "output"
                          ,"id"    : "output_WAXSiS_stderr"
                          ,"type" : "file"
                          ,"label" : "WAXSiS stderr file"
                          }
                      ,"stdout" : {
                          "role"   : "output"
                          ,"id"    : "output_WAXSiS_stdout"
                          ,"type" : "file"
                          ,"label" : "WAXSiS stdout file."  
                          }
               }
           ,"options" :
               {

               }
    }
 
    ,"SoftWAXS" :
       {
           "executable" : "softwaxs"
           ,"input" :
               {

               }
           ,"output" :
               {
                      "stderr" : {
                          "role"   : "output"
                          ,"id"    : "output_SoftWAXS_stderr"
                          ,"type" : "file"
                          ,"label" : "SoftWAXS stderr file"
                          }
                      ,"stdout" : {
                          "role"   : "output"
                          ,"id"    : "output_SoftWAXS_stdout"
                          ,"type" : "file"
                          ,"label" : "SoftWAXS stdout file."  
                          }
               }
           ,"options" :
               {

               }
    }
 
    ,"AXES" :
       {
           "executable" : "axes"
           ,"input" :
               {

               }
           ,"output" :
               {
                      "stderr" : {
                          "role"   : "output"
                          ,"id"    : "output_AXES_stderr"
                          ,"type" : "file"
                          ,"label" : "AXES stderr file"
                          }
                      ,"stdout" : {
                          "role"   : "output"
                          ,"id"    : "output_AXES_stdout"
                          ,"type" : "file"
                          ,"label" : "AXES stdout file."  
                          }
               }
           ,"options" :
               {

               }
    }

};             

var cmds = {};   // exec <options, flags>  for each method
var runcmds = {}; // full sequence of command-line commands including files & directory preparation 
var res = {}; // standard output objects to be written in _textarea 
var files = {}; // output file name objects from each method
var cmds_users = {}; // Command-line commands for users to reproduce by themselves 

//Get executables for each commands

const cmdpath ='/home/cheol/work/alphamultisaxshub/bin/';
const sastbx_path = cmdpath + 'sastbx_ubuntu_commandline/build/'
//const sastbx_bin = sastbx_path + "bin/"

for ( x in req ) {
    var new_x = x.replace(/select_all-/g,'')
    if (new_x in table) {
           cmds[ new_x ] = '';
           cmds_users[ new_x ] = '';
           if (new_x == 'CRYSOL') {
               cmds[ new_x ] = "export LD_LIBRARY_PATH='$LD_LIBRARY_PATH:/home/cheol/atsas/ATSAS-2.8.4-1/lib/x86_64-linux-gnu/atsas/' && ";
           };
           if (new_x == 'SASTBX') {
                cmds[ new_x ] += ". " + sastbx_path + "setpaths_all.sh && ";
//                cmds[ new_x ] += cmdpath + table[ new_x ].executable ;
//                cmds[ new_x ] += ". /home/cheol/work/alphamultisaxshub/bin/sastbx_ubuntu_commandline/build/setpaths_all.sh && ";
//                cmds[ new_x ] += "/home/cheol/work/alphamultisaxshub/bin/sastbx_ubuntu_commandline/build/bin/" + table[ new_x ].executable + ".she";
                  cmds_users[ new_x ] += cmds[ new_x ];
           }  
           cmds[ new_x ] += cmdpath+table[ new_x ].executable;
           cmds_users[ new_x ] += table[ new_x ].executable;
        };
};

// Add input files to command-line commands

//const string_expdata = "set_experimental_curve-experimental_curve";

const string_expdata = "experimental_curve";
const string_pdb = "pdb"; 

// required pdb allowing multiple pdbs

pdb = req[ string_pdb ].join( " " );

var pdbshort = pdb.split('/').slice(-1).pop();

// optional experimental data allowing multiple exp data files

expdata = "";
expdatashort = "";
var expdata_sastbx_short = "";
if ( string_expdata in req ) {
       expdata += req[ string_expdata ].join( " " );
       expdatashort = expdata.split('/').slice(-1).pop();
       expdata_sastbx_short = expdatashort.split('.').slice(0,-1) + ".qis";    
       runmode = "fit";
    };

//console.log(expdata,expdatashort);

for ( x in cmds ) {

    if ( x == "SASTBX" ) {
       cmds[ x ] += " structure=" + pdbshort;
       if ( string_expdata in req ) {
           cmds[ x ] += " experimental_data=" + expdata_sastbx_short;
           cmds_users[ x ] += " experimental_data=" + expdata_sastbx_short;
       };
    } else {
        cmds[ x ] += " " + pdbshort;
        cmds[ x ] += " " + expdatashort;
        cmds_users[ x ] += " " + pdbshort;
        cmds_users[ x ] += " " + expdatashort;
    }
};


// Adding options with arguments
for ( x in cmds ) { 
   
    for ( opt in table[ x ]["options"] ) {

        if ( opt in req ) {
            if ( table[ x ][ "options" ] [ opt ] [ "type" ] != "listbox" ) {
                cmds[ x ] += " " + table[ x ][ "options" ][ opt ][ "prefix_full" ];
                cmds_users[ x ] += " " + table[ x ][ "options" ][ opt ][ "prefix_full" ];
                argm = "" 
                if ( table[ x ][ "options" ] [ opt ] [ "type" ] != "checkbox" ) {
                    argm += req[ opt ] 
                };
                if ( x == "SASTBX" ) {
                    cmds[ x ] += argm;
                    cmds_users[ x ] += argm;
                } else {
                    cmds[ x ] += " " + argm;
                    cmds_users[ x ] += " " + argm;
                };
            } else {
              if ( req[ opt ] != 0) {
                 cmds[ x ] += " " + table[ x ][ "options" ][ opt ][ "prefix_full" ];
                 cmds_users[ x ] += " " + table[ x ][ "options" ][ opt ][ "prefix_full" ];
                 argm = ""
                 if ( opt == "angular_units" ) {
                     argm += angular_units[ x ][ req[ opt ] ]; 
                 } else {
                 argm += req[ opt ];
                 };
                 if ( x == "SASTBX" ) {
                    cmds[ x ] += argm;
                    cmds_users[ x ] += argm;
                 } else {
                    cmds[ x ] += " " + argm;
                    cmds_users[ x ] += " " + argm;
                 };
              }; 
            };
        }; 
   };
};

// Write command-line commands for each cmds
for ( x in cmds ) {
     runcmds[ x ] = "mkdir " + x +" && cd " + x + "  && cp ../" + pdbshort + " . && " ;
//     runcmds[ x ] = "cd " + x + "  && cp ../" + pdbshort + " . && " ;
     if (string_expdata in req) {
         runcmds[ x ] += "cp ../" + expdatashort + " . && " ;
         if ( x == "SASTBX" ) {
             runcmds[ x ] += "sed '1d' " + expdatashort + " > " + expdata_sastbx_short + " && " ;
         }
     };
     runcmds[ x ] +=  cmds[ x ]  + " > " + x + ".stdout  2> " + x + ".stderr"
};

// Prepare log at _textarea

res._textarea = "Run Command Setup:\n ";
res._textarea += JSON.stringify( runcmds, null, 2 ) + "\n";

res._textarea += "Command Lines:\n "
res._textarea += JSON.stringify(cmds, null, 2) + "\n";

res._textarea += "\n JSON input parameters:\n "
res._textarea += JSON.stringify(req, null, 2) +  "\n";

// function to find output files from each method with number of lines > 0

function udp_outfile( x ) {
    var output = {}; // use for UDP messaging
    files.x = fs.readdirSync(  x + '/' );
    try {
    if ( x != 'FoXS' ) {
    files.x.forEach ( function ( file ) {
        var suffix = file.split('.').pop();
        if ( suffix in table[ x ][ "output" ]) {
             var text = fs.readFileSync( x + "/" + file);
             var lines = text.toString().split('\n').length - 1;
             if ( lines != 0 ) {
                 //var xkey = 'outfile_' + x + '_' + suffix;
                 //var xvalue = req._base_directory + '/' + x  + '/' + file;
                 res[ 'outfile_' + x + '_' + suffix ] = req._base_directory + '/' + x  + '/' + file;
                 output[ 'outfile_' + x + '_' + suffix ] = req._base_directory + '/' + x  + '/' + file;
                 res._textarea += 'outfile_' + x + '_' + suffix + "\n";
             };
        };
    });
    } else {
        files.x.forEach ( function ( file ) {
        var suffix = file.split('.').pop();
        var suffix_dat = suffix;
        if ( suffix == 'dat') {
             if ( pdbshort + '.dat' == file) {           // FoXS theoretical Iq files has nameing convention; pdbname + '.dat'
                 suffix_dat = 'dat1';
             } else if  ( file == expdatashort ) {
                 suffix_dat = ' ';
             } else {
                 suffix_dat = 'dat2';
             }
        };
        if ( suffix_dat in table[ x ][ "output" ]) {
             var text = fs.readFileSync( x + "/" + file)
             var lines = text.toString().split('\n').length - 1;
             if ( lines != 0 ) {
                 //var xkey = 'outfile_' + x + '_' + suffix_dat;
                 //var xvalue = req._base_directory + '/' + x  + '/' + file;
                 res[ 'outfile_' + x + '_' + suffix_dat ] = req._base_directory + '/' + x  + '/' + file;
                 output[ 'outfile_' + x + '_' + suffix_dat ] = req._base_directory + '/' + x  + '/' + file;
                 res._textarea += 'outfile_' + x + '_' + suffix_dat + "\n";
             };
        };
        });
    }; 
     
    } catch(err) {
           results[ x ] = 'Error during writing output files of '+  x  + ' : ' + err.message;
    }
    return output;
}

// Generate a zip file of rea._base_directory excluding input files in each method directory
function write_zip(err) {
    var zipdir = '';
    var output = {};
    for ( x in cmds ) {
        zipdir += x + " ";
        var rm_cmd = "rm -rf " + './' + x + '/' + pdbshort;
        if (expdatashort.length != 0) {
            rm_cmd += " && rm -rf " + './' + x + '/' + expdatashort;
        };
        exec( rm_cmd );
    };

    var zipname = req._base_directory;
    zipname = req._base_directory + "/" + zipname.split('/').pop() + ".zip";
    const zipcmd = "zip -r " + zipname + " " + zipdir + " " +  runlogfile.split('/').pop() ;
//    res._textarea += zipcmd + "\n";

    exec(zipcmd,(err,stdout,stderr) => {
        if (err) {
            res._textarea += "Error in zipping output file :" + err.message + "\n";
        }
    });
    res.outfile_zip = zipname;
    output.outfile_zip = zipname;
    return output;
}

// Write log file for run 
function save_logfile(){
    var logtext = {};
    var patt = /^_/;
    logtext[ "Title" ] = "Job logs of " + modulename + " run";
    logtext[ "Activated Programs" ] = [];
    for ( x in runcmds ) {
        logtext[ "Activated Programs" ].push(x) ;
        logtext[ x ] = {};
        logtext[ x ][ "Run commands" ] = cmds_users[ x ]  + " > " + x + ".stdout  2> " + x + ".stderr";
    };

    logtext[ "Options" ] = {};
    for ( y in req ){
        if (!patt.test( y )){
            logtext[ "Options" ][ y ] = req[ y ];
        };
    };
    
    logtext_yaml =  yaml.dump(logtext, {
        //'styles': {
        //     '!!null': 'canonical'
       // }
        'sortKeys': false        // sort object keys
    });

    fs.writeFile(runlogfile, logtext_yaml, function(err){
    if(err){
         res._textarea += "Error in writing job log file : " + err.message + "\n"; 
         }
    });

    res.log_multisaxshub = runlogfile;
    var output = {};
    output.log_multisaxshub = runlogfile;
    return output
}   

// Write log file for run
function save_logfile_2(){
    var logtext = {};
    var patt = /^_/;
    logtext[ "Title" ] = "Job logs of " + modulename + " run";
    logtext[ "Activated Programs" ] = [];

    for ( x in runcmds ) {
        logtext[ "Activated Programs" ].push(x) ;
        logtext[ x ] = {};
        logtext[ x ][ "Run commands" ] = cmds_users[ x ]  + " > " + x + ".stdout  2> " + x + ".stderr";
        logtext[ x ][ "Options" ] = {};
            for ( opt in req ){
                if ( opt in table[ x ]["options"] ) {
                    if (!patt.test( opt ) ){

                        logtext[ x ][ "Options" ][ opt ] = req[ opt ] + " ==> ";
                        var opt_from_table = "";
                        opt_from_table += table[ x ][ "options" ][ opt ][ "prefix_full" ];

                        var opt_mapped = " ";
                        if ( table[ x ][ "options" ] [ opt ] [ "type" ] != "checkbox" ) {
                            opt_mapped += req[ opt ];
                        };

                        if ( opt == "angular_units"){
                            opt_mapped = angular_units[ x ][ req[ opt ] ];
                        }; 
                            
                        if ( x == "SASTBX"){ 
                           opt_from_table += opt_mapped;
                        } else {
                           opt_from_table += " " + opt_mapped;
                        }; 
                        logtext[ x ][ "Options" ][ opt ] += opt_from_table; 
                    };
                };
            };
    };

    logtext_yaml =  yaml.dump(logtext, {
        'styles': {
             '!!null': 'canonical'
        }
        ,'sortKeys': false        // sort object keys
    });

    fs.writeFile(runlogfile_2, logtext_yaml, function(err){
    if(err){
         res._textarea += "Error in writing job log file : " + err.message + "\n";
         }
    });

    res.log_multisaxshub_2 = runlogfile_2;
    var output = {};
    output.log_multisaxshub_2 = runlogfile_2;
    return output
}

// UDP messaging
function send_udpmsg( obj ) {
    obj._uuid = req._uuid;
    const udp_client = dgram.createSocket('udp4');
    const txt = JSON.stringify( obj );
    udp_client.send( txt,0, txt.length, req._udpport, req._udphost, ( err ) => {
        if (err) throw err;
        udp_client.close();
    });
}

function readfile(x,output) {
    var file_id = iqfiles[ x ][ runmode ][ "filename" ];
    const f = output[ file_id ];
    try {
        var data = fs.readFileSync(f);
        var result = data.toString().split('\n');
    } catch(error){result = "Error in reading " + f}; 
    return result;
}

function readfile_exp() {
    try {
        var data = fs.readFileSync(expdatashort);
        var result = data.toString().split('\n');
    } catch(error){
        res._textarea = "Error in reading " + expdatashort;
        throw err;
    } return result;
}

var Iq_ALL = {};
var Iq_ref_data = {};
var chi_square = [];
var chi_square_reduced = [];
var chi_square_pierson = [];

async function prepIqJSON_all( ) {
// Initialize Iq_ALL and Iq_exp_data
   
   if ( runmode == "calc" ) {
       var Iq_mock = JSON.parse(JSON.stringify( samplelayout ));
       Iq_mock[ "layout" ][ "title" ] = "Theoretical Iq profiles with " + pdbshort;
   };
   if ( runmode == "fit" ) {
       var Iq_mock = JSON.parse(JSON.stringify( sub_layout ));
       Iq_mock[ "layout" ][ "title" ] = "Fitted Iq profiles with " + pdbshort + " & " + expdatashort + " for q < " + req.qmax + " [1/Angstrom]" ;
       // Initialize experimental data for Iq plot
       var data_exp = await readfile_exp();
       iq_obj[ "EXPR" ] = JSON.parse( JSON.stringify( trace_marker ) );
       await getIqJSON_exp(data_exp); 
   };    
   Iq_ALL[ "iq_ALL" ] = Iq_mock;
}

async function getIqJSON_all( ) {
    if ( runmode == "fit" ) {
       Iq_ALL[ "iq_ALL" ][ "data" ].push( iq_obj[ "EXPR" ] );
       var data_chi_temp = JSON.parse( JSON.stringify( sub_tracebar ) );
       var chi2_reduced_obj = calc_reduced_chi2();
    };
//    var mock_chi = 20; // mock chi2 data for each x
    for ( x in cmds ) {
       Iq_ALL[ "iq_ALL" ][ "data" ].push( iq_obj[ x ] );
       if ( runmode == "fit" ) {
//           mock_chi += 10; // use chi_square[] array
           data_chi_temp[ "x" ].push( x );
           data_chi_temp[ "y" ].push( chi2_reduced_obj[ x ] );
           data_chi_temp[ "text" ].push( chi2_reduced_obj[ x ].toFixed(3) );
//           data_chi_temp[ "name" ] = "chi^2 for q < " + req.qmax + " [1/A]" ;
           data_chi_temp[ "showlegend" ] = false;
 
// CHECK getIqJSON_all
//           res._textarea += JSON.stringify(data_chi_temp) + "\n";
//           res._textarea += chi2_reduced_obj[x] + " THIS IS REDUCED CHI2\n";
           
       };
    };

    if ( runmode == "fit" ) {
       Iq_ALL[ "iq_ALL" ][ "data" ].push( data_chi_temp );
       for ( x in cmds ) {
           Iq_ALL[ "iq_ALL" ][ "data" ].push(iq_obj_residual[ x ]);
       };
    };
    
//   res._textarea += JSON.stringify(data_chi_temp);
//   res._textarea += JSON.stringify(Iq_ALL);  
}

function calc_reduced_chi2() {

    var chi2 = {};
    var expdata_len = iq_obj[ "EXPR" ][ "y" ].length;

    var delta_iq = 0.0;
    var correction_delta = 1E-16;
    var error_corrected = 0.0;
    var residual_q = 0.0;

    try { 
        for (x in runcmds ) {
            iq_obj_residual[ x ] = JSON.parse( JSON.stringify( trace_line ) );
            var calcdata_len = iq_obj[ x ][ "y" ].length;
            var sum_chi2 = 0.0;
            var q = [];
            var Iq_residual = []; 
//            if ( calcdata_len < expdata_len ) {
            var chi2_message = "Chi^2 obtained for q < " + req.qmax ;
//            };
                     
            for ( i = 0; i < calcdata_len; i++ ) {
                delta_iq = iq_obj[ x ][ "y" ][ i ] - iq_obj[ "EXPR" ][ "y" ][ i ]; 
                error_corrected = iq_obj[ "EXPR" ][ "error_y" ][ "array" ][ i ];
// Residual
                residual_q = delta_iq/iq_obj[ "EXPR" ][ "y" ][ i ] * 100.0;
                qx = iq_obj[ "EXPR" ][ "x" ][ i ];
                q.push( qx );
                Iq_residual.push( residual_q );
 
                sum_chi2 += (delta_iq/error_corrected)**2;
//                res._textarea += i.toString() + " " + qx + " " +residual_q + "\n";
            };
            iq_obj_residual[ x ][ "x" ] = q;
            iq_obj_residual[ x ][ "y" ] = Iq_residual;
            iq_obj_residual[ x ][ "name" ] = x + "_residual";
            iq_obj_residual[ x ][ "xaxis" ] = "x" ;
            iq_obj_residual[ x ][ "yaxis" ] = "y3";
//            iq_obj_residual[ x ][ "legendgroup" ] = x;
            iq_obj_residual[ x ][ "showlegend" ] = true;
            iq_obj_residual[ x ][ "line" ][ "color" ] = color_table[ x ];
            iq_obj_residual[ x ][ "line" ][ "dash" ] = "dot";
            chi2[ x ] =  sum_chi2 / Number( calcdata_len); //.toFixed(5);
        };
        return chi2;
        res._textarea += "CHI-SQUARE \n";
        res._textarea += JSON.stringify( chi2 ) + "\n";
    } catch (error) {
        res._textarea += "Error while calculating chi-squares" ;
    }

}

async function getIqJSON_exp( txt ) {

    try {
        const startline = iqfiles[ "EXPR" ][ runmode ][ "header" ];
        const id_q = iqfiles[ "EXPR" ][ runmode ][ "contents" ][ "q" ];
        const id_iq = iqfiles[ "EXPR" ][ runmode ][ "contents" ][ "Iq" ];
        if ("Iq_error" in iqfiles[ "EXPR" ][ runmode ][ "contents" ]) {
            var id_sd = iqfiles[ "EXPR" ][ runmode ][ "contents" ][ "Iq_error" ];
        };

        var q = [];
        var Iq = [];
        var Iq_error = [];
        for( i = startline; i < txt.length  ; i++ ) {
            var txtstring = txt[i].toString().replace( /^\s*/,"" );
            var patt = /\D{1,3}/g;
//            var txttrim = txtstring.replace(/\s+/g,'').replace(/\h+/g,'');
            var txttrim = txtstring.replace(/\s+/g,'').replace(/\h+/g,'').replace(/\./g,'').replace(/(e|E)(\-|\+)/g,'');
//            res._textarea += txttrim + " "+ patt.exec(txttrim) + "\n";
//            console.log(JSON.stringify( res ));
//            if ( txtstring !== null && txtstring !== '' && patt.exec(txttrim) !== null ){
            if ( txtstring !== null && txtstring !== '' && patt.exec(txttrim) == null ){
                var txtline = txt[i].toString().replace( /^\s*/,"" ).split( /\s+/ );
                q.push( txtline[ id_q - 1 ] );
                Iq.push( txtline[ id_iq - 1 ] );
                if ("Iq_error" in iqfiles[ "EXPR" ][ runmode ][ "contents" ] ) {
                    Iq_error.push( txtline[ id_sd - 1 ] );
                };
            };
        };
        iq_obj[ "EXPR" ][ "x" ] = q;
        iq_obj[ "EXPR" ][ "y" ] = Iq;
        iq_obj[ "EXPR" ][ "name" ] = expdatashort;
        iq_obj[ "EXPR" ][ "error_y" ][ "array" ] = Iq_error;
        iq_obj[ "EXPR" ][ "error_y" ][ "visible" ] = "true";
      } catch (error) { 
            res._textarea += "Error in making PLOTLY JSON of experimental data \n"
      };
}
 
function getIqJSON(x,layout,tempdata,txt) {
    try {
        const startline = iqfiles[ x ][ runmode ][ "header" ];
        const id_q = iqfiles[ x ][ runmode ][ "contents" ][ "q" ];
        const id_iq = iqfiles[ x ][ runmode ][ "contents" ][ "Iq" ];
        if ("Iq_error" in iqfiles[ x ][ runmode ][ "contents" ]) {
            var id_sd = iqfiles[ x ][ runmode ][ "contents" ][ "Iq_error" ];
        };

        var q = [];
        var Iq = [];
        var Iq_error = [];
//        for(i = startline; i < 5; i++) {
        for( i = startline; i < txt.length  ; i++ ) {
            var txtstring = txt[i].toString().replace( /^\s*/,"" );
            if ( txtstring !== null && txtstring !== '' ) {
                var txtline = txt[i].toString().replace( /^\s*/,"" ).split( /\s+/ );
                // throw q less than min q in experimental data
                if ( runmode == "fit" ) {
                    if ( !( Number( txtline[ id_q - 1 ] ).toFixed(5) < Number( iq_obj[ "EXPR" ][ "x" ][ 0 ] ).toFixed(5) ) ) {
                        q.push( txtline[ id_q - 1 ] );
                        Iq.push( txtline[ id_iq - 1 ] );
                        if ("Iq_error" in iqfiles[ x ][ runmode ][ "contents" ]) {
                            Iq_error.push( txtline[ id_sd - 1 ] );
                        };
                    };
                } else {
                    q.push( txtline[ id_q - 1 ] );
                    Iq.push( txtline[ id_iq - 1 ] );
                    if ("Iq_error" in iqfiles[ x ][ runmode ][ "contents" ]) {
                        Iq_error.push( txtline[ id_sd - 1 ] );
                    };
                };
             };
        };

       var Iq_layout = {};
       Iq_layout = JSON.parse( JSON.stringify( layout ) );
       iq_obj[ x ] = JSON.parse( JSON.stringify( tempdata ) );
       iq_obj[ x ][ "x" ] = q;
       iq_obj[ x ][ "y" ] = Iq;
       iq_obj[ x ][ "name" ] = x; 
//       iq_obj[ x ][ "legendgroup" ] = x
       iq_obj[ x ][ "line" ][ "color" ] = color_table[ x ];
       if ("Iq_error" in iqfiles[ x ][ runmode ][ "contents" ]) {
           iq_obj[ x ][ "error_y" ][ "array" ] = Iq_error; 
           iq_obj[ x ][ "error_y" ][ "visible" ] = "true";
       } else {
           delete iq_obj[ "error_y" ];
       };

       Iq_layout[ "data" ] = [ iq_obj[ x ] ];

       if ( runmode == "calc" ) {
          Iq_layout[ "layout" ][ "title" ] = "Theoretical Iq profile from " + x + " with " + pdbshort;
        };

       if ( runmode == "fit" ) {
          Iq_layout[ "layout" ][ "title" ] = "Fitted Iq profile from " + x + " with " + expdatashort ; 
          Iq_layout[ "data" ].push( iq_obj[ "EXPR" ] );
        };

//       iq_obj[ x ] = Iq_data ;
       var result = {};
       result[ "iq_" + x ] = Iq_layout;
//       res._textarea += JSON.stringify(result);

    } catch (error) { result = "Error in making PLOTLY JSON for " + x  +"\n"};

    return result;
};

// Define a chain of promise for each method before parallelizaton
async function do_exec( x ) {
    var x_startTime = performance.now();
    send_udpmsg( { _textarea : "Starting " + x + " job\n" } );

    result = await p_exec( runcmds[ x ] );
    var x_exeTime = (Number.parseInt(performance.now() - x_startTime))/1000;
    send_udpmsg( { _textarea : "Completed " + x + " job: " + x_exeTime + " seconds.\n" } );

    var output = udp_outfile( x );
    send_udpmsg( { _textarea : "Writing output files for " + x + " done\n" } );
    send_udpmsg( output );

    var tst = readfile(x,output);
    Iq_udp = getIqJSON(x,samplelayout,trace_line,tst);
//    send_udpmsg( { _textarea : "Sending UPD for " + x + " \n" + JSON.stringify(Iq_udp) + " \n" }); 
//    send_udpmsg( Iq_udp );
    res[ "iq_" + x ] = Iq_udp[ "iq_" + x ] 
//    send_udpmsg( { _textarea : "Sent UPD for " + x + " \n" });
    return result;
}

// Define parallel job of methods
async function parallel() {
    var tasks = {};
    var udptasks = {};
    for ( x in runcmds ) {
        tasks[ x ] = do_exec( x );
    }

    var results = {};
//    var errors = {};

    try {    
        for ( x in runcmds ) {
           results[ x ] = await tasks[ x ];
        }
     } catch(err) {
           results[ x ] = 'Error in '+  x  + ' : ' + err.message;
     }
    return results;
}

res._textarea += "\n LOG of executables:\n ";

// Main function to submit parallel job
async function taskRunner(fn, label) {
  const startTime = performance.now();
    try {
        prepIqJSON_all();
        var result = await fn();
        var output = save_logfile();
        send_udpmsg( output );
        var output = save_logfile_2();
        send_udpmsg( output );
        getIqJSON_all();
        var output = write_zip();
        send_udpmsg( output );
//        send_udpmsg( { _textarea : "Try to send UPD for Iq_ALL : \n" });
//        send_udpmsg( { _textarea : "IQ ALL : \n" + JSON.stringify(Iq_ALL) + " \n" });
//        send_udpmsg( Iq_ALL );
        if ( ! ( "debug_msg" in req ) ) { 
	    delete res._textarea;
        }
        res[ "iq_ALL" ] = Iq_ALL[ "iq_ALL" ] ;
//        res._textarea += "SUCCESS in Generating IQ_ALL \n";

        console.log(JSON.stringify(res));
        send_udpmsg( { _textarea : "Finished Multi SAXS job!" } );
    } 
    catch (err) {
       res._textarea += "Error : " + err.message + "\n";
       console.log(err.message);
    }
}

// void taskRunner(parallel, 'parallel');
taskRunner(parallel, 'parallel', {} );
