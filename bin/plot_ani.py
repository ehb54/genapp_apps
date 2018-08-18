#!/usr/bin/python
import operator
from preprocess_split import split_data
import statistics as stat
def ani_plot(color_list, exp_keys, dyna_flag, elm_flag, elmdock_flag):
    ani_output = split_data(dyna_flag, elm_flag, elmdock_flag)[2]
    ani_block = []
    for i in range(len(ani_output[0])):
        err_dev = stat.stdev(ani_output[3][i])
        dif = list(map(operator.sub,ani_output[2][i],ani_output[1][i]))
        tmp_data = [
        {
          "x": ani_output[1][i],
          "y": ani_output[2][i],
          "error_x": {
            "array" : ani_output[3][i], 
            "visible" : False
},
          "mode": "markers",
          "marker": {
            "color": color_list[i],
            "size": 12
          },
          "name" : exp_keys[i],
          "legendgroup" : str(i),
          "showlegend" : True
        },
        {
          "x" : ani_output[1][i],
          "y" : ani_output[1][i],
          "mode" : "lines",
          "line" : {
            "color" : color_list[i],
            "width": 3
          },
          "name" : exp_keys[i],
          "legendgroup" : str(i),
          "showlegend" : False
        },
        {
          "x": ani_output[0][i],
          "y": [x/err_dev for x in dif],
          "type": "bar",
          "marker": {"color": color_list[i]},
          "xaxis": "x2",
          "yaxis": "y2",
          "name" : exp_keys[i],
          "legendgroup" : str(i),
          "showlegend" : False
        }
        ]
        ani_block.extend(tmp_data)

    ani_plotly = { 
    "data" : ani_block, 
    "layout" : { 
        "title" : "Fully Anisotropic Model Fit",
        "xaxis" : {
        "domain": [0,0.4],
        "title" : "Rho_exp"
        },
        "yaxis" : {
        "title" : "Rho_pred"
        },
        "yaxis2" : {
        "anchor" : "x2",
        "title" : "Residuals/sigma"
        },
        "xaxis2" : {
        "domain" : [0.6,1],
        "title" : "residue number"
        }     
    }
    }
    return ani_plotly
   
