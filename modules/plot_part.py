
output_plot_2d_fit_upper = []
output_plot_2d_fit_lower = []
output_plot_2d_exp       = []

min_data_x = []
min_data_y = []
max_data_x = []
max_data_y = []

num_exp       = len(FileNameArr[0])
num_fit_upper = len(FileNameArr[1])
num_fit_lower = len(FileNameArr[2])

# resizing
for i in range(num_exp):
    output_plot_2d_exp.append([])
for i in range(num_fit_upper):
    output_plot_2d_fit_upper.append([])
for i in range(num_fit_lower):
    output_plot_2d_fit_lower.append([])

#Exp - providing number of Exp and Fit sets ARE the same...
for i in range(num_exp):
    xExpArr = GetData( ( FileNameArr[0] )[i] ) [0]
    yExpArr = GetData((FileNameArr[0])[i])[1]
    eExpArr = GetData((FileNameArr[0])[i])[2]   

    xFupArr = GetData((FileNameArr[1])[i])[0]
    yFupArr = GetData((FileNameArr[1])[i])[1]
    eFupArr = GetData((FileNameArr[1])[i])[2]   

    xFloArr = GetData((FileNameArr[2])[i])[0]
    yFloArr = GetData((FileNameArr[2])[i])[1]
    eFloArr = GetData((FileNameArr[2])[i])[2] 

    min_data_x.append( min(ii for ii in xExpArr ) )
    min_data_y.append( min(ii for ii in yExpArr if ii > 0 ) )
    max_data_x.append( max(ii for ii in xExpArr ) )
    max_data_y.append( max(ii for ii in yExpArr if ii > 0 ) )
        
    numpoints_exp = len(xExpArr)
    numpoints_fup = len(xFupArr)
    numpoints_flo = len(xFloArr)
            
    for j in range(numpoints_exp):
        output_plot_2d_exp[i].append([xExpArr[j],yExpArr[j],eExpeArr[j]])     
    for j in range(numpoints_fup):
        output_plot_2d_fit_upper[i].append([xFupArr[j],yFupArr[j]])     
    for j in range(numpoints_fup):
        output_plot_2d_fit_lower[i].append([xFloArr[j],yFloArr[j]])     
               
 


## Plotting with FLOT #####################################################
min_x = min(min_data_x)
min_y = min(min_data_y)
max_x = max(max_data_x)
max_y = max(max_data_y)

#print "max_y: ", max_data_y

xscale = ''
yscale = ''

flot_data = {
    "options": 
    {
        "title"  : "Experimental /&#03C1",
        "xscale" : "log",
        "yscale" : "log",
        "xmin"   : min_x,
        "xmax"   : max_x,
        "ymin"   : min_y,
        "ymax"   : max_y,
        "xlabel" : "q/[1/&#8491;]",
        "ylabel" : "I(q)/[1/cm]",
        "legend" :
        {            
            "position"            : "ne",
            "margin"              : [-70, -1],
            "backgroundColor"     : "null",
            "labelBoxBorderColor" : "#000000",
         },
        "grid" : 
        {
            "backgroundColor" : "#ffffff",
            "margin"    : 
                              { 
                                  "top"    : 0, 
                                  "left"   : 0,
                                  "bottom" : 0,
                                  "right"  : 70
                              }
        },
    },
    "data": [ ]
}

for i in range(num_exp):
    data_label = "Data" + str(i+1)
    current_set_data = {
        "points"  : { "show"      : "true", 
                      "radius"    : 2,
                      "errorbars" : "y", 
                      "yerr"      : { "lowerCap" : "-",
                                      "upperCap" : "-", 
                                      "show"     : "true", 
                                      #"color"    : "red",
                                      "radius"   : 3 
                                  }
                  },
        "label"  : data_label,
        "data"   : output_plot_2d_exp[i]
    }
    flot_data['data'].append(current_set_data)
