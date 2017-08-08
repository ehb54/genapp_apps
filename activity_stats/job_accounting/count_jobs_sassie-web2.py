import string,locale,os,sys

'''

#name    email   duration (h)    finished    running cancelled   started
ASAltieri   altieria@ibbr.umd.edu   0.032   11  0   0   0
Antony  antony@ibt.unam.mx  0.026   33  0   0   0
'''

f = open('dum3.txt','r').readlines()
jobs = 0 
duration = 0 
count = 0
max_jobs = 0 
max_duration = 0.0
ajob = []
username_jobs = ''
username_duration = ''
for line in f:

    try:
        lin = string.split(line)
        this_duration = locale.atof(lin[2])
        this_job = locale.atof(lin[3])
        ajob.append(this_job)
        jobs += this_job
        duration += this_duration
        if(this_job > max_jobs):
            max_jobs = this_job
            username_jobs = lin[0]
        count += 1
        if(this_duration > max_duration):
            max_duration = this_duration
            username_duration = lin[0]

    except:
        pass
    

print 'jobs = ',jobs, '\taverage = ',jobs/count,'\tmax jobs = ',max_jobs,'\tmax user = ',username_jobs
print 'duration = ',duration,'\taverage = ',duration/count,'\tmax duration = ',max_duration,'\tmax user = ',username_duration



