import string,locale,os,sys

'''
#name    email   projects    last-login  registered  jobs-not-removed    running admin
ASAltieri   altieria@ibbr.umd.edu   0   2014 Nov 04 21:13 UTC       2   0
Iamsam323   ssparks3@gmail.com  0   2015 Mar 01 21:29 UTC       1   0


0           1                       2   3                       4                       5   6 
ASAltieri   altieria@ibbr.umd.edu   2   2015 Nov 19 19:00 UTC                           11  0   
Antony      antony@ibt.unam.mx      0   2015 Sep 02 18:45 UTC   2015 Aug 28 15:59 UTC   33  0


0           1                       2   3    4   5  6     7     8    9   10 11    12    13  14
ASAltieri   altieria@ibbr.umd.edu   2   2015 Nov 19 19:00 UTC                           11  0   
Antony      antony@ibt.unam.mx      0   2015 Sep 02 18:45 UTC   2015 Aug 28 15:59 UTC   33  0
'''

f = open('dum.txt','r').readlines()
f = open('oct_20_2016.txt','r').readlines()
#f = open('dum2.txt','r').readlines()
zero = 0
jobs = 0 
projects = 0 
count = 0
max_jobs = 0 
max_projects = 0
ajob = []
aprojects = []
for line in f:

    try:
        lin = string.split(line)
        if len(lin) == 10 or len(lin) == 11:
            this_job = locale.atof(lin[8])
        elif len(lin) == 5:
            this_job = locale.atof(lin[4])
        elif len(lin) >= 15:
            this_job = locale.atof(lin[13])
        ajob.append(this_job)
        jobs += this_job
        if(this_job > max_jobs):
            max_jobs = this_job
        if(this_job == 0):
            zero += 1
        count += 1
    except:
        pass
    
    try:  
        lin = string.split(line)
        this_projects = locale.atof(lin[2])
        projects += this_projects
        aprojects.append(this_projects)
        if(this_projects > max_projects):
            max_projects = this_projects
    except:
        pass


print 'jobs = ',jobs, '\taverage = ',jobs/count,'\tmax jobs = ',max_jobs
print 'projects = ',projects,'\taverage = ',projects/count,'\tmax projects = ',max_projects
print 'number of zero jobs = ',zero


