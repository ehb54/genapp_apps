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



ASAltieri    altieria@ibbr.umd.edu    0.032    11    0    0    0


 ['wrightnt', 'wrightnt@jmu.edu', '0.003', '7', '0', '0', '0']

jobs_08_04_17.txt
'''

f = open('jobs_08_04_17.txt','r').readlines()
zero = 0
jobs = 0 
count = 0
max_jobs = 0 
ajob = []
num_ten = 0
num_fifty = 0
num_hundred = 0

for line in f:
    try:
        lin = string.split(line)
        print 'lin = ',lin
	if(lin[0][0] != "#"):
		this_job = locale.atoi(lin[3])	
        	ajob.append(this_job)
        	jobs += this_job
        	if(this_job > max_jobs):
            		max_jobs = this_job
        	if(this_job == 0):
            		zero += 1
        	if(this_job < 10):
            		num_ten += 1
        	if(this_job < 50):
            		num_fifty += 1
        	if(this_job < 100):
            		num_hundred += 1
        	count += 1
    except:
        pass
    

print 'jobs = ',jobs, '\taverage = ',jobs/count,'\tmax jobs = ',max_jobs
print 'number of zero jobs = ',zero
print 'number of users = ',count
print 'number of users with less than 10 jobs = ', num_ten
print 'number of users with less than 50 jobs = ', num_fifty
print 'number of users with less than 100 jobs = ', num_hundred


