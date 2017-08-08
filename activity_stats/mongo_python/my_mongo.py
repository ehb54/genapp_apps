from pymongo import MongoClient
import json

# connect to mongod instance
client = MongoClient('localhost')

# connect to sassie2 database

db = client.sassie2

# get names of all collections

collections = db.collection_names()

for c in collections:
	print c

collection = db['users'].find()

user_keys = []
users = []
sk = []
names = []

for item in collection:
	for key in item.keys():
		if key not in user_keys:
			user_keys.append(key)
			if isinstance(item[key], dict):
				for second_key in item[key]:
					if second_key not in sk:
						sk.append(second_key)

	if key == 'name':
		if item[key] not in names:
			names.append(item[key])


print 'user_keys = ', user_keys	
print 'second_key = ', sk
print 'names = ', names
print 'number of names = ', len(names)


''' 
# does not return anything
posts = db.posts
#for post in posts.find({"name":"joseph"}):
for post in posts.find():
	pprint.pprint(post)

''' 

#names = []

#collection = db['users'].find({"name"})
#for item in collection:
#	for key in item.keys():
#		if key not in names:
#			names.append(key)
#
#print 'names = ', names	

job_keys = []
collection = db['jobs'].find()
for item in collection:
	for key in item.keys():
		if key not in job_keys:
			job_keys.append(key)

print 'job_keys = ', job_keys	





'''
# does something for jobs, crashes with users
col = raw_input("input collection: ")

collection = db[col].find()

keylist = []
for item in collection:
    for key in item.keys():
        if key not in keylist:
            keylist.append(key)
        if isinstance(item[key], dict):
            for subkey in item[key]:
                subkey_annotated = key + "." + subkey
                if subkey_annotated not in keylist:
                    keylist.append(subkey_annotated)
                    if isinstance(item[key][subkey], dict):
                    #if isinstance(item[key][subkey], dict):
			for subkey2 in item[subkey]:
                #        for subkey2 in item[subkey]:
                            subkey2_annotated = subkey_annotated + "." + subkey2
                            if subkey2_annotated not in keylist:
                                keylist.append(subkey2_annotated)
        if isinstance(item[key], list):
            for l in item[key]:
                if isinstance(l, dict):
                    for lkey in l.keys():
                        lkey_annotated = key + ".[" + lkey + "]"
                        if lkey_annotated not in keylist:
                            keylist.append(lkey_annotated)
keylist.sort()
for key in keylist:
    keycnt = db[col].find({key:{'$exists':1}}).count()
    print "%-5d\t%s" % (keycnt, key)

'''

'''
system.indexes
users
security
joblock
jobs
running
licenserequests_CHARMM
license
captcha

'''




'''
# another way, that goes through all databases
d = dict((db, [collection for collection in client[db].collection_names()]) \
             for db in client.database_names())
print json.dumps(d)

'''


