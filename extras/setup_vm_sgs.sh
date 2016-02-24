#!/bin/bash -x

# copy the appconfig and directives templates
cp appconfig.json.vm_sgs appconfig.json
cp directives.json.vm_sgs directives.json

# make needed directories (this would happen automatically as needed during the web run, but we make them now for symbolic link sanity

mkdir -p output/html5/{results,deleted}

ln -s output/html5/ajax ajax
ln -s output/html5/util util
ln -s output/html5/results results
ln -s output/html5/deleted deleted

# docs for sassie (you will need to check this svn out separately into /var/www/htdocs/docs and you will need to contact Joseph for a login
# pushd /var/www/htdocs
# sudo chmod o+w .
# svn export http://www.smallangles.net/sassie/svn/sassie_docs docs
# sudo chmod o-w .
# popd 
# ln -s /var/www/htdocs/docs docs

sudo chown -R apache:apache .
sudo chmod -R g+rw .

pushd /var/www/htdocs
sudo ln -s /share/apps/genapp/sassie2test/output/html5 sassie2test
popd

