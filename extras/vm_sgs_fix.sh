#!/usr/bin/bash -x

rm -f ~root/.cpan
sudo cpan Hash:Merge
sudo pecl install imagick
sudo sh -c 'echo extension=imagick.so >> /usr/local/lib/php.ini'
sudo /etc/rc.d/rc.httpd restart
(cd /src/genapp; sudo -u ehb svn up)
