#!/bin/bash
set -x
cd $(dirname $0)
(
chmod u+xrw general-purpose-fortran
cd general-purpose-fortran
rm -rfv download/* mainpage.txt README.md LICENSE
cp $HOME/LIBRARY/libGPF/download/GPF.tgz download/
cp $HOME/LIBRARY/libGPF/mainpage.txt ./
cp $HOME/LIBRARY/libGPF/README.md ./
cp $HOME/LIBRARY/libGPF/LICENSE ./
(cd download; mkdir tmp;cd tmp;tar xvfz ../GPF.tgz)
)

chmod -R 644 general-purpose-fortran
chmod -R a+X general-purpose-fortran
cd general-purpose-fortran
git add -A
git commit -m GPF
#"$(cygpath --unix 'C:\Users\JSU\AppData\Local\GitHubDesktop\app-0.6.2\GitHub Desktop.exe')" &
if [ -f /cygdrive/c/Users/JSU/AppData/Local/GitHubDesktop/GitHubDesktop.exe ]
then
   '/cygdrive/c/Users/JSU/AppData/Local/GitHubDesktop/GitHubDesktop.exe' &
elif [ -f /cygdrive/c/Users/urban/AppData/Local/GitHubDesktop/GitHubDesktop.exe ]
then
   '/cygdrive/c/Users/urban/AppData/Local/GitHubDesktop/GitHubDesktop.exe' &
else
   git push
fi

#(
#cd general-purpose-fortran
#rm -rfv download/*
#)

exit
################################################################################

#ls -l '/cygdrive/c/Users/JSU/AppData/Local/GitHubDesktop/app-0.9.1/GitHubDesktop.exe'
#ls -l '/cygdrive/c/Users/JSU/AppData/Local/GitHubDesktop/app-1.0.3/GitHubDesktop.exe'
#ls -l '/cygdrive/c/Users/JSU/AppData/Local/GitHubDesktop/GitHub Desktop.exe'
#ls -l '/cygdrive/c/Users/JSU/AppData/Local/GitHubDesktop/GitHubDesktop.exe'

#/cygdrive/c/Users/JSU/AppData/Local/GitHubDesktop/app-1.0.3/GitHubDesktop.exe
#/cygdrive/c/Users/JSU/AppData/Local/GitHubDesktop/app-1.0.4/GitHubDesktop.exe
#/cygdrive/c/Users/JSU/AppData/Local/GitHubDesktop/GitHubDesktop.exe
