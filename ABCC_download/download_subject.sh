#!/bin/bash
set -e

if [ $# -ne 1 ]; then
    echo "No or too many arguments supplied"
    exit 1
else
    echo "Subject $1"
fi

tar -xzf python310.tar.gz
tar -xzf pyPackages.tar.gz

targetfile="/projects/abcd_data/abccbids/$1-abcc-fullforqc.tar.xz"
export PATH=$PWD/python/bin:$PWD/packages/bin:$PATH
export PYTHONPATH=$PWD/packages
export HOME=$PWD
export XZ_OPT='-T4' #Multithread 4 processes for compression/decompression

#List of target url's
grep -F $1 filtered_problem_manifest.txt > tmp/remote_paths.txt
#Sorted list of target files

#Sometimes, there are more than one paths which have the same filename so go through uniq here.
cat tmp/remote_paths.txt | sed 's!.*/!!' | sort | uniq > tmp/remote_files.txt

manifestfile=abccbids/$1_download_manifest.txt
# If the subject file already exists, unpack it. 
if test -f $targetfile ; then
    echo "Found a subject tar file, unpacking"
    if tar -xJvf $targetfile;
    then
        # local_files.txt is a list of already downloaded files.
	cat $manifestfile | sed 's!.*/!!' | sort > tmp/local_files.txt
    else # If tar fails:
	echo "Decompression Failed! clearing manifest and re-downloading"
	touch tmp/local_files.txt
	#rm $manifestfile || echo "no manifest file to delete"
    fi
else
    echo "No subject tar file found"
    touch tmp/local_files.txt # Make an empty list
fi

# This loop checks the manifest against targets and downloads targets not in the manifest.
# The loop should repeat until they match or we exceed a maximum number of attempts. 

recompress_needed=false
dwnld_complete=false

for i in {1..4};
do
    #List of files which are remote but not local.
    comm -23 tmp/remote_files.txt tmp/local_files.txt > tmp/filtered_filelist.txt
    #List of corresponding URLs. If there is a duplicate, we want just the last match.
    grep -F -f tmp/filtered_filelist.txt tmp/remote_paths.txt > tmp/filtered_paths.txt || true

    if [ -s tmp/filtered_paths.txt ]; then
	echo "running python script for the $i time"
	python3 download_subject.py || echo "Python exited with error"
    else
	echo "No Targets Remain"
	dwnld_complete=true
	break
    fi

    
    wc1=`cat $manifestfile | wc -l` || wc1=0
    if [ -d "abccbids" ]; then
	find abccbids/ ! -name *download_manifest.txt >> $manifestfile
	cat $manifestfile | sort | uniq > temp
	mv temp $manifestfile
	cat $manifestfile | sed 's!.*/!!' | sort > tmp/local_files.txt
    fi
    wc2=`cat $manifestfile | wc -l` || wc2=0

    if [ "$wc2" -gt "$wc1" ]; then
	echo "New files detected!"
	recompress_needed=true
    else
	echo "No new files detected."
    fi
done
	
if $recompress_needed; then
    echo "Recompressing Archive"
    rm $targetfile || echo "No previous archive to delete"
    tar -cvJf $targetfile abccbids
fi

if $dwnld_complete; then echo "Download Completed"; else echo "Download NOT completed, targets remain"; fi
#If we got to this point, exit with last code returned by python for error detection.
exit $pythonexit
