#!/usr/bin/env bash
set -e
thresh=$1
#thresh="1"

if [ ! -d abccbids ] #For interactive use, skip decompression if it's been done
then
    for i in *tar.gz; do echo "Decompressing $i" && tar -xzf $i; done
    for i in *tar.xz; do echo "Decompressing $i" && tar -xJf $i; done
    shuf pconnfiles.txt > tmp/rpconnfiles.txt
    N=`wc -l tmp/rpconnfiles.txt`
    scount=1
    for i in `cat tmp/rpconnfiles.txt`
    do
	echo "$(date '+%y.%m.%d %H:%M:%S') : Decompressing pconnfile $scount / $N for $i"
	tar -xJvf /projects/abcd_data/abccbids/$i-pconn.tar.xz abccbids/derivatives/$i/AllFD abccbids/derivatives/$i/pconntxt/$i-threshold$thresh.pconn.txt
	scount=$((scount+1))
    done
fi

#Python init
export PATH=$PWD/python/bin:$PATH
export PATH=$PATH:$PWD/workbench/bin_rh_linux64
export PYTHONPATH=$PWD/pypackages/

echo "$(date '+%y.%m.%d %H:%M:%S') : Running Python"
python3 matrixcalc.py $thresh


