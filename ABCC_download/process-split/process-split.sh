#!/usr/bin/env bash
set -e

#N.B.: We used to try to get statistics for QC-FC plots in this script as well, but that functionality was better handled elsewhere and has been removed. The functional connectivity/connectome workbench pieces below currently only are used to print some connecitivity plots for QC purpsoses.

#splitname="testsplit"
splitname=$1

if [ ! -d abccbids ] #For interactive use, skip decompression if it's been done
then
    for i in *tar.gz; do echo "Decompressing $i" && tar -xzf $i; done
    for i in *tar.xz; do echo "Decompressing $i" && tar -xJf $i; done
    #wget https://www.humanconnectome.org/storage/app/media/workbench/workbench-rh_linux64-v1.5.0.zip
    echo "Decompressing workbench"
    unzip -q workbench-rh_linux64-v1.5.0.zip
    for i in `cat splits/$splitname`; do echo "Decompressing $i" && tar -xJf $i; done
    #Matlab was downloaded from the website and installed with:
    #./install -agreeToLicense yes -mode silent -outputFile ~/MCRinstalllog -destinationFolder ~/MATLAB_Runtime
fi

#Python init
export PATH=$PWD/python/bin:$PATH
export PATH=$PATH:$PWD/workbench/bin_rh_linux64
export PYTHONPATH=$PWD/pypackages/
#Matlab init
#export LD_LIBRARY_PATH=$PWD/MATLAB_Runtime/v91/bin/glnxa64:$PWD/MATLAB_Runtime/v91/sys/os/glnxa64:$LD_LIBRARY_PATH 
#R init
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/rpackages

#Build a conc file out of the available ptseries
ls abccbids/fmriresults01/derivatives/abcd-hcp-pipeline/sub-NDARINV????????/ses-baselineYear1Arm1/func/sub-NDARINV????????_ses-baselineYear1Arm1_task-rest_bold_atlas-HCP2016FreeSurferSubcortical_desc-filtered_timeseries.ptseries.nii > tmp/ptseries.nii.conc
grep -o NDARINV........ tmp/ptseries.nii.conc | uniq > tmp/snumbers.conc

rm tmp/motionmats.conc || echo "No motionmats to delete"

for i in `cat tmp/snumbers.conc`; do echo abccbids/fmriresults01/derivatives/abcd-hcp-pipeline/sub-$i/ses-baselineYear1Arm1/func/sub-${i}_ses-baselineYear1Arm1_task-rest_desc-filtered_motion_mask.mat >> tmp/motionmats.conc; done

#cifti-connectivity/cifti_conn_wrapper.py --mre-dir MATLAB_Runtime/v91 --wb-command workbench/bin_rh_linux64/wb_command tmp/ptseries.nii.conc .8 tmp/out matrix

echo "Calculating matrices for un-filtered data"
#Matlab Runtime needs LD_LIBRARY_PATH set, but it can mess up other programs. So we just load it for calling cifti_conn_wrapper
#Get pconns without motion correction
LD_LIBRARY_PATH=$PWD/MATLAB_Runtime/v91/bin/glnxa64:$PWD/MATLAB_Runtime/v91/sys/os/glnxa64:$LD_LIBRARY_PATH \
	       cifti-connectivity/cifti_conn_wrapper.py \
	       --mre-dir MATLAB_Runtime/v91 \
	       --wb-command workbench/bin_rh_linux64/wb_command \
	       tmp/ptseries.nii.conc .8 tmp/connectivitynone/ template --keep-conn-matrices

#Get pconns for each threshold
for i in 0.1 0.2 0.3 0.4 0.5; do
    echo "Calculating matrices for threshold $i"
    LD_LIBRARY_PATH=$PWD/MATLAB_Runtime/v91/bin/glnxa64:$PWD/MATLAB_Runtime/v91/sys/os/glnxa64:$LD_LIBRARY_PATH \
		   cifti-connectivity/cifti_conn_wrapper.py \
		   --mre-dir MATLAB_Runtime/v91 \
		   --wb-command workbench/bin_rh_linux64/wb_command \
		   --fd-threshold $i \
		   --motion tmp/motionmats.conc \
		   tmp/ptseries.nii.conc .8 tmp/connectivity$i/ template --keep-conn-matrices
done

mkdir -p tmp/pconntxt

#convert pconns to text.
for s in `cat tmp/snumbers.conc`; do
    for t in 0.1 0.2 0.3 0.4 0.5 none; do
	./workbench/bin_rh_linux64/wb_command -cifti-convert -to-text \
	     ./tmp/connectivity$t/sub-${s}_ses-baselineYear1Arm1_task-rest_bold_atlas-HCP2016FreeSurferSubcortical_desc-filtered_timeseries.ptseries.nii_all_frames_at_FD_$t.pconn.nii \
	     tmp/pconntxt/$s-threshold$t.pconn.txt -col-delim ","
    done
done

#get example template pconn and generate labels
for i in tmp/connectivity0.2/*nii; do
     #Get Labels
     grep -oaE 'Parcel Name=".*"' $i | sed 's/Parcel Name="\(.*\)"/\1/' > $i.labels.txt
     #Get connectivity matrix as text
     ./workbench/bin_rh_linux64/wb_command -cifti-convert -to-text $i $i.txt -col-delim ","
done	

python3 get_image_FD.py
mkdir -p $splitname
Rscript processsplit.R $splitname
Rscript processsplit2.R $splitname

tar -cJvf /projects/abcd_data/abccbids/$splitname.tar.xz $splitname

