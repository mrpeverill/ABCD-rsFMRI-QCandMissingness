The code in this directory prints RDS files for the connectivity data we need for analysis.

Subject level:
 * FD per scan
 * Amount of useable rcfMRI data for each threshold

Group level:
 * A template connectivity matrix for QA purposes.

# Inputs

First, run the download script in the parent folder (ABCC_download). In our environment this saves subject level data to:

```
/projects/abcd_data/abccbids/NDARINV????????-abcc.tar.xz
```

The submit script runs on 'splits' of the overall dataset which are lists of files output by the download subject. In our environment, the splits, as well as a list of the split files to be queued via HTCondor, are generated via the following commands:

```
ls /projects/abcd_data/abccbids/NDARINV????????-abcc.tar.xz > abccslist.txt
split -l 60 abccslist.txt splits/
ls splits/* > splits.txt
```

# process-split.sh

Within each split, this script uses connectome workbench to calculate parcelated connectivity matrices (pconns) from the downloaded parcelated timeseries (ptseries) for each of 6 motion thresholds, including no scrubbing, for each participant.

# get_image_FD.py

Saves, for each possible functional image, whether the file exists and its average framewise displacement.

# processsplit.R

This R script processes each participant's ses-baselineYear1Arm1_task-rest_desc-filtered_motion_mask.mat file and:

 1. Records the number of useable seconds at each threshold.
 2. Records whether the participant has less than five minutes of data available.
 3. Records the average framewise displacement in the resting state scan after masking.
 4. Records the participants motion mask.
 5. Writes the participants motion mask to a file.
 6. Records the number of useable frames for each mask.
 7. Writes all of this subject level data to a file, e.g. abccsplit000/abccsplit000.subjectdata.RDS

# processsplit2.R

This file processes aggregate (split-wide) statistics on functional connectivity. Specifically:

 1. Read the glasser distance file (glasserdistances.txt)
 2. Read the glasser labels file.
 3. Print a connectivity matrix from three of the glasser networks (Visual2, Somatomotor, Frontopariet) as a qc measure. This is saved to e.g.: abccsplit000/tcorrplot.png

# Outputs

The above processes output to a folder which is compressed into:

/projects/abcd_data/abccbids/abccsplit???.tar.xz

