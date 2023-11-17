#!/usr/bin/env Rscript
splitnum = commandArgs(trailingOnly=TRUE)[1]
#splitnum='split000'

print(paste('R Processing Part 1:',splitnum))

## First we want to load subject level data. Primarily, we want to load the
## amount of resting state scan time remaining following scrubbing at a variety
## of thresholds. Average FD per subject will be collected by a separate python
## script.

basepath=path.expand("./abccbids/fmriresults01/derivatives/abcd-hcp-pipeline/")

library(tidyverse); library(ggthemes); theme_set(theme_tufte())

# Single file paths for testing
#matpath=path.expand("./raw/abccbids/fmriresults01/derivatives/abcd-hcp-pipeline/sub-***REMOVED***/ses-baselineYear1Arm1/func")
#matfile=file.path(matpath,"sub-***REMOVED***_ses-baselineYear1Arm1_task-rest_desc-filtered_motion_mask.mat")

# Each .mat file appears in R as a 1x51 matrix of lists. Each list item
# corresponds to one threshold and has child objects describing the number
# of seconds remaining, as well as the masked connectivity matrix at .3mm.

extract_seconds<-function(filename) {
  targetthresholds=c(11,21,31,41,51) #.1, .2, .3, .4, and .5mm
  matdataraw<-R.matlab::readMat(filename)$motion.data[1,targetthresholds]
  mat<-lapply(matdataraw,function(i) {
    unlist(i[[1]][c(3,8,9)])
  })

  sname=substr(basename(filename),5,19)
  df<-as.data.frame(do.call(rbind,mat))
  names(df)<-c("mmthresh","s_remaining","filtered_fd")
  df$sname<-sname
  df[,c(4,1,2,3)]
}
# extract_seconds(matfile)

matfiles=list.files(basepath,"sub-NDARINV........_ses-baselineYear1Arm1_task-rest_desc-filtered_motion_mask.mat",full.names=TRUE,recursive=TRUE)
if(!length(matfiles)>0) { stop(paste("No matfiles found in path",basepath)) }
rseconds_list<-lapply(matfiles,extract_seconds)
rseconds_long<-do.call(rbind,rseconds_list)
rseconds_wide<-rseconds_long %>%
  mutate(ltfiveminutes=s_remaining<5*60) %>%
  pivot_wider(names_from=mmthresh,
              values_from=c(s_remaining,ltfiveminutes,filtered_fd))

## We want to extract the motion mask for each participant at each threshold.
## This is so we can calculate:
## a) a connectivity matrix for each participant to build in to the hexbin
## objects
## b) a template connectivity matrix as a validity test.

write_masks<-function(filename) {
  targetthresholds=c(11,21,31,41,51) #.1, .2, .3, .4, and .5mm
  matdataraw<-R.matlab::readMat(filename)$motion.data[1,targetthresholds]
  mat<-lapply(matdataraw,function(i) {
    unlist(i[[1]][4])
  })
  sid=stringr::str_extract(filename,'NDARINV[A-Z0-9]{8}')
  gframes=list()
  for (i in 1:5) {
    maskvector=+(mat[[i]] == 0)
    maskname=paste0("./tmp/",sid,"_t",i,"_maskV.txt")
    #This gets written by workbench
    #message(paste("Writing",maskname))
    #data.table::fwrite(list(maskvector),maskname)
    gframes[i]<-sum(maskvector)
  }
  names(gframes)<-c('gframe1','gframe2','gframe3','gframe4','gframe5')
  gframes$sname<-sid
  as.data.frame(gframes)
}

masks_list<-lapply(matfiles,write_masks)
masks_df<-do.call(rbind,masks_list)

sdata<-full_join(rseconds_wide,masks_df)

# Get data from python script
dflong<-read.csv("tmp/AllFD",na.strings="999")
FDdata<-dflong %>%
  pivot_wider(id_cols=pid,names_from=Run,values_from=c(Exists,Average))
names(FDdata)[1]<-"sname"

sdata2<-full_join(sdata,FDdata)
saveRDS(sdata2,paste0(splitnum,"/",splitnum,".subjectdata.RDS"))

## Now we want aggregated information for this split, specifically:
##  * A couple of random participant connectivity plots, sorted by network: this
##    is to check that the matrix in the .mat file is in the order we expect.
##  * A list of 5 hexplot objects describing motion x correlation pairs.

# This is the matrix of distances between ROIs in the Glasser Parcellation:
#library(reticulate)
#np <- import("numpy")
# data reading
#distmat <- np$load("../GlasserDistances/glasserdistances.npy")
#glasserlabels<- np$load("../GlasserDistances/glasser_labels.npy")
#glassernetworks<-np$load("../GlasserDistances/glasser_networks.npy")

