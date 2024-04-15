#!/usr/bin/env Rscript
splitnum = commandArgs(trailingOnly=TRUE)[1]
#splitnum='abccsplit110'

print(paste('R Processing Part 1:',splitnum))

## First we want to load subject level data. Primarily, we want to load the
## amount of resting state scan time remaining following scrubbing at a variety
## of thresholds. Average FD per subject will be collected by a separate python
## script.

basepath=path.expand("./abccbids/fmriresults01/derivatives/abcd-hcp-pipeline/")

library(tidyverse); library(ggthemes); theme_set(theme_tufte())

# Single file paths for testing
#matpath=path.expand("./raw/abccbids/fmriresults01/derivatives/abcd-hcp-pipeline/sub-NDARINV<REMOVED>/ses-baselineYear1Arm1/func")
#matfile=file.path(matpath,"sub-NDARINV<REMOVED>_ses-baselineYear1Arm1_task-rest_desc-filtered_motion_mask.mat")

# Each .mat file appears in R as a 1x51 matrix of lists. Each list item
# corresponds to one threshold and has child objects describing the number
# of seconds remaining, as well as the masked connectivity matrix at .3mm.

extract_seconds<-function(filename) {
  targetthresholds=c(1:51) #.00 to .50 in .01 mm increments
  matdataraw<-R.matlab::readMat(filename)$motion.data[1,targetthresholds]
  mat<-lapply(matdataraw,function(i) {
    unlist(i[[1]][c(3,7,8,9)])
  })

  sname=substr(basename(filename),5,19)
  df<-as.data.frame(do.call(rbind,mat))
  names(df)<-c("mmthresh","gframes","s_remaining","filtered_fd")
  df$sname<-sname
  df[,c("sname","mmthresh","gframes","s_remaining","filtered_fd")]
}
# extract_seconds(matfile)

matfiles=list.files(basepath,"sub-NDARINV........_ses-baselineYear1Arm1_task-rest_desc-filtered_motion_mask.mat",full.names=TRUE,recursive=TRUE)
if(!length(matfiles)>0) { stop(paste("No matfiles found in path",basepath)) }
rseconds_list<-lapply(matfiles,extract_seconds)
rseconds_long<-do.call(rbind,rseconds_list)
rseconds_wide<-rseconds_long %>%
  mutate(ltfiveminutes=s_remaining<5*60) %>%
  pivot_wider(names_from=mmthresh,
              values_from=c(s_remaining,ltfiveminutes,gframes,filtered_fd))

sdata<-rseconds_wide

# Get data from python script
dflong<-read.csv("tmp/AllFD",na.strings="999")
FDdata<-dflong %>%
  pivot_wider(id_cols=pid,names_from=Run,values_from=c(Exists,Average))
names(FDdata)[1]<-"sname"

sdata2<-full_join(sdata,FDdata)
saveRDS(sdata2,paste0(splitnum,"/",splitnum,".subjectdata.RDS"))
