#!/usr/bin/env Rscript
# This, second R script produces aggregate statistics on a subject split.
splitnum = commandArgs(trailingOnly=TRUE)[1]
#splitnum='split000'
library(tidyverse); library(ggthemes); theme_set(theme_tufte())
library(viridis)

###############
## Functions ##
###############

# Shortcut for matrix reading
matrix_read<-function(x) {as.matrix(read.csv(x,header=FALSE))[1:360,1:360]}

#####
# Read our parcellation data.

glasser_distances <- matrix_read("./glasserdistances.txt")
glasser_labels <- read.csv("tmp/connectivity0.2/ptseries.nii.conc_all_frames_at_FD_0.2_and_smoothing_none_AVG.pconn.nii.labels.txt",header=FALSE)[1:360,1]

# The labels we got from neurostars, which correspond to our distance matrix,
# are the same as the ones we pull out of the .nii files except that the nii
# files also contain subcortical regions. We will strip those for the time
# being.
# all(glasser_labels==gsub("_ROI", "", glasser_altlabels)[1:360])

templatepconn<-matrix_read("tmp/connectivity0.2/ptseries.nii.conc_all_frames_at_FD_0.2_and_smoothing_none_AVG.pconn.nii.txt")[1:360,1:360]

# For plotting we need to convert data in to a long format. We only plot three
# networks (V2, Somatomotor, and Frontopariet) in R hemisephere for legibility.
glasser_networks <- read.csv("./glasser_networks.txt",header = FALSE)[,1]
netindex<-c(which(glasser_networks[1:180]=="Visual2"),
            which(glasser_networks[1:180]=="Somatomotor"),
            which(glasser_networks[1:180]=="Frontopariet"))

templateconnlong <- templatepconn[netindex,netindex] %>%
  reshape2::melt() %>%
  mutate(
    Var1 = factor(Var1,labels=glasser_labels[netindex]),
    Var2 = factor(Var2,labels=glasser_labels[netindex])
  ) %>% rowwise() %>%
  mutate(value_w=min(value,.5))

tcorrplot<-ggplot(data=templateconnlong,
        aes(x=Var1,y=Var2,fill=value_w)) +
    geom_tile() +
    scale_fill_viridis_c() +
    ggtitle(paste(splitnum,"- Template Example Correlations")) +
    theme(axis.text.x=element_text(angle=90),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none")

message("Writing QC connectivity plot")
ggsave(paste0(splitnum,"/tcorrplot.png"),tcorrplot,width = 7.5,height=7)