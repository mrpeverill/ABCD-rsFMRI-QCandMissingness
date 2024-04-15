FastQC01 processing
================

This document processes abcd_fastqc01.txt and outputs table indicating
whether participants have adequate useable data according to that
source. The file must be saved in this folder.

# fastqc01

This file describes qc data in the fast track images from which the ABCC
was derived.

``` r
tnames<-read_tsv("abcd_fastqc01.txt",col_names=FALSE,n_max=1)
```

    ## Rows: 1 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (21): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
desc<-read_tsv("abcd_fastqc01.txt",col_names=FALSE,skip=1,n_max=1)
```

    ## Rows: 1 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (21): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fastqc<-read_tsv("abcd_fastqc01.txt",skip=2,col_names=FALSE)
```

    ## Rows: 816467 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (10): X4, X5, X6, X8, X11, X12, X13, X18, X20, X21
    ## dbl (11): X1, X2, X3, X7, X9, X10, X14, X15, X16, X17, X19
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
names(fastqc)<-t(tnames)
cbind(t(tnames),t(desc))
```

    ##     [,1]               
    ## X1  "collection_id"    
    ## X2  "abcd_fastqc01_id" 
    ## X3  "dataset_id"       
    ## X4  "subjectkey"       
    ## X5  "src_subject_id"   
    ## X6  "interview_date"   
    ## X7  "interview_age"    
    ## X8  "sex"              
    ## X9  "img03_id"         
    ## X10 "origin_dataset_id"
    ## X11 "visit"            
    ## X12 "file_source"      
    ## X13 "ftq_series_id"    
    ## X14 "abcd_compliant"   
    ## X15 "ftq_complete"     
    ## X16 "ftq_quality"      
    ## X17 "ftq_recalled"     
    ## X18 "ftq_recall_reason"
    ## X19 "ftq_usable"       
    ## X20 "ftq_notes"        
    ## X21 "collection_title" 
    ##     [,2]                                                                                             
    ## X1  "collection_id"                                                                                  
    ## X2  "abcd_fastqc01_id"                                                                               
    ## X3  "dataset_id"                                                                                     
    ## X4  "The NDAR Global Unique Identifier (GUID) for research subject"                                  
    ## X5  "Subject ID how it's defined in lab/project"                                                     
    ## X6  "Date on which the interview/genetic test/sampling/imaging/biospecimen was completed. MM/DD/YYYY"
    ## X7  "Age in months at the time of the interview/test/sampling/imaging."                              
    ## X8  "Sex of subject at birth"                                                                        
    ## X9  "Corresponds to row_id in image03 data structure, mapping derivative to source record in image03"
    ## X10 "Origin dataset Id"                                                                              
    ## X11 "Visit name"                                                                                     
    ## X12 "File name/location"                                                                             
    ## X13 "Series identifier (subject_event_type_date_time)"                                               
    ## X14 "Sequence parameters compliant with the ABCD imaging protocol"                                   
    ## X15 "Whether series is complete (no missing DICOMs)"                                                 
    ## X16 "Whether series passed manual quality review"                                                    
    ## X17 "Whether series is active or has been recalled and should not be used"                           
    ## X18 "Brief description of why series is recalled"                                                    
    ## X19 "Whether series is recommended for use"                                                          
    ## X20 "Notes from manual quality review"                                                               
    ## X21 "collection_title"

``` r
fastqc <- fastqc %>%
  select(c(subjectkey,visit,ftq_recalled,ftq_series_id,ftq_usable,ftq_notes,file_source))

privatize <- function(dataframe) {
  # This function removes subject id's from output
  # Step 1: Replace factors with character vectors
  factors_columns <- sapply(dataframe, is.factor)
  dataframe[factors_columns] <- lapply(dataframe[factors_columns], as.character)

  # Step 2: Replace character values matching regex with "sid_removed"
  regex_pattern1 <- "NDARMC[0-9A-Z]{6}"
  regex_pattern2 <- "NDARINV[0-9A-Z]{8}"
  dataframe <- lapply(dataframe, function(col) {
    if (is.character(col)) {
      col <- gsub(regex_pattern1, "sid_removed", col)
      col <- gsub(regex_pattern2, "sid_removed", col)
    }
    return(col)
  })

  return(dataframe)
}
```

The fastqc file is parsed and a few relevant columns are selected:

- **subjectkey**: *sid_removed*, *sid_removed*, *sid_removed*,
  *sid_removed*, *sid_removed* and *sid_removed*
- **visit**: *baseline_year_1\_arm_1*, *baseline_year_1\_arm_1*,
  *baseline_year_1\_arm_1*, *baseline_year_1\_arm_1*,
  *baseline_year_1\_arm_1* and *2_year_follow_up_y\_arm_1*
- **ftq_recalled**: *0*, *1*, *0*, *1*, *0* and *1*
- **ftq_series_id**:
  *sid_removed_baselineYear1Arm1_ABCD-MID-fMRI_20161213165238*,
  *sid_removed_baselineYear1Arm1_ABCD-MID-fMRI_20161213170251*,
  *sid_removed_baselineYear1Arm1_ABCD-MID-fMRI_20161213170251*,
  *sid_removed_baselineYear1Arm1_ABCD-SST-fMRI_20161213171252*,
  *sid_removed_baselineYear1Arm1_ABCD-SST-fMRI_20161213171252* and
  *sid_removed_2YearFollowUpYArm1_ABCD-T1_20181221131833*
- **ftq_usable**: *1*, *0*, *1*, *0*, *1* and *0*
- **ftq_notes**: *NA*, *NA*, *NA*, *NA*, *NA* and *NA*
- **file_source**:
  *s3://NDAR_Central_1/submission_13124/sid_removed_baselineYear1Arm1_ABCD-MID-fMRI_20161213165238.tgz*,
  *s3://NDAR_Central_2/submission_16925/sid_removed_baselineYear1Arm1_ABCD-MID-fMRI_20161213170251.tgz*,
  *s3://NDAR_Central_4/submission_31575/sid_removed_baselineYear1Arm1_ABCD-MID-fMRI_20161213170251.tgz*,
  *s3://NDAR_Central_2/submission_16925/sid_removed_baselineYear1Arm1_ABCD-SST-fMRI_20161213171252.tgz*,
  *s3://NDAR_Central_4/submission_31575/sid_removed_baselineYear1Arm1_ABCD-SST-fMRI_20161213171252.tgz*
  and
  *s3://NDAR_Central_3/submission_21142/sid_removed_2YearFollowUpYArm1_ABCD-T1_20181221131833.tgz*

<!-- end of list -->

Now we filter for baseline images and exclude recalled images (these
have either been replaced or had consent withdrawn, and are not
available).

``` r
fastqc <- fastqc %>%
  filter(visit=="baseline_year_1_arm_1" & ftq_recalled==0)
```

Parse filenames to see what images are present per subject:

``` r
fastqc <- fastqc %>%
  mutate(subjectkey=substr(ftq_series_id,0,15))
pattern <- "(?<=ABCD-).*?(?=_2)"

fastqc$imgtype<-stringr::str_extract(fastqc$ftq_series_id,pattern)

fastqc<-fastqc %>% filter(imgtype=="T1" |
                            imgtype=="T1-NORM" |
                            imgtype=="T2" |
                            imgtype=="T2-NORM" |
                            imgtype=="rsfMRI" |
                            imgtype=="fMRI-FM" |
                            imgtype=="fMRI-FM-PA" |
                            imgtype=="fMRI-FM-AP")

fastqc$imgtype <- as.factor(fastqc$imgtype)

dwndabledf<-fastqc %>%
  group_by(subjectkey, imgtype) %>%
  summarize(usecount=length(ftq_usable)) %>%
  pivot_wider(id_cols=subjectkey,names_from=imgtype,values_from=usecount)
```

    ## `summarise()` has grouped output by 'subjectkey'. You can override using the
    ## `.groups` argument.

``` r
dwndabledf <- dwndabledf %>% mutate(across(where(is.numeric),\(x) replace_na(x,0)))
```

This yields, e.g.:

- **subjectkey**: *sid_removed*, *sid_removed*, *sid_removed*,
  *sid_removed*, *sid_removed* and *sid_removed*
- **fMRI-FM-AP**: *5*, *0*, *0*, *5*, *5* and *6*
- **fMRI-FM-PA**: *5*, *0*, *0*, *5*, *5* and *6*
- **rsfMRI**: *4*, *4*, *4*, *4*, *4* and *4*
- **T1**: *1*, *1*, *1*, *1*, *1* and *1*
- **T1-NORM**: *1*, *0*, *0*, *1*, *1* and *1*
- **T2**: *1*, *1*, *1*, *1*, *1* and *1*
- **T2-NORM**: *1*, *0*, *0*, *1*, *1* and *1*
- **fMRI-FM**: *0*, *5*, *5*, *0*, *0* and *0*

<!-- end of list -->

Participants with useable data will have a useable framemap, rs-fmri
image, and T1. Here we construct a variable based on whether the user
has enough images passing operator QC:

``` r
useabledf<-fastqc %>%
  filter(ftq_usable==1) %>% 
  group_by(subjectkey, imgtype) %>%
  summarize(usecount=length(ftq_usable)) %>%
  pivot_wider(id_cols=subjectkey,names_from=imgtype,values_from=usecount)
```

    ## `summarise()` has grouped output by 'subjectkey'. You can override using the
    ## `.groups` argument.

``` r
useabledf <- useabledf %>% mutate(across(where(is.numeric),\(x) replace_na(x,0)))

useabledf$T1ok<- (useabledf$T1 + useabledf$`T1-NORM`)>0

#rsfmri needs to be present.

useabledf$rsfmriok<-useabledf$rsfMRI>0

#FM needs to be present, or the fm-ap and fm-pa are the same length and not equal to 0.

useabledf$fmok<- (useabledf$`fMRI-FM`>0) | (useabledf$`fMRI-FM-AP` > 0 & (useabledf$`fMRI-FM-AP` == useabledf$`fMRI-FM-PA`))

#We need all three

useabledf <- useabledf %>% 
  mutate(fastqcok = T1ok & rsfmriok & fmok) %>% 
        mutate(across(ends_with("ok"),\(x) replace_na(x,FALSE)))
```

# Inclusion Table

Output a table of participant inclusion flags.

``` r
inclusion <- useabledf %>% 
  select(c(subjectkey,fastqcok,T1ok,rsfmriok,fmok)) %>% 
  mutate(fastqcok=replace_na(fastqcok,FALSE))

saveRDS(inclusion,file="imginclusion.Rds")
```

61, 0, 1662, 10092
