import os
import pandas as pd
import re
import glob

def abcd_run_from_filename(input_str):
    pattern = r'sub-(NDARINV........)_ses-baselineYear1Arm1_task-([a-zA-Z]+)_run-(\d)_desc-filteredincludingFD_motion\.tsv'
    match = re.search(pattern, input_str)
    if match:
        return [match.group(1),match.group(2) + '_' + match.group(3)]
    else:
        return None

def get_file_info(filename):
    result = []
    # Check if file exists
    pid, runid = abcd_run_from_filename(filename)
    if not os.path.isfile(filename):
        result.append(pid)
        result.append(runid)
        result.append(False)
        result.append(None)
        return pd.DataFrame([result], columns=["pid", "Run", "Exists", "Average"])
    
    # Read file and calculate average of last column
    with open(filename, 'r') as f:
        next(f)
        lines = f.readlines()
        last_col = [float(line.strip().split()[-1]) for line in lines]
        avg_last_col = sum(last_col) / len(last_col)
    
    # Append file info to result
    result.append(pid)
    result.append(runid)
    result.append(True)
    result.append(avg_last_col)
    return pd.DataFrame([result], columns=["pid", "Run", "Exists", "Average"])

#Make a list of the possible beginnings of the files we want (by subject id)
pattern='abccbids/fmriresults01/derivatives/abcd-hcp-pipeline/sub-NDARINV*'
sdirs=glob.glob(pattern) 
tfiles_prefix = []
for d in sdirs:
  sid= os.path.basename(d)
  tfiles_prefix.append(d+"/ses-baselineYear1Arm1/func/"+sid+"_ses-baselineYear1Arm1_task-")
  
#Now make a list of the possible endings (i.e. all possible runs)
#This was generated in bash:
tfiles_suffix = ['MID_run-1_desc-filteredincludingFD_motion.tsv',
             'MID_run-2_desc-filteredincludingFD_motion.tsv',
             'SST_run-1_desc-filteredincludingFD_motion.tsv',
             'SST_run-2_desc-filteredincludingFD_motion.tsv',
             'nback_run-1_desc-filteredincludingFD_motion.tsv',
             'nback_run-2_desc-filteredincludingFD_motion.tsv',
             'rest_run-1_desc-filteredincludingFD_motion.tsv',
             'rest_run-2_desc-filteredincludingFD_motion.tsv',
             'rest_run-3_desc-filteredincludingFD_motion.tsv',
             'rest_run-4_desc-filteredincludingFD_motion.tsv']
  
# Stitch together the lists for a list of all possible functional runs given
# the currently downloaded subjects.
tfiles= [s1 + s2 for s1 in tfiles_prefix for s2 in tfiles_suffix] 

# Now we look at the motion data:
# Initialize an empty list to store the dataframes
df_list = []

# Iterate over the file list and apply the get_file_info function to each file
for file in tfiles:
    file_info = get_file_info(file)
    if file_info is not None:
        df_list.append(file_info)

# Concatenate the resulting list of dataframes into a single dataframe
if len(df_list) > 0:
    result_df = pd.concat(df_list, ignore_index=True)
else:
    result_df = pd.DataFrame()

result_df.to_csv("tmp/AllFD",na_rep="999",index=False)
