import pandas as pd
import numpy as np
import glob
import sys
import os

thresh=sys.argv[1]
#thresh="0.2"
print("# Outputting QC-FC matrix at threshold "+thresh)

sfile=open("./tmp/rpconnfiles.txt","r")
sdata=sfile.read()
slist=sdata.strip().split("\n")

print("# Reading pconn files")
#This shows up nans in some data files. So we need some fancier process that detects those and issues warnings for handling.
pconnfiles=["./abccbids/derivatives/"+sn+"/pconntxt/"+sn+"-threshold"+thresh+".pconn.txt" for sn in slist]
pconns = pd.DataFrame(columns=range(143641))
count=0
for i in pconnfiles:
    count += 1
    if count % 10 == 0:
        print("Reading pconn %s out of %s" % (count, len(pconnfiles)))
    r=np.loadtxt(i,delimiter=",").ravel()
    if np.isnan(r).any():
        print(f"File {os.path.basename(i)} contains {np.isnan(r).sum()} NaNs.")
    if r.shape != (143641,):
        print(f"File {os.path.basename(i)} is shape {r.shape} instead of (143641,). Skipping")
        continue
    pconns.loc[count-1] = r

print("# Reading FD Files")
fdfiles=["./abccbids/derivatives/"+sn+"/AllFD" for sn in slist]

meanFD = []
for file in fdfiles:
    df=pd.read_csv(file)
    filtered_df=df[df.iloc[:,1].str.startswith("rest_")]
    meanFD.append(filtered_df.iloc[:,3].mean())

FDdf=pd.DataFrame(meanFD)

outmat=pconns.corrwith(FDdf[0]).to_numpy().ravel().reshape((379,379))


# # nan's are on the diagonal only. Verify with:
# # np.fill_diagonal(test,1)
# # np.argwhere(np.isnan(test))

np.savetxt("./QCFCthresh"+thresh+".csv",outmat,delimiter=",")
