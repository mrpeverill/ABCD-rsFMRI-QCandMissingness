## Authentication from https://github.com/NDAR/nda-data-access-example-code/blob/main/example/PYTHON.md
import os
import sys
import base64
import requests
import json
import urllib.request
import shutil
import logging
from pathlib import Path

logging.basicConfig(level=logging.INFO)
# Encode our credentials then convert it to a string.

#credentials are included in credfile. gencreds.py can make this file for you.
with open('credfile', 'r') as file:
    credentials=file.read() 

# Create the headers we will be using for all requests.
headers = {
    'Authorization': 'Basic ' + credentials,
    'User-Agent': 'Example Client',
    'Accept': 'application/json'
}

# Send Http request

logging.debug('Auth request')
response = requests.get('https://nda.nih.gov/api/package/auth', headers=headers)

# If the response status code does not equal 200
# throw an exception up.
if response.status_code != requests.codes.ok:
    print('failed to authenticate')
    response.raise_for_status()

# The auth endpoint does no return any data to parse
# only a Http response code is returned.


## 'From S3 URL'
import csv

# Assume code in authentication section is present.

packageId = 1207036

s3Files = []

# Load in and process the manifest file.
# Not all manifest files are structured like this, all you require is
# an S3 url and a package that has the files associated with it.
logging.debug('constructing s3 pathss object')
with open('tmp/filtered_paths.txt', 'r') as manifest:
    for line in manifest:
        if line.startswith('s3://'):
            s3Files.append(line.rstrip())

logging.debug(s3Files[:10])

# Construct the request to get the files of package
# URL structure is: https://nda.nih.gov/api/package/{packageId}/files
logging.debug('Posting file request')
response = requests.post('https://nda.nih.gov/api/package/' + str(packageId) + '/files', json=s3Files, headers=headers)

# Business Logic.

files = {}

# Add important file data to the files dictionary.
# We can skip having to transform the json because a json array is returned.
#print(response.json())
for f in response.json():
    files[f['package_file_id']] = {'name': f['download_alias'],'size': f['file_size']}

logging.debug('got file list:')
logging.debug(dict(list(files.items())[:3]))

## Downloading files
# Create a post request to the batch generate presigned urls endpoint.
# Use keys from files dictionary to form a list, which is converted to
# a json array which is posted.

logging.debug('Posting S3 presigned url request')
response = requests.post('https://nda.nih.gov/api/package/' + str(packageId) + '/files/batchGeneratePresignedUrls', json=list(files.keys()), headers=headers)

logging.debug(response)
# Get the presigned urls from the response.
results = response.json()['presignedUrls']

logging.debug(results[:3])
# Business Logic.

# Add a download key to the file's data.
for url in results:
    files[url['package_file_id']]['download'] = url['downloadURL']

logging.debug('adding download key to file data')
logging.debug(dict(list(files.items())[:3]))

# Iterate on file id and it's data to perform the downloads.
#for id, data in files:
for index, (id, data) in enumerate(files.items()):
    name = data['name']
    downloadUrl = data['download']
    predsize = data['size']
    # Create a downloads directory
    file = './abccbids/' + name
    # Strip out the file's name for creating non-existent directories
    directory = file[:file.rfind('/')]
    
    # Create non-existent directories, package files have their
    # own directory structure, and this will ensure that it is
    # kept in tact when downloading.
    Path(directory).mkdir(parents=True, exist_ok=True)
    
    # Initiate the download.
    with urllib.request.urlopen(downloadUrl) as dl, open(file, 'wb') as out_file:
        print("Downloading %s..." % out_file)
        shutil.copyfileobj(dl, out_file)
    sizeondisk=os.path.getsize(file)
    if sizeondisk!=predsize:
        print("Error: File is %s bytes, %s predicted" % (sizeondisk,predsize))
        os.remove(file) # If the check failed, remove the file.
        sys.exit(1)
print("Download Complete")
