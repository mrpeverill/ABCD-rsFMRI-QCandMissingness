#!/usr/bin/python3

# Utility to generate a credential file. Writes to credfile in the current directory.

import base64
import sys

try:
    credstring=sys.argv[1]+":"+sys.argv[2]
except IndexError as e:
    print("Needs two arguments. Usage: './gencreds.py (USERNAME) (PWD)' where PWD is your NDA (not era) password")
    exit(1)

credentials = base64.b64encode(bytes(credstring,encoding="utf-8")).decode('utf-8')
try:
    with open("credfile","w+") as f:
        f.write(credentials)
except Exception as e:
    print(str(e))

