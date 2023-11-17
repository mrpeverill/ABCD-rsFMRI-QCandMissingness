#!/bin/bash
set -e

for i in /projects/abcd_data/abccbids/NDARINV*tar.xz; do
    echo "Reading $i"
    tar -tJf $i >> localmanifest.txt
done

grep -oE 'NDARINV[0-9A-Z]{8}' localmanifest.txt | sort | uniq > tmp/subjectsinmanifest
grep -oE 'NDARINV[0-9A-Z]{8}' filtered_manifest.txt | sort | uniq > tmp/subjectsintargets
echo "NDARINVPEVERILL" >> tmp/subjectsintargets
comm -23 tmp/subjectsintargets tmp/subjectsinmanifest > remedialsubjects
echo "DONE"
