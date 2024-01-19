#!/bin/bash


# convert sam to bam
for i in *.sam; do
    if [ -e "$i" ]; then
        bam_file="${i%.sam}.bam"
        echo "Converting: $i to $bam_file"
        samtools view -bS "$i" > "$bam_file"
    else
        echo "File not found: $i"
    fi
done

