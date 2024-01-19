#!/bin/bash

reference_index=$(pwd)

# align each fastq file to reference
for i in {0..4}; do
bowtie2 -x reference_index -U ../test_data/condition1/fastqs/*/condition1_sample${i}_merged.fastq -S condition1sample${i}.sam
    for i in {0..4}; do
    bowtie2 -x reference_index -U ../test_data/condition2/fastqs/*/condition2_sample${i}_merged.fastq -S condition2sample${i}.sam
    done 
done
 
