#!/bin/bash

# directory to store sorted BAMs
mkdir -p sorted_bams

# Look at BAM files in the current directory
for bam_file in *.bam; do
    if [ -e "$bam_file" ]; then
        # Get filename without .bam
        filename="${bam_file%.bam}"

        # Sort BAM file and save into the new directory
        samtools sort -o "sorted_bams/sort_$filename.bam" "$bam_file"
        samtools index "sorted_bams/sort_$filename.bam"

        echo "Sorted: $bam_file"
    else
        echo "File not found: $bam_file"
    fi
done

# convert bam to VCF
for sorted_bam in sorted_bams/*.bam; do
    if [ -e "$sorted_bam" ]; then
        # get filename without .bam
        filename="${sorted_bam%.bam}"

        # convert for each bam 
        samtools mpileup -uf reference.fasta "$sorted_bam" | bcftools call -mv -o "$filename.vcf"
        echo "Variant calling: $sorted_bam"
    else
        echo "Sorted BAM not found: $sorted_bam"
    fi
done

