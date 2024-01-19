#!/bin/bash


for vcf_file in sorted_bams/*.vcf.gz; do
    if [ -e "$vcf_file" ]; then
        # copy filename 
        filename="${vcf_file%.vcf}"

        # create a directory matching the filename
        mkdir -p "vcf_comparison_results/$filename"

        # comparison 
        bcftools isec -n=2 -p "vcf_comparison_results/$filename" "$vcf_file" ../true_positives.vcf.gz

        echo "Processed: $vcf_file"
    else
        echo "File not found: $vcf_file"
    fi
done
