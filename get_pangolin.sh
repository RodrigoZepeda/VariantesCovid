#!/bin/bash
#Se encarga de procesar con pangolin
#https://stackoverflow.com/questions/34534513/calling-conda-source-activate-from-bash-script
eval "$(/home/rodrigo/miniconda3/bin/conda shell.bash hook)"
conda activate pangolin
for varname in $(ls fasta)
do
    idname=${varname%.*}
    pangolin --threads 4 --outfile "fasta_processed/$idname.csv" fasta/$varname
done
conda deactivate

