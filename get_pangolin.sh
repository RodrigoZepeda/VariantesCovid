#!/bin/bash
#Se encarga de procesar con pangolin
#https://stackoverflow.com/questions/34534513/calling-conda-source-activate-from-bash-script
eval "$(/usr/local/Caskroom/miniconda/base/bin/conda shell.bash hook)"
mamba activate pangolin
for varname in $(ls fasta)
do
    idname=${varname%.*}
    pangolin --threads 4 --outfile "fasta_processed/$idname.csv" fasta/$varname
done
mamba deactivate

