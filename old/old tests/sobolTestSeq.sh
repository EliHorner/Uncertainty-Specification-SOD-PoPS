#!/bin/bash
#BSUB -n 1
#BSUB -W 80:00
#BSUB -q cnr
#BSUB -R "rusage[mem=32GB]"
#BSUB -J pops_pro
#BSUB -oo pops_out
#BSUB -eo popserr
module load conda
eval "$(conda shell.bash hook)"
conda activate /usr/local/usrapps/rkmeente/eahorner/pops_env
Rscript ./hpc_sobol_sod_seq_1yr.R
conda deactivate