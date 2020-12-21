#!/bin/bash
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
echo "R is about to run"
R --vanilla < $HOME/ykl17_HPC_2020_cluster.R
mv ykl17_cluster_output* $HOME
echo "R has finished running"