#!/bin/bash -l
#SBATCH -A snic2015-6-12
#SBATCH -p node -n 1
#SBATCH -t 00:20:00
#SBATCH -J parsing

./Main 
