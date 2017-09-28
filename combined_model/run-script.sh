#!/bin/bash
#SBATCH -n 48
#SBATCH -N 1     # Number of cores
#SBATCH --job-name=test-cores
#SBATCH --output=out-%j.txt
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --time=10:00
#SBATCH --mail-type=END  	  # Type of email notification- BEGIN,END,FAIL,ALL
#SBATCH --mail-user=jdeleeuw@vassar.edu

sleep 10
Rscript ~/model/test-cores.R
