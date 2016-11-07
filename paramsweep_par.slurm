#!/bin/tcsh

#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH -t 90:00:00
#SBATCH -J bvr_paramsweep
#SBATCH -o bvr_paramsweep.out
# #SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mail-user=eleanor.brush@gmail.com

module load R

# mkdir -p /tmp/ebrush/$SLURM_JOB_ID

echo '-------------------------------'
date
echo 'Current directory:'
pwd
echo 'Running on'
hostname
echo '-------------------------------'

R --no-save < paramsweep_par.R

# rm -rf /tmp/ebrush/$SLURM_JOB_ID
