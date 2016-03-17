#!/bin/tcsh

#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20
#SBATCH -t 10:00:00
#SBATCH -J testjob
#SBATCH -o testjob.out
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mail-user=eleanor.brush@gmail.com

. ~/.profile

module load R

mkdir -p /tmp/ebrush/$SLURM_JOB_ID

echo '-------------------------------'
${SLURM_SUBMIT_DIR}
echo ${SLURM_SUBMIT_DIR}
echo Running on host $(hostname)
echo Time is $(date)
echo SLURM_NODES are $(echo ${SLURM_NODELIST})
echo '-------------------------------'
echo -e '\n\n'

R --no-save < testjob.R

rm -rf /tmp/ebrush/$SLURM_JOB_ID