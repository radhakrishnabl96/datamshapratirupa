#!/bin/bash -l
#SBATCH --job-name=pest_case5_III
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks-per-core=1
#SBATCH --mem-per-cpu=4G
#SBATCH --time=199:59:00
#SBATCH --partition=amo,enos
#SBATCH --constraint=[skylake|haswell]
#SBATCH --output pest_case5_start_%j.out
#SBATCH --error pest_case1_start_%j.err
 
# Change to my work dir
cd $SLURM_SUBMIT_DIR

# Load modules
module load GCCcore/.11.2.0 Python/3.9.6
module load GCC/10.2.0  OpenMPI/4.0.5 SciPy-bundle/2020.11
 
# Run the ats model command within the loop
singularity exec /bigwork/nhgjrabl/Singularity/ats_pest_final_2.sif pest Case5_III_su_cv_cf_v1.pst &>pest_out.log