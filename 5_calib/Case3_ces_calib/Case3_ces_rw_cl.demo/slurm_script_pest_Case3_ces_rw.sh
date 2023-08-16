#!/bin/bash -l
#SBATCH --job-name=pest_Case3_ces_rw
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks-per-core=1
#SBATCH --mem-per-cpu=4G
#SBATCH --time=199:59:00
#SBATCH --output pest_case3_ces_rw%j.out
#SBATCH --error pest_case3_ces_rw%j.err
 
# Change to my work dir
cd $SLURM_SUBMIT_DIR

# Load modules
module load GCCcore/.11.2.0 Python/3.9.6
module load GCC/10.2.0  OpenMPI/4.0.5 SciPy-bundle/2020.11
 
# Run the ats model command within the loop
singularity exec /bigwork/nhgjrabl/Singularity/ats_pest_final_2.sif pestpp-glm Case3_ces_cf_v1_rw.pst &>pest_out.log