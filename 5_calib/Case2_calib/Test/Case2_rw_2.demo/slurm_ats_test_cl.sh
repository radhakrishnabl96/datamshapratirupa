#!/bin/bash -l
#SBATCH --job-name=ats_case5_trial
#SBATCH --nodes=1
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=4G
#SBATCH --time=00:30:00
#SBATCH --partition=amo,enos
#SBATCH --constraint=[skylake|haswell]
#SBATCH --output ats_case5_%j.out
#SBATCH --error ats_case1_rw_%j.err
 
# Change to my work dir
cd $SLURM_SUBMIT_DIR/test_ats_cl.demo

# Load modules
module load GCCcore/.11.2.0 Python/3.9.6
module load GCC/10.2.0  OpenMPI/4.0.5 SciPy-bundle/2020.11
 
# Run the ats model command within the loop
singularity exec /bigwork/nhgjrabl/Singularity/ats_pest_final_2.sif ats --xml_file=../Case5_su_cv.xml