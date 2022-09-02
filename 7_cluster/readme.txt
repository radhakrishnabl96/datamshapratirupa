This folder contains the files required to download ats+pest+pestpp using singularity & template files for running slurm scripts in the cluster.

1. ats_s : Contains the ats version (1.1) that I used for running the software.
2. pest_setup : Contains the files for setting up pest and pestpp
3. sing_ats_pest.recipe : Contains the singularity recipe file which makes use of the above two folders to download ats+pest+pestpp
4. slurm_ats_cl_template.sh : A template slurm script to run ats
5. slurm_script_pest_template.sh : A template slurm script to run pest  