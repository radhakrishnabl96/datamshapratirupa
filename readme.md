This folder contains all the relevant materials required for running the simulations with respect to the first Article. Note that ATS, PEST, and PESTPP softwares have to be downloaded. 
Each folder always contains three sub-folders or cases (based on the top boundary condition): 
1. Case1: Subsurface temperature at 0.0 m 
2. Case2: Surface energy balance without snow
3. Case3: Surface energy balance with snow  

The folder structure:

1. Data preparation (1_data_prep): To prepare the input and output datasets for the simulations. It also contains the raw datasets.
2. Trial ats files (2_trial_ats): To test the validity of input files and data. It contains the raw input XML files. 
3. Spin-up (3_spinup): It generates the initial conditions for the calibration process. It contains two stages: Static (Considering only Case1) and Dynamic spinup
4. Sensitivity analysis (4_sens_analy): To identify the parameters of interest that will be considered for the calibration process. (Do I consider the manual SA or results from PESTPP-GLM?)
5. Calibration (5_calib): To calibrate the sensitized model parameters 
6. Validation (6_valid): To validate the model results, determine the model efficiency and interpretation.
7. Cluster setup (7_cluster): To run the model on the LUIS cluster, Singularity was downloaded, and an image file with all the respective software (ATS, pest, ..) was installed. 
Basic template files are stored here.
8. Reference files (8_ref): Relevant reference materials to better understand the topic. It includes presentations, articles, images .. etc.
9. Tables and figures for the first article (9_article1_FigsTables): A jupyter notebook is used to prepare the tables and figures used in the first article. The tables and figures are also stored in the folder. 
10. Final simulation (10_finalsim): This folder contains the simulations carried out for one year with cyclic average data (as there is no other yearly data) to conduct a final comparison analysis after validation.


An article is currently under the process of submission; until the article is published, the GitHub repository needs to be cited as - [![DOI](https://zenodo.org/badge/531972787.svg)](https://zenodo.org/badge/latestdoi/531972787)


The results were also published in EGU Conference 2023: Bangalore Lakshmiprasad, R., Graf, T., Zhang, F., Xiao, X., and Coon, E. T.: A comparison study of process complexity in permafrost dominated regions, EGU General Assembly 2022, Vienna, Austria, 23–27 May 2022, EGU22-7266, https://doi.org/10.5194/egusphere-egu22-7266, 2022.
