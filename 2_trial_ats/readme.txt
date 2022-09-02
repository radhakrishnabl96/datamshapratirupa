This folder contains all the relevant materials required for running the simulations with respect to the first Article. Note that ATS, PEST, PESTPP softwares has to be downloaded. 
Each individual folder always contains three cases (based on the top boundary condition): 
1. Case1: Subsurface temperature at 0.0 m 
2. Case2: Surface energy balance without snow
3. Case3: Surface energy balance with snow  

The folder structure:
1. Simulations:
    a. Data preparation (data_prep): To prepare the input and output datasets for the simulations. It also contains the raw datasets
    b. Trial ats files (trial_ats): To test the validity of input files and data. It contains the raw input xml files. 
    c. Spin-up (spinup): It generates the initial conditions for the calibration process. It contains two stages: Static (Considering only Case1) and Dynamic spinup
    d. Sensitivity analysis (sens_analy): To identify the parameters of interest which will later on be considered for the calibration process. (Do I consider the manual SA or results from PESTPP-GLM?)
    e. Calibration (calib): To calibrate the sensitized model parameters 
    f. Validation (valid): To validate the model results, determine the model efficiency, and interpretation of the results.