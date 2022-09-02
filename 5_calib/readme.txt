This folder contains all the relevant materials for calibrating the Case1, Case2, and Case3 models. ATS, PEST, PESTPP needs to be downloaded.

1. Case1_calib (Subsurface temperature at 0.0 m)
2. Case2_calib (Surface energy balance without snow)
3. Case3_calib (Surface energy balance with snow)
4. Final_comp_cases_data (The material for comparing the results of the three cases)
5. Output_comp_analysis.ipynb: It is a jupyter notebook to compare the results from the three cases. 
6. Freyberg_example: This folder was taken from the pestpp github repository. It contains the templates for pest files.
7. all_params_afterrw.demo: The pest analysis was conducted several times with different parameter initial values and ranges. The results from one of the pest analysis is stored here. They are used to generate the par_data file. 
8. Case5_I_E_suitable_params.csv: Parameter ranges were decided such that the simulations can run within a short period [within physical and simulation range]. 



