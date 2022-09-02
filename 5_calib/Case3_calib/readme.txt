It contains all the files necessary for Case3 calibration. The calibration process can be divided into start and reweighting case. 
The start case is used for determining the weights to be provided for temperature and moisture observations. This is to ensure equal weightage for both temperature and moisture.
The rw case is used for the final PEST analysis. 

Folder structure:
1. Case3_pestpp_filesprep.ipynb: A jupyter notebook with code necessary to generate the input files for the PEST analysis. 
2. Case3_output_analysis.ipynb: A jupyter notebook with code to extract the results from the PEST analysis.
3. Case3_start_cl.demo: All PEST files necessary to conduct the start analysis is present here.
4. Case3_rw_cl.demo: All PEST files necessary to conduct the final PEST analysis is present here.
5. Case3_dynamic_spinup.xml: A template xml file for Case 3.

Note: The simulations takes a longer period of time,therefore they were executed in the LUIS cluster. 
