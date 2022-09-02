The folder contains the raw dataset, jupyter notebooks for processing the dataset, final input and output datasets for the different simulations.

The folder structure:
1. Raw dataset (Final_data_excelsheet): The data was downloaded from the ITP website. It contains weather station data from 2015-20. 
2. AWS_final_metdata_4ys_concise_spinup.ipynb: It prepares the 10 year input data for the dynamic spin-up.
3. Input_data_Spinup: Stores the processed data from 2 - Spinup_input_2017_10yrs.h5, Spinup_input_2017_10yrs_smooth.h5 (Smoothened dataset)
4. AWS_final_metdata_4ys_concise.ipynb: It processes the raw data and produces input and output datasets suitable for calibration and validation. 
5. AWS_final_metdata_4ys_concise_cyclicavg.ipynb: It processes the raw data and produces input and output datasets suitable for calibration and validation (Dont consider 4?). 
6. Final_InputData_4yrs: Input data for calibration and validation.
7. Final_OutputData_4yrs: Output data for calibration and validation.
8. AWS_final_metdata_Case3.ipynb: It processes the raw data and produces input and output datasets suitable for calibration and validation for Case3 (Since it considers only 2016 and 2017)
9. Final_InputData_Case3: Input data for calibration and validation - Case3
10. Final_OutputData_Case3: Output data for calibration and validation - Case3