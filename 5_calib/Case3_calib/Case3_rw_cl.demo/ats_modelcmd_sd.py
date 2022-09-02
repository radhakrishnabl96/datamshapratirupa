import os
import subprocess
import shutil
import fileinput
import time
import re
import pandas as pd
import numpy as np
import h5py

# The file name, porosity of peat and mineral line has to be identified the porosity - Therefore change when the .xml file changes
file_name = 'Case5_III_C_cv' # Change
line_por_peat = 602 # Change (-1 of the line where porosity of peat is found)
line_por_mineral = 611 # Change (-1 of the line where porosity of mineral is found)
line_sd = 1011 # Change (-1 of the line where snow density is found)

# 1. To find the snow density from the xml file
filename = f'{file_name}.xml'

with open(f'{filename}') as oldfile:
            for line, content in enumerate(oldfile):
                if line == line_sd: 
                    sd_line = str(content)
                    result = re.findall('\".*?\"', sd_line)
                    #print(result)
                    snow_dens_ratio = float(result[2].replace('"',''))
                    #print(snow_density_ratio)



# Snow depth is also a input parameter - data collected from Dr. Xiao's article
Snow_depth = pd.read_excel('/home/rk/ats_rk/testing/ats-demos/rk_model/Data/Data_Yakou/Yakou_met_data_ITP_rk/Final_data_excelsheet/Yakou_metstation_data_2017_snowdepth.xlsx',sheet_name='snow_depth',index_col=0, parse_dates=True) 

# Filling the snow depth missing data with an average value
Snow_depth = Snow_depth.fillna(np.average(Snow_depth.loc['2017-12']))
# Snow depth is also a input parameter - data collected from Dr. Xiao's article
Snow_depth = pd.read_excel('/home/rk/ats_rk/testing/ats-demos/rk_model/Data/Data_Yakou/Yakou_met_data_ITP_rk/Final_data_excelsheet/Yakou_metstation_data_2017_snowdepth.xlsx',sheet_name='snow_depth',index_col=0, parse_dates=True) 

# Filling the snow depth missing data with an average value
Snow_depth = Snow_depth.fillna(np.average(Snow_depth.loc['2017-12']))

# Concatatenating all the dataframes for the year 2016, 2017, 2018, 2019.

snow_depth_2016_2019 = np.concatenate([Snow_depth['Snow depth (m)'].values, Snow_depth['Snow depth (m)'].values, Snow_depth['Snow depth (m)'].values, Snow_depth['Snow depth (m)'].values])

# Note: The year 2016 has 366 days, hence I am adding the Dec 31st value once more
snow_depth_2016_2019 = np.insert(snow_depth_2016_2019, 366, Snow_depth['Snow depth (m)']['2017-12-31'])

# Creating a dataframe with four years of snow depth data

# The snow depth dataframe with the same index
Snow_depth_4_yrs = pd.DataFrame(index=pd.date_range(start='1/1/2016',end='31/12/2019',freq='D'),data = snow_depth_2016_2019, columns=['Snow depth (m)'])

# Calculating the snow water equivalent
Snow_depth_4_yrs['precipitation snow [m SWE s^-1]'] = Snow_depth_4_yrs['Snow depth (m)']*(snow_dens_ratio/86400)

# Storing the data in a seperate dataframe
data_SWE_input_calib = Snow_depth_4_yrs['2016':'2018']

# Opening the input file
hf_input = h5py.File('../Snow_depth_data/SWE_2016_2018.h5','w')

# Creating a variable with time instances for 1096 days
no_days = len(data_SWE_input_calib) # Total number of days
no_secs_day = 86400 # Total number of seconds in a day
times = np.arange(0,no_days*no_secs_day,no_secs_day)
len(times)

# Changing the storage to numpy array
hf_input.create_dataset('time [s]', data=times)
hf_input.create_dataset('precipitation snow [m SWE s^-1]', data=data_SWE_input_calib['precipitation snow [m SWE s^-1]'].values)

hf_input.close()


# Removing the previously generated output file
os.system(f"rm -rf {file_name}_obs_data.dat")


# Running ats command
os.system(f"ats --xml_file={file_name}.xml")


# Generating a suitable observation file with no hash!
remove_hashs = ['#']

old_file_name = 'observations.dat'
new_file_name = f'{file_name}_obs_data_mois.dat'
with open(f'{old_file_name}') as oldfile, open(f'{new_file_name}', 'w') as newfile:
            for line in oldfile:
                if not any(remove_hash in line for remove_hash in remove_hashs):
                    newfile.write(line)
                    

                    
# 1. To find the porosity
filename = f'{file_name}.xml'

with open(f'{filename}') as oldfile:
            for line, content in enumerate(oldfile):
                if line == line_por_peat: # Line 603 (+1) has the porosity_peat 
                    poro_peat_line = str(content)
                    result = re.findall('\".*?\"', poro_peat_line)
                    poro_peat = float(result[2].replace('"',''))
                    #print(poro_peat)
                elif line == line_por_mineral: # Line 612 (+1) has the porosity_mineral
                    poro_mineral_line = str(content)
                    result_2 = re.findall('\".*?\"', poro_mineral_line)
                    poro_mineral = float(result_2[2].replace('"',''))
                    #print(poro_mineral)

                    
# 2. To find the saturation of liquid

obs_data = f'{file_name}_obs_data_mois.dat'

file_data = pd.read_csv(obs_data,sep=' ', index_col='time [s]')



depths = [0.04, 0.1, 0.2, 0.4, 0.8, 1.2, 1.6]

depth_peat = 0.385

for i, depth in enumerate(depths):
    if depth < depth_peat:
        file_data[f'point -{depth} saturation liquid'] = file_data[f'point -{depth} saturation liquid']*(poro_peat*100)
    else:
        file_data[f'point -{depth} saturation liquid'] = file_data[f'point -{depth} saturation liquid']*(poro_mineral*100)

file_data.to_csv(f'{file_name}_obs_data_2.dat',sep=' ',index=True)


# Removing the top row since it is not suitable with instruction file
remove_hashs = ['"']

old_file_name = f'{file_name}_obs_data_2.dat'

new_file_name = f'{file_name}_obs_data.dat'

with open(f'{old_file_name}') as oldfile, open(f'{new_file_name}', 'w') as newfile:
            for line in oldfile:
                if not any(remove_hash in line for remove_hash in remove_hashs):
                    newfile.write(line)


# Remove all the unnescessary output files expect the .dat file
os.system(f"rm -rf *.xmf *.h5 observations.dat {file_name}_obs_data_mois.dat {file_name}_obs_data_2.dat")
