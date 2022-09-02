import os
import subprocess
import shutil
import fileinput
import time
import re
import pandas as pd

# The file name, porosity of peat and mineral line has to be identified the porosity - Therefore change when the .xml file changes
file_name = 'Case5_III_su_cv' # Change
line_por_peat = 588 # Change (-1 of the line where porosity of peat is found)
line_por_mineral = 597 # Change (-1 of the line where porosity of mineral is found)

# Removing the previously generated output file
os.system(f"rm -rf {file_name}_obs_data.dat")


# Running ats command
#os.system(f"singularity exec /bigwork/nhgjrabl/Singularity/ats_pest_final_2.sif ats --xml_file={file_name}.xml")
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

depth_peat = 0.30

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
