import pandas as pd
import glob
import os
import sys

path = "Separate_dfs/*"
all_files = glob.glob(os.path.join(path, "*.csv"))  # make a list of paths
all_files

for files in all_files:
  dir_path = os.path.dirname(os.path.realpath(files)) # get the csv file's current directory
  csv_file = os.path.splitext(os.path.basename(files))[0]  # get file name without extension
  xlsx_file = dir_path + "\\"+ csv_file + ".xlsx" # save csv file in current directory
  pd.read_csv(files, delimiter= ",").to_excel(xlsx_file, index=False)
  print(csv_file, "FINISHED!")
