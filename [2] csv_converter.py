# 12.9.2019 - Updated script to include a progress bar of sorts

import pandas as pd
import glob
import os
import sys

path = "C:/Users/albei/Desktop/dancing/Entropy Calculation/output/*"
all_files = glob.glob(os.path.join(path, "*.csv"))  # make a list of paths
all_files
i = 0
shape = len(all_files)

for files in all_files:
  dir_path = os.path.dirname(os.path.realpath(files)) # get the csv file's current directory
  csv_file = os.path.splitext(os.path.basename(files))[0]  # get file name without extension
  xlsx_file = dir_path + "\\"+ csv_file + ".xlsx" # save csv file in current directory
  pd.read_csv(files, delimiter= ",").to_excel(xlsx_file, index=False)
  i = i + 1
  print("||",i, "/", shape, " FINISHED! || ", csv_file)

