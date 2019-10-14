# Description
Scripts to separate and clean Noraxon raw outputs, csv -> xlsx converter, and analyze entropy from Matlab output.

## Dance File Separator

* Written in R
* Separates raw Noraxon output into 7 files
    * Orientation
    * Trajectories
    * Joint Movements
    * Pitch
    * Roll
    * Course
    * Pelvic Movements
* Output is under Separate_dfs folder, one of the 7 subfolders

## CSV Converter

* Written in Python
* Simple script to convert all .csv files in all subdirectories to .xslsx files
* Output is in the respective subdirectories

## Dance Entropy Clean Analyze

* Written in Python
* Takes a Matlab script's output (csv, containing entropies of Noraxon raw outputs) and reorganizes it:
    * Gets date, participant code, dance type. Puts them in respective columns
    * Identifies and labels PD and control groups
    * Gets mean and standard deviation of entropies (manually put in desired variable)
    * Contains example code for ggplot plotting