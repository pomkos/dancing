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

## Dance Entropy Clean

* Written in R
* Takes a Matlab script's output (csv, containing entropies of Noraxon raw outputs) and reorganizes it:
    * Gets date, participant code, dance type. Puts them in respective columns
    * Identifies and labels PD and control groups
    * Replaces 0 with NA
    * Cleaned dfs saved as csv files

## Dance Entropy Graph
* Written in R
* ksu_theme variable created for ggplot
* Four graphs created with ksu_theme
* Two scatter plots created, with two sources for each, and a custom legend for each
* All graphs saved as pdf files

## Dance Entropy Stats
* Written in R
* Gets mean and standard deviation of entropies (manually put in desired variable)
* Scripts for t-tests
* Scripts for ANOVA 
   * Scripts for checking ANOVA eligibility
