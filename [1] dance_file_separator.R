# This script takes all .csv files that start with "20", deletes first 3 rows, cleans the column names, and saves them in seven separate files in seven locations
# This separates the Noraxon data by Orientation, joint movement, trajectories, pitch, roll, course, pelvis
# --------------------------------------------------------------------------
# Required Folder Outline:
#   - output
#     - Course
#     - Joint
#     - Orientation
#     - Pelvis
#     - Pitch
#     - Roll
#     - Trajectories
# Files must be named in format of "DATE_pddancecon006_descriptor"
# --------------------------------------------------------------------------
# Run this script before running dance files through matlab entropy analysis
# --------------------------------------------------------------------------
# 12/9/2019 - Updated instructions
# 2/9/2020 - Added progress bar of sorts

library(tidyverse)
library(xlsx) # write.xlsx()
library(zeallot) # assign to multiple variables
library(xlsx) # convert csv to xlsx

#1. Get list of all csv files that start with "20"
get_files <- function(){
  setwd("C:\\Users\\albei\\Desktop\\dancing\\Entropy Calculation")
  folders <- list.dirs()
  old_files <- vector(mode="list")
  name_or <- vector(mode="list")
  name_joint <- vector(mode="list")
  name_traj <- vector(mode="list")
  name_pitch <- vector(mode="list")
  name_roll <- vector(mode="list")
  name_course <- vector(mode="list")
  name_pelvis <- vector(mode="list")
  
  i = 0
  for (x in folders){
    fold_list = list()
    fold <- list.files(pattern = "^20(.*)csv$", path = x)
    fold_list[[x]] <- fold
    for (y in fold_list[[x]]){
      i = i+1
      
      # to save as .xlsx
      old <- str_c(x,y, sep="/")
      new <- str_replace(y,".csv",".xlsx")
      old_files[i] <- old
      
      # Can't save as .xlsx, too big. Saving as .csv. Convert with csv_converter.py
      new <- str_replace(y,".xlsx",".csv")
      
      name_or[i] <- str_c("./output/Orientation", new, sep = "/")
      name_joint[i] <- str_c("./output/Joint", new, sep = "/")
      name_pitch[i] <- str_c("./output/Pitch", new, sep = "/")
      name_roll[i] <- str_c("./output/Roll", new, sep = "/")
      name_course[i] <- str_c("./output/Course", new, sep = "/")
      name_pelvis[i] <- str_c("./output/Pelvis", new, sep = "/")
      
      name_traj[i] <- str_c("./output/Trajectories", new, sep = "/")
    }
  }
  old_new <- list(old_files, name_or, name_joint, name_traj, name_pitch, name_roll, name_course, name_pelvis)
  return(old_new)
}

#2. Remove unnecessary columns, rename columns
clean_df <- function(df){
  df2 <- df %>%
    select(-2:-3) %>%
    select(1:171) 
  names(df2) <- str_replace_all(names(df2),"Noraxon MyoMotion-","")
  return(df2)
}

#3. Separate files into seven separate dataframes. Inner function.
separate_df <- function(df){
  # Separate into ...
  # ... body and segment orientation
  df.orientation <- df %>%
    select(time, contains("Orientation"))
  names(df.orientation) <- str_replace_all(names(df.orientation),"Segments-","")
  df.orientation <- as.data.frame(df.orientation)
  # ... joint movements
  df.joints <- df%>%
    select(time, contains("Joints-"))
  names(df.joints) <- str_replace_all(names(df.joints),"Joints-","")
  df.joints <- as.data.frame(df.joints)
  # ... bone landmark trajectories
  df.trajectories <- df %>%
    select(time, contains("Trajectories-"))
  names(df.trajectories) <- str_replace_all(names(df.trajectories),"Trajectories-","")
  df.trajectories <- as.data.frame(df.trajectories)
  # ... pitch
  df.pitch <- df %>%
    select(time, contains("-Pitch"))
  names(df.pitch) <- str_replace_all(names(df.pitch),"Segments-","")
  df.pitch <- as.data.frame(df.pitch)
  # ... segment roll
  df.roll <- df %>%
    select(time, contains("-Roll ("))
  names(df.roll) <- str_replace_all(names(df.roll),"Segments-","")
  df.roll <- as.data.frame(df.roll)
  # ... segment course
  df.course <- df %>%
    select(time, contains("-Course ("))
  names(df.course) <- str_replace_all(names(df.course),"Segments-","")
  df.course <- as.data.frame(df.course)
  # ... pelvis movement
  df.pelvis <- df %>%
    select(time, contains("Pelvis-Pelvic "))
  names(df.pelvis) <- str_replace_all(names(df.pelvis),"Segments-Pelvis-","")
  df.pelvis <- as.data.frame(df.pelvis)
  
  # Return as a list
  separated <- list(
    df.orientation = df.orientation,
    df.joints = df.joints,
    df.trajectories = df.trajectories,
    df.pitch = df.pitch,
    df.roll = df.roll,
    df.course = df.course,
    df.pelvis = df.pelvis
  )
  return(separated)
}

#4. Save the movements as different files
save_files <- function(old_files, name_or, name_joint, name_traj, name_pitch, name_roll, name_course, name_pelvis){
  i = 0
  count <- length(old_files)
  for (x in old_files){
    i = i + 1
    df <- read_csv(x, skip = 3)
    df <- clean_df(df) 
    c(df.orientation, df.joints, df.trajectories, df.pitch, df.roll, df.course, df.pelvis) %<-% separate_df(df)
    # Save as dataframes ...
    # ... df.orientation
    name_or_save <- as.character(name_or[i])
    write.csv(df.orientation, name_or_save, row.names = FALSE)

    # ... df.joints
    name_joint_save <- as.character(name_joint[i])
    write.csv(df.joints, name_joint_save, row.names = FALSE)

    # ... df.trajectories
    name_traj_save <- as.character(name_traj[i])
    write.csv(df.trajectories, name_traj_save, row.names = FALSE)

    # ... df.pitch
    name_pitch_save <- as.character(name_pitch[i])
    write.csv(df.pitch, name_pitch_save, row.names = FALSE)

    # ... df.roll
    name_roll_save <- as.character(name_roll[i])
    write.csv(df.roll, name_roll_save, row.names = FALSE)

    # ... df.course
    name_course_save <- as.character(name_course[i])
    write.csv(df.course, name_course_save, row.names = FALSE)

    # ... df.pelvis
    name_pelvis_save <- as.character(name_pelvis[i])
    write.csv(df.pelvis, name_pelvis_save, row.names = FALSE)
    print(str_c("||",i, "/", count, " FINISHED! || ", x, sep=" "))
  }
  print("DANCE FILES SEPARATED")
}

#5. Starts the functions above
start <- function(){
  # Run get_files()
  c(old_files, name_or, name_joint, name_traj, name_pitch, name_roll, name_course, name_pelvis) %<-% get_files()
  # Run everything else
  save_files(old_files, name_or, name_joint, name_traj, name_pitch, name_roll, name_course, name_pelvis)
}

start()
