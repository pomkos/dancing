# This script cleans and analyzes one dance entropy file (from Matlab)

library(tidyverse)
library(lubridate)
# library(Hmisc) I don't know what this is for

# 1. LOADING SECTION ####

setwd("C:/Users/albei/Documents/github/dancing")

df_clean_path = "C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/clean_samen.csv"
df_analyze_path = "C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/analyze_samen.csv"


df <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/SamEn_joints.csv")

# 2. VARIABLE CLEANING SECTION ####
glimpse(df)
df_clean <- df %>%
  # Extract appropriate entropy
  select(-useless_col) %>%
  # Create Date column
  mutate(Date = str_extract(string = file_name, pattern = "^20..............")) %>%
  mutate(Date = ymd_hm(Date)) %>%
  # Create Participant column
  mutate(Participant = str_extract(file_name, "pddance[0-9][0-9][0-9]|pddancecon[0-9][0-9][0-9]")) %>%
  mutate(Participant = factor(Participant)) %>%
  # Create Dance_Type column
  mutate(file_name = str_remove(file_name,".*pddance[0-9][0-9][0-9]_")) %>%
  mutate(file_name = str_remove(file_name,".*pddancecon[0-9][0-9][0-9]_")) %>%
  mutate(Dance_Type = str_remove(file_name,".xlsx")) %>%  
  # Reorder, remove duplicates
  select(Date, Participant, Dance_Type, ends_with("(deg)")) %>% # EDIT ME
  # Clean Dance column
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*Tango.*|.*tango.*", replacement = "Tango")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*Waltz.*|.*waltz.*", replacement = "Waltz")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*Waltz.*|.*waltz.*", replacement = "Waltz")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*line.*|.*Line.*", replacement = "Line Dance")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*foxtrot.*|.*Foxtrot.*", replacement = "Foxtrot")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*Prominent.*", replacement = "Tango")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*BoxStep.*", replacement = "Foxtrot")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*Electric slide.*|.*Electric Slide.*", replacement = "Electric_Slide")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*rumba.*|.*Rumba.*", replacement = "Rumba")) %>%
  mutate(Dance_Type = factor(Dance_Type)) %>%
  # Remove Rumba, Swing, Electric Slide
  filter(Dance_Type != "Rumba") %>%
  filter(Dance_Type != "Swing") %>%
  filter(Dance_Type != "Electric_Slide") %>%
  # Control v PD column
  mutate(Group = Participant) %>%
  mutate(Group = str_replace(Group, pattern = ".*pddancecon.*", replacement = "OA")) %>%
  mutate(Group = str_replace(Group, pattern = ".*pddance.*", replacement = "PD")) %>%
  mutate(Group = factor(Group))

# 3. ANALYSIS SECTION ####

# Set all 0 values to NA
df_clean[df_clean == 0] <- NA

# Analyze entropies by dance type
df_analyze <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  group_by(Group, Dance_Type) %>%
  summarize_if(is.numeric, list(mean = mean, sd = sd)) # but this gets rid of participants column

write_csv(df_clean, path = df_clean_path)
write_csv(df_analyze, path = df_analyze_path)

