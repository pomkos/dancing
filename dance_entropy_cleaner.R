library(tidyverse)
library(lubridate)

setwd("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/noraxon")

df <- read_csv("Separate_dfs/Orientation/entropy_body_orientation2.csv")

head(df)
df_clean <- df %>%
  # Extract appropriate entropy
  select(file_name,starts_with("SamEn")) %>%
  # Create Date column
  mutate(Date = str_extract(string = file_name, pattern = "^20..............")) %>%
  mutate(Date = ymd_hm(Date)) %>%
  # Create Participant column
  mutate(Participant = str_extract(file_name, "pddance...")) %>%
  # Create Dance_Type column
  mutate(file_name = str_remove(file_name,"....-..-..-..-.._pddance..._")) %>%
  mutate(Dance_Type = str_remove(file_name,".xlsx")) %>%
  # Reorder, remove duplicates
  select(Date, Participant, Dance_Type, ends_with("X"), ends_with("Y"))

head(df_clean,)



