# This script cleans and analyzes one dance entropy file (from Matlab)

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(Hmisc)

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
  mutate(Participant = str_extract(file_name, "pddance[0-9][0-9][0-9]|pddancecon[0-9][0-9][0-9]")) %>%
  mutate(Participant = factor(Participant)) %>%
  # Create Dance_Type column
  mutate(file_name = str_remove(file_name,".*pddance[0-9][0-9][0-9]_")) %>%
  mutate(file_name = str_remove(file_name,".*pddancecon[0-9][0-9][0-9]_")) %>%
  mutate(Dance_Type = str_remove(file_name,".xlsx")) %>%  
  # Reorder, remove duplicates
  select(Date, Participant, Dance_Type, ends_with("X"), ends_with("Y")) %>%
  # Clean Dance column
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*Tango.*|.*tango.*", replacement = "Tango")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*Waltz.*|.*waltz.*", replacement = "Waltz")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*Waltz.*|.*waltz.*", replacement = "Waltz")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*line.*|.*Line.*", replacement = "Line_Dance")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*foxtrot.*|.*Foxtrot.*", replacement = "Foxtrot")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*Prominent.*", replacement = "Tango")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*BoxStep.*", replacement = "Foxtrot")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*Electric slide.*|.*Electric Slide.*", replacement = "Electric_Slide")) %>%
  mutate(Dance_Type = str_replace(Dance_Type, pattern = ".*rumba.*|.*Rumba.*", replacement = "Rumba")) %>%
  mutate(Dance_Type = factor(Dance_Type)) %>%
  # Control v PD column
  mutate(Group = Participant) %>%
  mutate(Group = str_replace(Group, pattern = ".*pddancecon.*", replacement = "Control")) %>%
  mutate(Group = str_replace(Group, pattern = ".*pddance.*", replacement = "PD")) %>%
  mutate(Group = factor(Group))

glimpse(df_clean)

# Analyze entropies by dance type
df_analyze <- df_clean %>%
  group_by(Group, Dance_Type) %>%
  summarise(Count = n(),
            mean_SamEn_x = mean(SamEn_Body_Orientation_X), 
            mean_SamEn_y = mean(SamEn_Body_Orientation_Y),
            sd_SamEn_X = sd(SamEn_Body_Orientation_X), 
            sd_SamEn_y = sd(SamEn_Body_Orientation_Y)
            ) 
df_analyze

ggplot(df_analyze,aes(x=Dance_Type,y=mean_SamEn_x, fill = Group)) +
  geom_col(position = "dodge") +
  labs(title = "Mean SamEn on X Coordinate", x = "Dance Types", y = "Mean SamEn") +
  geom_text(aes(label=Count), position = position_dodge(width = 1), vjust = -0.5)

ggplot(df_analyze,aes(x=Dance_Type,y=mean_SamEn_y, fill = Group)) +
  geom_col(position = "dodge") +
  labs(title = "Mean SamEn on Y Coordinate", x = "Dance Types", y = "Mean SamEn") +
  geom_text(aes(label=Count), position = position_dodge(width = 1), vjust = -0.5)
