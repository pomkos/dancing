# This script cleans and analyzes one dance entropy file (from Matlab)

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(readxl)

# LOADING SECTION # 

setwd("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/noraxon")

df <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/noraxon/Separate_dfs/Joint/entropy_joints.csv")

head(df)

# VARIABLE CLEANING SECTION #

df_clean <- df %>%
  # Extract appropriate entropy
  select(-useless_col, -`SamEn_Elbow-LT-Flexion (deg)`, -`SamEn_Elbow-RT-Flexion (deg)`) %>%
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

glimpse(df_clean)

# ANALYSIS SECTION #

# Set all 0 values to NA
df_clean[df_clean == 0] <- NA
write_csv(df_clean, "df_clean.csv")


# Analyze entropies by dance type
df_analyze <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  group_by(Group, Dance_Type) %>%
  summarize_if(is.numeric, list(mean = mean, sd = sd))

summary(df_analyze)

write_csv(df_clean, "joint_entropy.csv",na = "NA")
write_csv(df_analyze, "joint_entropy_grouped.csv")

# GRAPHING SECTION #

lt_knee_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`SamEn_Knee-LT-Flexion (deg)_mean`,
  fill = Group )) + 
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = df_analyze$`SamEn_Knee-LT-Flexion (deg)_mean`,
                ymax = df_analyze$`SamEn_Knee-LT-Flexion (deg)_mean` + df_analyze$`SamEn_Knee-LT-Flexion (deg)_sd`),
                position = "dodge") +
  labs(title = "Mean SamEn of LT Knee Flexion", x = "Dance Types", y = "Mean SamEn") +
  theme_classic() +
  scale_fill_manual(" ", values = c("OA" = "yellow", "PD" = "Blue")) +
  ylim(0,0.2)

rt_knee_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`SamEn_Knee-RT-Flexion (deg)_mean`,
  fill = Group )) + 
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = df_analyze$`SamEn_Knee-RT-Flexion (deg)_mean`,
                    ymax = df_analyze$`SamEn_Knee-RT-Flexion (deg)_mean` + df_analyze$`SamEn_Knee-RT-Flexion (deg)_sd`),
                position = "dodge") +
  labs(title = "Mean SamEn of RT Knee Flexion", x = "Dance Types", y = "Mean SamEn") +
  theme_classic() +
  scale_fill_manual(" ", values = c("OA" = "yellow", "PD" = "Blue")) +
  ylim(0,0.2)

lt_hip_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`SamEn_Hip-LT-Flexion (deg)_mean`,
  fill = Group )) + 
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = df_analyze$`SamEn_Hip-LT-Flexion (deg)_mean`,
                    ymax = df_analyze$`SamEn_Hip-LT-Flexion (deg)_mean` + df_analyze$`SamEn_Hip-LT-Flexion (deg)_sd`),
                position = "dodge") +
  labs(title = "Mean SamEn of LT Hip Flexion", x = "Dance Types", y = "Mean SamEn") +
  theme_classic() +
  scale_fill_manual(" ", values = c("OA" = "yellow", "PD" = "Blue")) +
  ylim(0,0.2)

rt_hip_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`SamEn_Hip-RT-Flexion (deg)_mean`,
  fill = Group )) + 
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = df_analyze$`SamEn_Hip-RT-Flexion (deg)_mean`,
                    ymax = df_analyze$`SamEn_Hip-RT-Flexion (deg)_mean` + df_analyze$`SamEn_Hip-RT-Flexion (deg)_sd`),
                position = "dodge") +
  labs(title = "Mean SamEn of RT Hip Flexion", x = "Dance Types", y = "Mean SamEn") +
  theme_classic() +
  scale_fill_manual(" ", values = c("OA" = "yellow", "PD" = "Blue")) +
  ylim(0,0.2)

ggsave(filename = "rt_hip.png", plot = rt_hip_plt, device = "png")
ggsave(filename = "lt_hip.png", plot = lt_hip_plt, device = "png")
ggsave(filename = "rt_knee.png", plot = rt_knee_plt, device = "png")
ggsave(filename = "lt_knee.png", plot = lt_knee_plt, device = "png")

