library(tidyverse)
library(ggplot2)
library(ggthemes)

df_analyze <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/analyze_samen.csv")
df_clean <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/clean_samen.csv")

df_clean <- df_clean %>%
  mutate(Effected_Side = factor(Effected_Side))

head(df_clean)

df_pd_path = "C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/pd_samen.csv"
df_oa_path = "C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/oa_samen.csv"
graph_path = "C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Graphs"

# 4. GRAPHING SECTION ####

# KSU Theme: unify the design
ksu_theme = list(
  geom_col(position = position_dodge(0.9), color = "black"), # black gives the bars an outline. Dodge separates instead of default stack
  labs(x = "Dance Types", y = "Mean SamEn"),
  theme_classic(),  # gets rid of background lines
  theme(plot.title = element_text(hjust = 0.55)),
  scale_fill_manual(" ", values = c("OA" = "yellow", "PD" = "Blue")), # default colors at KSU
  ylim(0,0.15)
)

# Left Knee Flexion
lt_knee_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`SamEn_Knee-LT-Flexion (deg)_mean`,
  fill = Group )) + 
  ggtitle("Mean SamEn of LT Knee Flexion") +
  geom_errorbar(aes(ymin = df_analyze$`SamEn_Knee-LT-Flexion (deg)_mean`, # ymin = mean, then there is no downward error bar
                    ymax = df_analyze$`SamEn_Knee-LT-Flexion (deg)_mean` + df_analyze$`SamEn_Knee-LT-Flexion (deg)_sd`),
                width = 0.2,
                position = position_dodge(0.9)) +
  ksu_theme

lt_knee_plt

# Right Knee Flexion
rt_knee_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`SamEn_Knee-RT-Flexion (deg)_mean`,
  fill = Group )) + 
  ggtitle("Mean SamEn of RT Knee Flexion") +
  geom_errorbar(aes(ymin = df_analyze$`SamEn_Knee-RT-Flexion (deg)_mean`,
                    ymax = df_analyze$`SamEn_Knee-RT-Flexion (deg)_mean` + df_analyze$`SamEn_Knee-RT-Flexion (deg)_sd`),
                width = 0.2,
                position = position_dodge(0.9)) +
  ksu_theme

rt_knee_plt

# Left Hip Flexion
lt_hip_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`SamEn_Hip-LT-Flexion (deg)_mean`,
  fill = Group )) + 
  ggtitle("Mean SamEn of LT Hip Flexion") +
  geom_errorbar(aes(ymin = df_analyze$`SamEn_Hip-LT-Flexion (deg)_mean`,
                    ymax = df_analyze$`SamEn_Hip-LT-Flexion (deg)_mean` + df_analyze$`SamEn_Hip-LT-Flexion (deg)_sd`),
                width = 0.2,
                position = position_dodge(0.9)) +
  ksu_theme

lt_hip_plt

# Right Hip Flexion
rt_hip_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`SamEn_Hip-RT-Flexion (deg)_mean`,
  fill = Group )) + 
  ggtitle("Mean SamEn of RT Hip Flexion") +
  geom_errorbar(aes(ymin = df_analyze$`SamEn_Hip-RT-Flexion (deg)_mean`,
                    ymax = df_analyze$`SamEn_Hip-RT-Flexion (deg)_mean` + df_analyze$`SamEn_Hip-RT-Flexion (deg)_sd`),
                width = 0.2,
                position = position_dodge(0.9)) +
  ksu_theme

rt_hip_plt

ggsave(filename = "SamEn_rt_hip.pdf",
       path = graph_path,
       plot = rt_hip_plt, 
       device = "pdf")
ggsave(filename = "SamEn_lt_hip.pdf",
       path = graph_path,
       plot = lt_hip_plt, 
       device = "pdf")
ggsave(filename = "SamEn_rt_knee.pdf",
       path = graph_path,
       plot = rt_knee_plt, 
       device = "pdf")
ggsave(filename = "SamEn_lt_knee.pdf",
       path = graph_path,
       plot = rt_knee_plt, 
       device = "pdf")

# ...... custom legend ####
df_oa <- df_clean %>%
  filter(Group == "OA") %>%
  select(2,3,5,8,10,11,13) %>%
  drop_na()

str(df_oa)
write_csv(df_oa, path = df_oa_path)

# PD hip and knee ####
df_pd <- df_clean %>%
  filter(Group == "PD") %>%
  select(2,3,5,8,10,11,13) %>%
  drop_na()

str(df_pd)
write_csv(df_pd, path = df_pd_path)

library(ggrepel)
pos <- position_jitter(width = 0.5, seed = 2) # standardizes randomness

pd_knee_plt <- ggplot(df_pd, 
                      aes(x = Dance_Type, 
                          label = ifelse(Effected_Side == "U", "U", "") # only label Unknown Side Effected
                          )
                      ) + 
  geom_point(position = pos, # gives randomness in x direction so we can see points
             aes(y = df_pd$`SamEn_Knee-RT-Flexion (deg)`, 
                 color = "2356")) + # labels data (smallest number so first)
  geom_text(hjust = 1.3,
            aes(y = df_pd$`SamEn_Knee-RT-Flexion (deg)`), # labels datapoints
                   position = pos) +
  geom_point(position = pos,
             aes(y = df_pd$`SamEn_Knee-LT-Flexion (deg)`, 
                 color = "5632")) + #labels data (largest number so second)
  geom_text(hjust = 1.3,
            aes(y = df_pd$`SamEn_Knee-LT-Flexion (deg)`), # labels datapoints
            position = pos) +
  labs(title = "SamEn of PD Knee Flexion",
       subtitle = "Left side most effected unless otherwise indicated",
        x = "Dance Types", 
        y = "SamEn") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.55),
        legend.position = "right") +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5) +
  geom_vline(xintercept = 3.5) +
  ylim(0,0.2) +
  scale_color_manual(name = "Location", # name of legend
                     labels = c("Right Knee Flexion", "Left Knee Flexion"), # labels the color = "number", in numerical order
                     values = c("red","black")) # colors the color = "number", in numerical order

pd_knee_plt

ggsave(filename = "SamEn_pd_knee.pdf", 
       plot = pd_knee_plt, 
       path = graph_path,
       device = "pdf")


pd_hip_plt <- ggplot(df_pd, aes(x = Dance_Type,
                                label = ifelse(Effected_Side == "U", "U", "")
                                )) + 
  geom_point(position = pos,
             aes(y = df_pd$`SamEn_Hip-RT-Flexion (deg)`,
                 color = "6532")
  ) +
  geom_text(hjust = 1.3,
            aes(y = df_pd$`SamEn_Hip-RT-Flexion (deg)`), # labels datapoints
            position = pos) +
  geom_point(position = pos,
             aes(y = df_pd$`SamEn_Hip-LT-Flexion (deg)`,
                 color = "8965"),
  ) +
  geom_text(hjust = 1.3,
            aes(y = df_pd$`SamEn_Hip-LT-Flexion (deg)`), # labels datapoints
            position = pos) +
  labs(title = "SamEn of PD Hip Flexion",
       subtitle = "Left side most effected unless otherwise indicated",
       x = "Dance Types", 
       y = "SamEn") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.55),
        legend.position = "right") +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5) +
  geom_vline(xintercept = 3.5) +
  ylim(0,0.2) +
  scale_color_manual(name = "Location",
                     labels = c("Right Hip Flexion", "Left Hip Flexion"),
                     values = c("red","black"))

pd_hip_plt
ggsave(filename = "SamEn_pd_hip.pdf",
       plot = pd_hip_plt, 
       path = graph_path,
       device = "pdf")

# OA hip and knee ####

df_oa <- df_clean %>%
  filter(Group == "OA") %>%
  select(1:3, 5, 8, 10:12, -Date, -Participant, -Group) %>%
  drop_na()

oa_knee_plt <- ggplot(df_oa, aes(x = Dance_Type)) + 
  geom_point(position = "jitter", # gives randomness in x direction so we can see points
             aes(y = df_oa$`SamEn_Knee-RT-Flexion (deg)`, 
                 color = "2356")) + # labels data (smallest number so first)
  geom_point(position = "jitter",
             aes(y = df_oa$`SamEn_Knee-LT-Flexion (deg)`, 
                 color = "5632")) + #labels data (largest number so second)
  ggtitle("SamEn of OA Knee Flexion") +
  labs(x = "Dance Types", y = "SamEn") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.55),
        legend.position = "right") +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5) +
  geom_vline(xintercept = 3.5) +
  ylim(0,0.2) +
  scale_color_manual(name = "Location", # name of legend
                     labels = c("Right Knee Flexion", "Left Knee Flexion"), # labels the color = "number", in numerical order
                     values = c("red","black")) # colors the color = "number", in numerical order

oa_knee_plt

ggsave(filename = "SamEn_oa_knee.pdf", 
       plot = oa_knee_plt, 
       path = graph_path,
       device = "pdf")

oa_hip_plt <- ggplot(df_oa, aes(x = Dance_Type)) + 
  geom_point(position = "jitter", # gives randomness in x direction so we can see points
             aes(y = df_oa$`SamEn_Hip-RT-Flexion (deg)`, 
                 color = "2356")) + # labels data (smallest number so first)
  geom_point(position = "jitter",
             aes(y = df_oa$`SamEn_Hip-LT-Flexion (deg)`, 
                 color = "5632")) + #labels data (largest number so second)
  ggtitle("SamEn of OA Hip Flexion") +
  labs(x = "Dance Types", y = "SamEn") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.55),
        legend.position = "right") +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5) +
  geom_vline(xintercept = 3.5) +
  ylim(0,0.2) +
  scale_color_manual(name = "Location", # name of legend
                     labels = c("Right Hip Flexion", "Left Hip Flexion"), # labels the color = "number", in numerical order
                     values = c("red","black")) # colors the color = "number", in numerical order

oa_hip_plt

ggsave(filename = "SamEn_oa_hip.pdf", 
       plot = oa_hip_plt, 
       path = graph_path,
       device = "pdf")
