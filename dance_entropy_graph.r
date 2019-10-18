library(tidyverse)
library(ggplot2)
library(ggthemes)

df_analyze <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/analyze_entropy.csv")
df_clean <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/clean_entropy.csv")


df_pd_path = "C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/pd_entropy.csv"
graph_path = "C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Graphs"

# 4. GRAPHING SECTION ####

# KSU Theme: unify the design
ksu_theme = list(
  geom_col(position = position_dodge(0.9), color = "black"), # black gives the bars an outline. Dodge separates instead of default stack
  labs(x = "Dance Types", y = "Mean ApEn"),
  theme_classic(),  # gets rid of background lines
  theme(plot.title = element_text(hjust = 0.55)),
  scale_fill_manual(" ", values = c("OA" = "yellow", "PD" = "Blue")), # default colors at KSU
  ylim(0,0.15)
)

# Left Knee Flexion
lt_knee_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`ApEn_Knee-LT-Flexion (deg)_mean`,
  fill = Group )) + 
  ggtitle("Mean ApEn of LT Knee Flexion") +
  geom_errorbar(aes(ymin = df_analyze$`ApEn_Knee-LT-Flexion (deg)_mean`, # ymin = mean, then there is no downward error bar
                    ymax = df_analyze$`ApEn_Knee-LT-Flexion (deg)_mean` + df_analyze$`ApEn_Knee-LT-Flexion (deg)_sd`),
                width = 0.2,
                position = position_dodge(0.9)) +
  ksu_theme

lt_knee_plt

# Right Knee Flexion
rt_knee_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`ApEn_Knee-RT-Flexion (deg)_mean`,
  fill = Group )) + 
  ggtitle("Mean ApEn of RT Knee Flexion") +
  geom_errorbar(aes(ymin = df_analyze$`ApEn_Knee-RT-Flexion (deg)_mean`,
                    ymax = df_analyze$`ApEn_Knee-RT-Flexion (deg)_mean` + df_analyze$`ApEn_Knee-RT-Flexion (deg)_sd`),
                width = 0.2,
                position = position_dodge(0.9)) +
  ksu_theme

rt_knee_plt

# Left Hip Flexion
lt_hip_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`ApEn_Hip-LT-Flexion (deg)_mean`,
  fill = Group )) + 
  ggtitle("Mean ApEn of LT Hip Flexion") +
  geom_errorbar(aes(ymin = df_analyze$`ApEn_Hip-LT-Flexion (deg)_mean`,
                    ymax = df_analyze$`ApEn_Hip-LT-Flexion (deg)_mean` + df_analyze$`ApEn_Hip-LT-Flexion (deg)_sd`),
                width = 0.2,
                position = position_dodge(0.9)) +
  ksu_theme

lt_hip_plt

# Right Hip Flexion
rt_hip_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`ApEn_Hip-RT-Flexion (deg)_mean`,
  fill = Group )) + 
  ggtitle("Mean ApEn of RT Hip Flexion") +
  geom_errorbar(aes(ymin = df_analyze$`ApEn_Hip-RT-Flexion (deg)_mean`,
                    ymax = df_analyze$`ApEn_Hip-RT-Flexion (deg)_mean` + df_analyze$`ApEn_Hip-RT-Flexion (deg)_sd`),
                width = 0.2,
                position = position_dodge(0.9)) +
  ksu_theme

rt_hip_plt

ggsave(filename = "apen_rt_hip.pdf",
       path = graph_path,
       plot = rt_hip_plt, 
       device = "pdf")
ggsave(filename = "apen_lt_hip.pdf",
       path = graph_path,
       plot = lt_hip_plt, 
       device = "pdf")
ggsave(filename = "apen_rt_knee.pdf",
       path = graph_path,
       plot = rt_knee_plt, 
       device = "pdf")
ggsave(filename = "apen_lt_knee.pdf",
       path = graph_path,
       plot = rt_knee_plt, 
       device = "pdf")

# ...... custom legend ####
df_pd <- df_clean %>%
  filter(Group == "PD") %>%
  select(1:3, 5, 8, 10:12, -Date, -Participant, -Group) %>%
  drop_na()

write_csv(df_pd, path = df_pd_path)

df_pd

pd_knee_plt <- ggplot(df_pd, aes(x = Dance_Type)) + 
  geom_point(position = "jitter", # gives randomness in x direction so we can see points
             aes(y = df_pd$`ApEn_Knee-RT-Flexion (deg)`, 
                 color = "2356")) + # labels data (smallest number so first)
  geom_point(position = "jitter",
             aes(y = df_pd$`ApEn_Knee-LT-Flexion (deg)`, 
                 color = "5632")) + #labels data (largest number so second)
  ggtitle("ApEn of Knee Flexion") +
  labs(x = "Dance Types", y = "ApEn") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.55),
        legend.position = "right") +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5) +
  geom_vline(xintercept = 3.5) +
  scale_color_manual(name = "Location", # name of legend
                     labels = c("Right Knee Flexion", "Left Knee Flexion"), # labels the color = "number", in numerical order
                     values = c("red","black")) # colors the color = "number", in numerical order

pd_knee_plt

ggsave(filename = "apen_pd_knee.pdf", 
       plot = pd_knee_plt, 
       path = graph_path,
       device = "pdf")


pd_hip_plt <- ggplot(df_pd, aes(x = Dance_Type)) + 
  geom_point(position = "jitter",
             aes(y = df_pd$`ApEn_Hip-RT-Flexion (deg)`,
                 color = "6532")
  ) +
  geom_point(position = "jitter",
             aes(y = df_pd$`ApEn_Hip-LT-Flexion (deg)`,
                 color = "8965"),
  ) +
  ggtitle("ApEn of Hip Flexion") +
  labs(x = "Dance Types", y = "ApEn") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.55),
        legend.position = "right") +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5) +
  geom_vline(xintercept = 3.5) +
  scale_color_manual(name = "Location",
                     labels = c("Right Hip Flexion", "Left Hip Flexion"),
                     values = c("red","black"))

pd_hip_plt
ggsave(filename = "apen_pd_hip.pdf",
       plot = pd_hip_plt, 
       path = graph_path,
       device = "pdf")
