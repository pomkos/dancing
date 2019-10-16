# This script cleans and analyzes one dance entropy file (from Matlab)

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(Hmisc)

# 1. LOADING SECTION ####

setwd("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/noraxon")
df <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/noraxon/Separate_dfs/Joint/ApEn_joints.csv")

head(df)

# 2. VARIABLE CLEANING SECTION ####

df_clean <- df %>%
  # Extract appropriate entropy
  select(-useless_col, -`ApEn_Elbow-LT-Flexion (deg)`, -`ApEn_Elbow-RT-Flexion (deg)`) %>%
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

# 3. ANALYSIS SECTION ####

# Set all 0 values to NA
df_clean[df_clean == 0] <- NA


summary(df_clean)

# Analyze entropies by dance type
df_analyze <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  group_by(Group, Dance_Type) %>%
  summarize_if(is.numeric, list(mean = mean, sd = sd))

glimpse(df_analyze)


# 4. GRAPHING SECTION ####

<<<<<<< HEAD

# KSU Theme: unify the design
ksu_theme = list(
  geom_col(position = position_dodge(0.9), color = "black"),
  labs(x = "Dance Types", y = "Mean ApEn"),
  theme_classic(),
  theme(plot.title = element_text(hjust = 0.55)),
  scale_fill_manual(" ", values = c("OA" = "yellow", "PD" = "Blue")),
  ylim(0,0.15)
  )

=======
# GRAPHING SECTION #
# Left Knee Flexion
>>>>>>> 85e306533de6e472714e25b681113db8af4dea6e
lt_knee_plt <- ggplot(df_analyze, aes(
  x = Dance_Type,
  y = df_analyze$`ApEn_Knee-LT-Flexion (deg)_mean`,
  fill = Group )) + 
<<<<<<< HEAD
  ggtitle("Mean ApEn of LT Knee Flexion") +
  geom_errorbar(aes(ymin = df_analyze$`ApEn_Knee-LT-Flexion (deg)_mean`,
                    ymax = df_analyze$`ApEn_Knee-LT-Flexion (deg)_mean` + df_analyze$`ApEn_Knee-LT-Flexion (deg)_sd`),
                width = 0.2,
                position = position_dodge(0.9)) +
  ksu_theme

lt_knee_plt
=======
  geom_col(position = "dodge", color = "black") + # black gives the bars an outline. Dodge separates instead of default stack
  geom_errorbar(aes(ymin = df_analyze$`SamEn_Knee-LT-Flexion (deg)_mean`, # ymin = mean, then there is no downward error bar
                ymax = df_analyze$`SamEn_Knee-LT-Flexion (deg)_mean` + df_analyze$`SamEn_Knee-LT-Flexion (deg)_sd`),
                position = "dodge") +
  labs(title = "Mean SamEn of LT Knee Flexion", x = "Dance Types", y = "Mean SamEn") +
  theme_classic() + # gets rid of background lines
  scale_fill_manual(" ", values = c("OA" = "yellow", "PD" = "Blue")) + # default colors at KSU
  ylim(0,0.2)
>>>>>>> 85e306533de6e472714e25b681113db8af4dea6e

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

ggsave(filename = "sam_rt_hip.png", plot = rt_hip_plt, device = "png")
ggsave(filename = "sam_lt_hip.png", plot = lt_hip_plt, device = "png")
ggsave(filename = "sam_rt_knee.png", plot = rt_knee_plt, device = "png")
ggsave(filename = "sam_lt_knee.png", plot = lt_knee_plt, device = "png")

# 5. STATS SECTION ####

# Overall comparison of PD v OA in the ...
# ... LT Knee Flexion
spread_lt_knee <- spread(df_analyze, key = Group, value = `ApEn_Knee-LT-Flexion (deg)_mean`)
t.test(spread_lt_knee$PD,spread_lt_knee$OA, alternative = "two.sided", var.equal = FALSE) # p = 0.06267

# ... RT Knee Flexion
spread_rt_knee <- spread(df_analyze, key = Group, value = `ApEn_Knee-RT-Flexion (deg)_mean`)
t.test(spread_rt_knee$PD,spread_rt_knee$OA, alternative = "two.sided", var.equal = FALSE)  # p = 0.5

# ... LT Hip Flexion
spread_lt_hip <- spread(df_analyze, key = Group, value = `ApEn_Hip-LT-Flexion (deg)_mean`)
t.test(spread_lt_hip$PD,spread_lt_hip$OA, alternative = "two.sided", var.equal = FALSE) # p = 0.11

# ... RT Hip Flexion
spread_rt_hip <- spread(df_analyze, key = Group, value = `ApEn_Hip-RT-Flexion (deg)_mean`)
t.test(spread_rt_hip$PD,spread_rt_hip$OA, alternative = "two.sided", var.equal = FALSE) # p = 0.09722

# OA v PD in RT Hip Flexion for the ...
df_t <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  unite(temp, Group, Dance_Type, sep = "_") %>%
  spread(temp, `ApEn_Hip-RT-Flexion (deg)`)
# ... Tango
t.test(df_t$OA_Tango, df_t$PD_Tango, alternative = "two.sided", var.equal = FALSE) # p = 0.002406

# ... Foxtrot
t.test(df_t$OA_Foxtrot, df_t$PD_Foxtrot, alternative = "two.sided", var.equal = FALSE) # p = 0.68

# OA v PD in LT Hip Flexion for the ...
df_t <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  unite(temp, Group, Dance_Type, sep = "_") %>%
  spread(temp, `ApEn_Hip-LT-Flexion (deg)`)
# ... Tango
t.test(df_t$OA_Tango, df_t$PD_Tango, alternative = "two.sided", var.equal = FALSE)  # p = 0.44

# ... Foxtrot
t.test(df_t$OA_Foxtrot, df_t$PD_Foxtrot, alternative = "two.sided", var.equal = FALSE) # p = 0.76

# OA v PD in LT Knee Flexion for the ...
df_t <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  unite(temp, Group, Dance_Type, sep = "_") %>%
  spread(temp, `ApEn_Knee-LT-Flexion (deg)`)
# ... Tango
t.test(df_t$OA_Tango, df_t$PD_Tango, alternative = "two.sided", var.equal = FALSE) # p = 0.54

# ... Foxtrot
t.test(df_t$OA_Foxtrot, df_t$PD_Foxtrot, alternative = "two.sided", var.equal = FALSE) # p = 0.26

# OA v PD in RT Knee Flexion for the ...
df_t <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  unite(temp, Group, Dance_Type, sep = "_") %>%
  spread(temp, `ApEn_Knee-RT-Flexion (deg)`)
# ... Tango
t.test(df_t$OA_Tango, df_t$PD_Tango, alternative = "two.sided", var.equal = FALSE) # p = 0.90

# ... Foxtrot
t.test(df_t$OA_Foxtrot, df_t$PD_Foxtrot, alternative = "two.sided", var.equal = FALSE) # p = 0.90

# PD Left v Right Knee Flexion
df_t <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  select(Group, `ApEn_Knee-RT-Flexion (deg)`, `ApEn_Knee-LT-Flexion (deg)`, Dance_Type) %>%
  mutate(Group2 = str_replace(string = Group, pattern = "PD", replacement = "PD2")) %>%
  mutate(Group2 = str_replace(string = Group2, pattern = "OA", replacement = "OA2")) %>%
  mutate(Dance_Type2 = Dance_Type) %>%
  unite(temp1, Group, Dance_Type, sep = "_") %>%
  unite(temp2, Group2, Dance_Type2, sep = "_") %>%
  spread(key = temp1, value = `ApEn_Knee-RT-Flexion (deg)`) %>%
  spread(key = temp2, value = `ApEn_Knee-LT-Flexion (deg)`)

df_pd <- df_clean %>%
  filter(Group == "PD") %>%
  select(1:3, 5, 8, 10:12, -Date, -Participant, -Group) %>%
  drop_na()

df_pd

# PD left v right knee and hip
t.test(df_pd$`ApEn_Hip-LT-Flexion (deg)`, df_pd$`ApEn_Hip-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # p = 0.45
t.test(df_pd$`ApEn_Knee-LT-Flexion (deg)`, df_pd$`ApEn_Knee-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) #0.02

# ... Foxtrot
df_pd_fox <- df_pd %>%
  filter(Dance_Type == "Foxtrot")

t.test(df_pd_fox$`ApEn_Hip-LT-Flexion (deg)`, df_pd_fox$`ApEn_Hip-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) #NS # p = 0.79
t.test(df_pd_fox$`ApEn_Knee-LT-Flexion (deg)`, df_pd_fox$`ApEn_Knee-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # 0.069
# ... Tango
df_pd_tango <- df_pd %>%
  filter(Dance_Type == "Tango")

t.test(df_pd_tango$`ApEn_Hip-LT-Flexion (deg)`, df_pd_tango$`ApEn_Hip-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # p = 0.66
t.test(df_pd_tango$`ApEn_Knee-LT-Flexion (deg)`, df_pd_tango$`ApEn_Knee-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # p = 0.54

pd_knee_plt <- ggplot() + 
  geom_point(color = "black", position = "jitter",
           aes(x = df_pd$Dance_Type,
               y = df_pd$`ApEn_Knee-LT-Flexion (deg)`
               )
           ) +
  geom_point(color = "red", position = "jitter",
           aes(x = df_pd$Dance_Type,
               y = df_pd$`ApEn_Knee-RT-Flexion (deg)`, 
               fill = "RT Knee Flexion", 
               alpha = 0.1
               )
           ) +
  ggtitle("ApEn of Knee Flexion") +
  labs(x = "Dance Types", y = "ApEn") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.55)) +
  ylim(0,0.15)

pd_knee_plt

pd_hip_plt <- ggplot() + 
  geom_point(color = "black", position = "jitter",
             aes(x = df_pd$Dance_Type,
                 y = df_pd$`ApEn_Hip-LT-Flexion (deg)`
             )
  ) +
  geom_point(color = "red", position = "jitter",
             aes(x = df_pd$Dance_Type,
                 y = df_pd$`ApEn_Hip-RT-Flexion (deg)`, 
                 fill = "RT Hip Flexion", 
                 alpha = 0.1
             )
  ) +
  ggtitle("ApEn of Hip Flexion") +
  labs(x = "Dance Types", y = "ApEn") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.55)) +
  ylim(0,0.15)
pd_hip_plt

# T plot
library(mcStats)
showT.Test(df_pd_fox$`ApEn_Knee-LT-Flexion (deg)`, df_pd_fox$`ApEn_Knee-RT-Flexion (deg)`)

# 6. ANOVA ####
library(ggpubr)
# PD
df_pd %>%
  group_by(Dance_Type) %>%
  summarise(
    count = n(),
    mean = mean(`ApEn_Knee-LT-Flexion (deg)`, na.rm = TRUE),
    sd = sd(`ApEn_Knee-LT-Flexion (deg)`, na.rm = TRUE)
  )
ggboxplot(df_pd, x = "Dance_Type", y = "`ApEn_Knee-LT-Flexion (deg)`", 
          color = "Dance_Type",
          order = c("Foxtrot", "Tango", "Line Dance", "Waltz"),
          ylab = "ApEn", xlab = "Dance Type")

df_pd_lt_k.aov <- aov(df_pd$`ApEn_Knee-LT-Flexion (deg)` ~ df_pd$Dance_Type)
summary(df_pd_lt_k.aov)
df_pd_rt_k.aov <- aov(df_pd$`ApEn_Knee-RT-Flexion (deg)` ~ df_pd$Dance_Type)
summary(df_pd_rt_k.aov)
df_pd_lt_h.aov <- aov(df_pd$`ApEn_Hip-LT-Flexion (deg)` ~ df_pd$Dance_Type)
summary(df_pd_lt_h.aov)
df_pd_rt_h.aov <- aov(df_pd$`ApEn_Hip-RT-Flexion (deg)` ~ df_pd$Dance_Type)
summary(df_pd_rt_h.aov)

# OA
df_oa <- df_clean %>%
  filter(Group == "OA") %>%
  select(1:3, 5, 8, 10:12, -Date, -Participant, -Group) %>%
  drop_na()

df_oa %>%
  group_by(Dance_Type) %>%
  summarise(
    count = n(),
    mean = mean(`ApEn_Knee-LT-Flexion (deg)`, na.rm = TRUE),
    sd = sd(`ApEn_Knee-LT-Flexion (deg)`, na.rm = TRUE)
  )
ggboxplot(df_oa, x = "Dance_Type", y = "`ApEn_Knee-LT-Flexion (deg)`", 
          color = "Dance_Type",
          order = c("Foxtrot", "Tango", "Line Dance", "Waltz"),
          ylab = "ApEn", xlab = "Dance Type")

df_oa_Y <- cbind(df_oa$`ApEn_Knee-LT-Flexion (deg)`,
                 df_oa$`ApEn_Knee-RT-Flexion (deg)`,
                 df_oa$`ApEn_Hip-LT-Flexion (deg)`,
                 df_oa$`ApEn_Hip-RT-Flexion (deg)`)
fit_oa <- manova(df_oa_Y ~ df_oa$Dance_Type)
fit_oa_t <- summary(fit_oa) #NS 0.9

df_pd_Y <- cbind(df_pd$`ApEn_Knee-LT-Flexion (deg)`,
                 df_pd$`ApEn_Knee-RT-Flexion (deg)`,
                 df_pd$`ApEn_Hip-LT-Flexion (deg)`,
                 df_pd$`ApEn_Hip-RT-Flexion (deg)`)
fit_pd <- manova(df_pd_Y ~ df_pd$Dance_Type)
fit_pd_t <- summary(fit_pd) #NS 0.1

fit_pd_t$stats[1,6]

# 7. Make T-Test Table ####
t_tests <- list(fit_pd_t, fit_oa_t)
for(i in t_tests){
  print(t_tests)
  print(i$stats[1,6])
}

# 8. ANOVA VALIDITY TESTS####

# Homogeneity of Variance test
## Plot test
plot(df_pd_lt_k.aov, 1) # checks out, but 3 outliers
## Levene's Test
library(car)
leveneTest(df_pd$`ApEn_Knee-LT-Flexion (deg)` ~ df_pd$Dance_Type) # NS, checks out
leveneTest(df_pd$`ApEn_Knee-RT-Flexion (deg)` ~ df_pd$Dance_Type) # NS, checks out
leveneTest(df_pd$`ApEn_Hip-LT-Flexion (deg)` ~ df_pd$Dance_Type) # NS, checks out
leveneTest(df_pd$`ApEn_Hip-RT-Flexion (deg)` ~ df_pd$Dance_Type) # NS, checks out

# Test of Normality
plot(df_pd_lt_k.aov, 2) # Most points fall along straight reference line, checks out
df_pd
library(qwraps2)

df_pd %>%
  summary_table(.)
 