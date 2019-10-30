library(tidyverse)

setwd("C:/Users/albei/Documents/github/dancing")
df_analyze <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/analyze_samen.csv")
df_clean <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/clean_samen.csv")
df_pd <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/pd_samen.csv")
df_oa <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/Data/oa_samen.csv")

# 5. STATS SECTION ####

# Comparison "KNEE" in PD v OA (t test)
# Comparison "KNEE" in PD across dances (ANOVA)

# Overall comparison of PD v OA in the ...
# ... LT Knee Flexion
spread_lt_knee <- spread(df_analyze, key = Group, value = `SamEn_Knee-LT-Flexion (deg)_mean`)
t.test(spread_lt_knee$PD,spread_lt_knee$OA, alternative = "two.sided", var.equal = FALSE) # p = 0.054

# ... RT Knee Flexion
spread_rt_knee <- spread(df_analyze, key = Group, value = `SamEn_Knee-RT-Flexion (deg)_mean`)
t.test(spread_rt_knee$PD,spread_rt_knee$OA, alternative = "two.sided", var.equal = FALSE)  # p = 0.74

# ... LT Hip Flexion
spread_lt_hip <- spread(df_analyze, key = Group, value = `SamEn_Hip-LT-Flexion (deg)_mean`)
t.test(spread_lt_hip$PD,spread_lt_hip$OA, alternative = "two.sided", var.equal = FALSE) # p = 0.08

# ... RT Hip Flexion
spread_rt_hip <- spread(df_analyze, key = Group, value = `SamEn_Hip-RT-Flexion (deg)_mean`)
t.test(spread_rt_hip$PD,spread_rt_hip$OA, alternative = "two.sided", var.equal = FALSE) # p = 0.0583

# OA v PD in RT Hip Flexion for the ...
df_t <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  unite(temp, Group, Dance_Type, sep = "_") %>%
  spread(temp, `SamEn_Hip-RT-Flexion (deg)`)

df_t

# ... Tango
t.test(df_t$OA_Tango, df_t$PD_Tango, alternative = "two.sided", var.equal = FALSE) # p = 0.003227

# ... Foxtrot
t.test(df_t$OA_Foxtrot, df_t$PD_Foxtrot, alternative = "two.sided", var.equal = FALSE) # p = 0.6102

# OA v PD in LT Hip Flexion for the ...
df_t <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  unite(temp, Group, Dance_Type, sep = "_") %>%
  spread(temp, `SamEn_Hip-LT-Flexion (deg)`)
# ... Tango
t.test(df_t$OA_Tango, df_t$PD_Tango, alternative = "two.sided", var.equal = FALSE)  # p = 0.299

# ... Foxtrot
t.test(df_t$OA_Foxtrot, df_t$PD_Foxtrot, alternative = "two.sided", var.equal = FALSE) # p = 0.6315

# OA v PD in LT Knee Flexion for the ...
df_t <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  unite(temp, Group, Dance_Type, sep = "_") %>%
  spread(temp, `SamEn_Knee-LT-Flexion (deg)`)
# ... Tango
t.test(df_t$OA_Tango, df_t$PD_Tango, alternative = "two.sided", var.equal = FALSE) # p = 0.4334

# ... Foxtrot
t.test(df_t$OA_Foxtrot, df_t$PD_Foxtrot, alternative = "two.sided", var.equal = FALSE) # p = 0.179

# OA v PD in RT Knee Flexion for the ...
df_t <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  unite(temp, Group, Dance_Type, sep = "_") %>%
  spread(temp, `SamEn_Knee-RT-Flexion (deg)`)
# ... Tango
t.test(df_t$OA_Tango, df_t$PD_Tango, alternative = "two.sided", var.equal = FALSE) # p = 0.8022

# ... Foxtrot
t.test(df_t$OA_Foxtrot, df_t$PD_Foxtrot, alternative = "two.sided", var.equal = FALSE) # p = 0.9766

# PD Left v Right Knee Flexion
df_t <- df_clean %>%
  select(1:3, 5, 8, 10:12) %>%
  drop_na() %>%
  select(Group, `SamEn_Knee-RT-Flexion (deg)`, `SamEn_Knee-LT-Flexion (deg)`, Dance_Type) %>%
  mutate(Group2 = str_replace(string = Group, pattern = "PD", replacement = "PD2")) %>%
  mutate(Group2 = str_replace(string = Group2, pattern = "OA", replacement = "OA2")) %>%
  mutate(Dance_Type2 = Dance_Type) %>%
  unite(temp1, Group, Dance_Type, sep = "_") %>%
  unite(temp2, Group2, Dance_Type2, sep = "_") %>%
  spread(key = temp1, value = `SamEn_Knee-RT-Flexion (deg)`) %>%
  spread(key = temp2, value = `SamEn_Knee-LT-Flexion (deg)`)

# PD left v right knee and hip
t.test(df_pd$`SamEn_Hip-LT-Flexion (deg)`, df_pd$`SamEn_Hip-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # p = 0.64
t.test(df_pd$`SamEn_Knee-LT-Flexion (deg)`, df_pd$`SamEn_Knee-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) #0.0116

# ... Foxtrot
df_pd_fox <- df_pd %>%
  filter(Dance_Type == "Foxtrot")

t.test(df_pd_fox$`SamEn_Hip-LT-Flexion (deg)`, df_pd_fox$`SamEn_Hip-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) #NS # p = 0.6379
t.test(df_pd_fox$`SamEn_Knee-LT-Flexion (deg)`, df_pd_fox$`SamEn_Knee-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # 0.005227
# ... Tango
df_pd_tango <- df_pd %>%
  filter(Dance_Type == "Tango")

t.test(df_pd_tango$`SamEn_Hip-LT-Flexion (deg)`, df_pd_tango$`SamEn_Hip-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # p = 0.8257
t.test(df_pd_tango$`SamEn_Knee-LT-Flexion (deg)`, df_pd_tango$`SamEn_Knee-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # p = 0.4074

# OA Left v Right Knee Flexion

# OA left v right knee and hip
t.test(df_oa$`SamEn_Hip-LT-Flexion (deg)`, df_oa$`SamEn_Hip-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # p = 0.74
t.test(df_oa$`SamEn_Knee-LT-Flexion (deg)`, df_oa$`SamEn_Knee-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) #0.46

# ... Foxtrot
df_oa_fox <- df_oa %>%
  filter(Dance_Type == "Foxtrot")

t.test(df_oa_fox$`SamEn_Hip-LT-Flexion (deg)`, df_oa_fox$`SamEn_Hip-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) #NS # p = 0.951
t.test(df_oa_fox$`SamEn_Knee-LT-Flexion (deg)`, df_oa_fox$`SamEn_Knee-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # 0.9944
# ... Tango
df_oa_tango <- df_oa %>%
  filter(Dance_Type == "Tango")

t.test(df_oa_tango$`SamEn_Hip-LT-Flexion (deg)`, df_oa_tango$`SamEn_Hip-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # p = 0.1013
t.test(df_oa_tango$`SamEn_Knee-LT-Flexion (deg)`, df_oa_tango$`SamEn_Knee-RT-Flexion (deg)`, alternative = "two.sided", var.equal = FALSE) # p = 0.7799

# T plot
library(mcStats)
showT.Test(df_pd_fox$`SamEn_Knee-LT-Flexion (deg)`, df_pd_fox$`SamEn_Knee-RT-Flexion (deg)`)

# 6. ANOVA ####
library(ggpubr)
# PD
df_pd %>%
  group_by(Dance_Type) %>%
  summarise(
    count = n(),
    mean = mean(`SamEn_Knee-LT-Flexion (deg)`, na.rm = TRUE),
    sd = sd(`SamEn_Knee-LT-Flexion (deg)`, na.rm = TRUE)
  )
ggboxplot(df_pd, x = "Dance_Type", y = "`SamEn_Knee-LT-Flexion (deg)`", 
          color = "Dance_Type",
          order = c("Foxtrot", "Tango", "Line Dance", "Waltz"),
          ylab = "ApEn", xlab = "Dance Type")

df_pd_lt_k.aov <- aov(df_pd$`SamEn_Knee-LT-Flexion (deg)` ~ df_pd$Dance_Type)
summary(df_pd_lt_k.aov) # 0.0984
df_pd_rt_k.aov <- aov(df_pd$`SamEn_Knee-RT-Flexion (deg)` ~ df_pd$Dance_Type)
summary(df_pd_rt_k.aov) #0.142
df_pd_lt_h.aov <- aov(df_pd$`SamEn_Hip-LT-Flexion (deg)` ~ df_pd$Dance_Type)
summary(df_pd_lt_h.aov) #0.456
df_pd_rt_h.aov <- aov(df_pd$`SamEn_Hip-RT-Flexion (deg)` ~ df_pd$Dance_Type)
summary(df_pd_rt_h.aov) #0.100

# OA
df_oa <- df_clean %>%
  filter(Group == "OA") %>%
  select(1:3, 5, 8, 10:12, -Date, -Participant, -Group) %>%
  drop_na()

df_oa %>%
  group_by(Dance_Type) %>%
  summarise(
    count = n(),
    mean = mean(`SamEn_Knee-LT-Flexion (deg)`, na.rm = TRUE),
    sd = sd(`SamEn_Knee-LT-Flexion (deg)`, na.rm = TRUE)
  )
ggboxplot(df_oa, x = "Dance_Type", y = "`SamEn_Knee-LT-Flexion (deg)`", 
          color = "Dance_Type",
          order = c("Foxtrot", "Tango", "Line Dance", "Waltz"),
          ylab = "ApEn", xlab = "Dance Type")

df_oa_Y <- cbind(df_oa$`SamEn_Knee-LT-Flexion (deg)`,
                 df_oa$`SamEn_Knee-RT-Flexion (deg)`,
                 df_oa$`SamEn_Hip-LT-Flexion (deg)`,
                 df_oa$`SamEn_Hip-RT-Flexion (deg)`)
fit_oa <- manova(df_oa_Y ~ df_oa$Dance_Type)
fit_oa_t <- summary(fit_oa) #NS 0.9

df_pd_Y <- cbind(df_pd$`SamEn_Knee-LT-Flexion (deg)`,
                 df_pd$`SamEn_Knee-RT-Flexion (deg)`,
                 df_pd$`SamEn_Hip-LT-Flexion (deg)`,
                 df_pd$`SamEn_Hip-RT-Flexion (deg)`)
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
leveneTest(df_pd$`SamEn_Knee-LT-Flexion (deg)` ~ df_pd$Dance_Type) # NS, checks out
leveneTest(df_pd$`SamEn_Knee-RT-Flexion (deg)` ~ df_pd$Dance_Type) # NS, checks out
leveneTest(df_pd$`SamEn_Hip-LT-Flexion (deg)` ~ df_pd$Dance_Type) # NS, checks out
leveneTest(df_pd$`SamEn_Hip-RT-Flexion (deg)` ~ df_pd$Dance_Type) # NS, checks out

# Test of Normality
plot(df_pd_lt_k.aov, 2) # Most points fall along straight reference line, checks out
df_pd

# 9. Both, long df ####

# Make df long
df_clean_long <- df_clean %>%
  select(c(2,3,5,8,10,11,12,13)) %>%
  pivot_longer(c(`SamEn_Hip-LT-Flexion (deg)`, `SamEn_Hip-RT-Flexion (deg)`), names_to = "Hip_Side", values_to = "SamEn_Hip_Flexion") %>%
  mutate(ID = Participant + Group + Effected_Side)

df_clean_long
