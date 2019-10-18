library(tidyverse)

df <- read_csv("C:/Users/albei/Nextcloud/Documents/PhD/Ridgel Lab/Dance Poster/Entropy Analysis/noraxon/Playground/pddance001/2018-07-14-12-20_pddance001_Tango - Part 1 (easy).csv", 
               skip = 3)
df <- df %>%
  select(-2:-3) %>%
  select(1:171) 
names(df) <- str_replace_all(names(df2),"Noraxon MyoMotion-","")

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

names.or <- list(names(df.orientation))
names.joint <- list(names(df.joints))
names.traj <- list(names(df.trajectories))
names.pitch <- list(names(df.pitch))
names.roll <- list(names(df.roll))
names.course <- list(names(df.course))
names.pelvis <- list(names(df.pelvis))

names.df <- list(
  names.or,
  names.joint,
  names.traj,
  names.pitch,
  names.roll,
  names.course,
  names.pelvis
)
names(names.df) <- c("Orientation","Joints","Trajectories","Pitch","Roll","Course","Pelvis")

capture.output(print(names.or), file = "orientation.txt")

write.csv(names.or, "orientation.txt", row.names = FALSE)
write.csv(names.joint, "joint.txt", row.names = FALSE)
write.csv(names.traj, "trajectories.txt", row.names = FALSE)
write.csv(names.pitch, "pitch.txt", row.names = FALSE)
write.csv(names.roll, "roll.txt", row.names = FALSE)
write.csv(names.course, "course.txt", row.names = FALSE)
write.csv(names.pelvis, "pelvis.txt", row.names = FALSE)
