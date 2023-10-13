library(readr)
library(tidyr)
library(reshape2)
library(tidyverse)
library(dplyr)
library(raincloudplots)

#Define colors - unfamiliar
unfamiliar_upright_col = '#FF3030'
unfamiliar_inverted_col = "#836FFF"

#Mentored Group Colors
familiar_upright_col = '#EE2C2C'
familiar_inverted_col = "#6959CD"

#Control Group Colors
self_upright_col = '#CD2626'
self_inverted_col = "#473C8B"

Mean_Frames <- read_csv("Mean_Frames_Familiar_Faces_Filtered.csv")
Mean_Frames_long <- melt(Mean_Frames, 
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("Participant", "Age"),
                         # The source columns
                         measure.vars=c("Unfamiliar_Upright", "Unfamiliar_Inverted", 
                                        "Familiar_Upright", "Familiar_Inverted", 
                                        "Self_Upright", "Self_Inverted"),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Condition",
                         value.name="Mean_N_Frames"               
                         
                         )

Mean_Frames_long <- Mean_Frames_long %>%
  mutate(Orientation = case_when(Condition == 'Unfamiliar_Upright' ~ 'Upright',
                                    Condition == 'Familiar_Upright' ~ 'Upright',
                                    Condition == 'Self_Upright' ~ 'Upright',
                                    TRUE ~ 'Inverted'))

Mean_Frames_long <- Mean_Frames_long %>%
  mutate(Familiarity = case_when(Condition == 'Unfamiliar_Upright' ~ 'Unfamiliar',
                                 Condition == 'Unfamiliar_Inverted' ~ 'Unfamiliar',
                                 Condition == 'Familiar_Upright' ~ 'Familiar',
                                 Condition == 'Familiar_Inverted' ~ 'Familiar',
                                 TRUE ~ 'Self'))

Mean_Frames_long$Mean_time <- Mean_Frames_long$Mean_N_Frames*(1000/120)

MeanFrames_UF_Upright <- filter(Mean_Frames_long, Condition == 'Unfamiliar_Upright')
MeanFrames_UF_Inverted <- filter(Mean_Frames_long, Condition == 'Unfamiliar_Inverted')
MeanFrames_F_Upright<- filter(Mean_Frames_long, Condition == 'Familiar_Upright')
MeanFrames_F_Inverted <- filter(Mean_Frames_long, Condition == 'Familiar_Inverted')
MeanFrames_S_Upright<- filter(Mean_Frames_long, Condition == 'Self_Upright')
MeanFrames_S_Inverted <- filter(Mean_Frames_long, Condition == 'Self_Inverted')

df_2x3_Frames_All <- data_2x2(
  array_1 = MeanFrames_UF_Upright$Mean_N_Frames,
  array_2 = MeanFrames_UF_Inverted$Mean_N_Frames,
  array_3 = MeanFrames_F_Upright$Mean_N_Frames,
  array_4 = MeanFrames_F_Inverted $Mean_N_Frames,
  array_5 = MeanFrames_S_Upright$Mean_N_Frames,
  array_6 = MeanFrames_S_Inverted$Mean_N_Frames,
  labels = (c('Unfamiliar','Familiar')),
  jit_distance = .09,
  jit_seed = 321,
 # spread_x_ticks = FALSE
 ) 


df_2x3_Time_All <- data_2x2(
  array_1 = MeanFrames_UF_Upright$Mean_time,
  array_2 = MeanFrames_UF_Inverted$Mean_time,
  array_3 = MeanFrames_F_Upright$Mean_time,
  array_4 = MeanFrames_F_Inverted$Mean_time,
  array_5 = MeanFrames_S_Upright$Mean_time,
  array_6 = MeanFrames_S_Inverted$Mean_time,
  labels = (c('Unfamiliar','Familiar')),
  jit_distance = .09,
  jit_seed = 321,
  # spread_x_ticks = FALSE
) 

faces_2x3 <- raincloud_2x3_repmes(
  data = df_2x3_Frames_All,
  colors = c(unfamiliar_upright_col, unfamiliar_inverted_col, familiar_upright_col, familiar_inverted_col, self_upright_col, self_inverted_col),
  fills = c(unfamiliar_upright_col, unfamiliar_inverted_col, familiar_upright_col, familiar_inverted_col, self_upright_col, self_inverted_col),
  alpha = .5) +
  scale_x_continuous(breaks=c(1,2,3), labels=c("Unfamiliar", "Familiar", "Self"), limits=c(0.5, 4)) +
  xlab("Face Familiarity") + 
  ylab("Mean N Frames") +
  labs(title = "Threshold frames for face presentation") +
  theme_classic()
faces_2x3


faces_2x3_time <- raincloud_2x3_repmes(
  data = df_2x3_Time_All,
  colors = c(unfamiliar_upright_col, unfamiliar_inverted_col, familiar_upright_col, familiar_inverted_col, self_upright_col, self_inverted_col),
  fills = c(unfamiliar_upright_col, unfamiliar_inverted_col, familiar_upright_col, familiar_inverted_col, self_upright_col, self_inverted_col),
  alpha = .5) +
  scale_x_continuous(breaks=c(1,2,3), labels=c("Unfamiliar", "Familiar", "Self"), limits=c(0.5, 4)) +
  xlab("Face Familiarity") + 
  ylab("Mean recognition time (milliseconds)") +
  labs(title = "Threshold recognition times for face presentation") +
  theme_classic()
faces_2x3_time
