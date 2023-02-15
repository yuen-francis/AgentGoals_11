library(tidyverse)
library(here)
library(janitor)
library(eyetrackingR)

pilot_1 <- read_csv(here("data/raw_data", "condition_1_pilot_data.csv")) %>%
  clean_names() %>%
  separate(participant, sep = 4,
           into = c("participant", "block")) %>%
  mutate(block = ifelse(block == "", 1, 2))
pilot_1_p <- read_csv(here("data/raw_data", "condition_1_pilot_participant.csv")) %>%
  clean_names()

protagonist_aoi <- read_csv(here("data/raw_data", "protagonist_aoi.csv"))
blue_agent_aoi <- read_csv(here("data/raw_data", "blue_agent_aoi.csv"))
yellow_agent_aoi <- read_csv(here("data/raw_data", "yellow_agent_aoi.csv"))
tree_aoi <- read_csv(here("data/raw_data", "tree_object_aoi.csv"))
candy_aoi <- read_csv(here("data/raw_data", "candy_object_aoi.csv"))


# Resample time -----------------------------------------------------------

#create column (t) where for each participant, start time of first observation is set to 0
# see cumulative times for each participant's observations, 
#indicating the time that each observation began after the first for each participant

# create column (trial_t) where for each participant, start time of each trial is set to 0
# see cumulative times for each observation per trial per participant
# indicates the time that each observation began after the relevant trial started

d <- pilot_1 %>%
  group_by(participant, block) %>% #perform following actions when considering each participant
  mutate(t = round(recording_time_ms - min(recording_time_ms), 0), #min recording time is the time of the first recording for each participant. '0' indicates the number of decimal places to round to.
         .before = recording_time_ms) %>%
  ungroup() %>%
  group_by(participant, block, trial) %>% #do the same thing but for each trial per participant
  mutate(trial_t = t - min(t),
         .after = t) %>%
  ungroup() 

# Relabel trial information -----------------------------------------------
# Due to issues with creating the stimuli in Experiment Center
# The labeling of trials is inconsistent
# Redefining them here so they are consistent across infants

d_filtered <- d %>%
  separate(col = stimulus,
           into = c("type", "filter"),
           sep = " ") %>%
  filter(is.na(filter) == T,
         type != "star_calib.avi",
         type != "blackscreen500ms.png") %>%
  separate(col = trial,
           into = c("ignore", "trial_num"),
           sep = "l") %>%
  mutate(trial_num = as.numeric(trial_num)) 

manual_inspection <- d_filtered %>%
  group_by(participant, block, trial_num, type) %>%
  summarize()

data_trial <- d_filtered %>%
  group_by(participant, block, trial_num) %>%
  summarize() %>%
  ungroup() %>%
  group_by(participant, block) %>%
  mutate(trial_id = 1:(n()))

# merge trial info
d <- d_filtered %>%
  left_join(data_trial)


# Data quality checking ---------------------------------------------------
# Checking proportion of 'bad data'
d_check <- d %>%
  mutate(eyes_match = case_when(`point_of_regard_left_x_px` == 0 & `point_of_regard_right_x_px` != 0 ~ "no",
                                `point_of_regard_left_x_px` != 0 & `point_of_regard_right_x_px` == 0 ~ "no",
                                `point_of_regard_left_x_px` == 0 & `point_of_regard_right_x_px` == 0 ~ "yes",
                                `point_of_regard_left_x_px` != 0 & `point_of_regard_right_x_px` != 0 ~ "yes",
                                `point_of_regard_left_y_px` == 0 & `point_of_regard_right_y_px` != 0 ~ "no",
                                `point_of_regard_left_y_px` != 0 & `point_of_regard_right_y_px` == 0 ~ "no",
                                `point_of_regard_left_y_px` == 0 & `point_of_regard_right_y_px` == 0 ~ "yes",
                                `point_of_regard_left_y_px` != 0 & `point_of_regard_right_y_px` != 0 ~ "yes")) %>%
  group_by(eyes_match) %>%
  summarize(n = n()) 

# sanity check to see if a significant number of observations are potential tracker errors
# plotting eye gaze using coordinates on the screen. graph of x coordinates for eyes and of y coordinates for eyes
# idea is that right and left eye have to be looking at points close together, the eyes cannot look in different directions

d$x_left = as.numeric(d$`point_of_regard_left_x_px`) # creating the x variable of the graph of x coordinates of eye gaze. make the observations numeric so graph works
d$x_right = as.numeric(d$`point_of_regard_right_x_px`) # creating the y variable of the graph. make the observations numeric so graph works
ggplot(sample_n(d, 1000), 
       aes(x = x_left, y = x_right)) +
  geom_point()

d$y_left = as.numeric(d$`point_of_regard_left_y_px`) # same as above but for y coordinates of eye gaze
d$y_right = as.numeric(d$`point_of_regard_right_y_px`)
ggplot(sample_n(d, 1000), 
       aes(x = y_left, y = y_right)) +
  geom_point()

# Filter out any trials that have 0 or negative as the coordinate.
# these are tracker errors (e.g., cannot have a negative eye gaze coordinate)
# Need to treat these cases differently
# Only one eye = just use that coordinate
# None = exclude (no data)
# both eyes = compare them, exclude if needed, then average

d_filter <- d %>% # changed new file to be called d_filter so that we can easily compare as we go along
  mutate(eye_valid = case_when(x_left <= 0 & x_right <= 0 ~ "none",
                               x_left <= 0 & x_right > 0 ~ "right",
                               x_left > 0 & x_right <= 0 ~ "left",
                               x_left > 0 & x_right > 0 ~ "both",
                               y_left <= 0 & y_right <= 0 ~ "none",
                               y_left <= 0 & y_right > 0 ~ "right",
                               y_left > 0 & y_right <= 0 ~ "left",
                               y_left > 0 & y_right > 0 ~ "both"),
         trackloss = ifelse(eye_valid == "none",
                            yes = "yes", no = "no"))

# Catch the impossible coordinates (x must be smaller than 1920, y must be smaller than 1080)
# these are the dimensions of the screen. Any coordinates larger than this are therefore errors
d_filter <- d_filter %>% 
  mutate(trackloss = ifelse(x_left > 1920 | y_left > 1080 | x_right > 1920 | y_right > 1080,
                            yes = "yes", no = "no"))

# Brief look at trackloss so far
trackloss <- d_filter %>%
  group_by(trackloss) %>%
  summarize(n = n())

by_eyes <- d_filter %>%
  group_by(eye_valid) %>%
  summarize(n = n())

# Merging instances where both eyes have data -----------------------------------------------------
d_both_eyes <- d_filter %>%
  filter(eye_valid == "both",
         trackloss == "no")

ggplot(sample_n(d_both_eyes, 1000),  # plot to check if we removed the negative and 0s successfully
       aes(x = x_left, y = x_right)) +
  geom_point()

ggplot(sample_n(d_both_eyes, 1000), 
       aes(x = y_left, y = y_right)) +
  geom_point()

# check for large deviation between eyes
# again, the left and right eye gaze have to be realistically close together (they can't look at different places on the screen of unrealistic distance apart)
#therefore large deviations between eyes are likely due to tracker errors
d_both_eyes$delta_x <- as.numeric(d_both_eyes$`point_of_regard_left_x_px`) - as.numeric(d_both_eyes$`point_of_regard_right_x_px`) #delta_x is the difference between x coordinates of left and right eye
d_both_eyes$delta_y <- as.numeric(d_both_eyes$`point_of_regard_left_y_px`) - as.numeric(d_both_eyes$`point_of_regard_right_y_px`) #delta_y is the difference between y coordinates of left and right eye

# view histogram of differences between the two eyes' coordinates to check for outliers (the likely tracker errors)
hist(d_both_eyes$delta_x)
hist(d_both_eyes$delta_y)

d_both_eyes <- d_both_eyes %>% #filter out the NA (missing) values in delta_x
  filter(is.na(delta_x) == F)

# summarise the data about the x and y coordinates of eye gaze. gives info about mean differences in coordinates, standard deviation, etc.
# also calculate maximum and minimum differences in coordinates for x and y, so that anything outside this range is likely a tracker error (too much or little deviation between the eyes to be realistically possible)
eyes_x <- d_both_eyes %>%
  summarize(mean = mean(delta_x),
            sd = sd(delta_x),
            n_obs = length(delta_x),
            x_max = mean + 2*sd, 
            x_min = mean - 2*sd) 
eyes_y <- d_both_eyes %>%
  summarize(mean = mean(delta_y),
            sd = sd(delta_y),
            n_obs = length(delta_y),
            y_max = mean + 2*sd,
            y_min = mean - 2*sd)

# filter trials where the difference in coordinate between two eyes exceed 2 SD from the mean as this likely indicates a tracker error (as set in previous code)
# again, logic here is that two eyes cannot look at two different spots on the screen
data <- d_both_eyes %>%
  mutate(trackloss = ifelse(delta_x <= eyes_x$x_max & #filter for differences in x coordinates less than or equal to the set maximum difference defined in eyes_x
         delta_x >= eyes_x$x_min & #filter for differences in x coordinates greater than or equal to the set minimum difference defined in eyes_x
         delta_y <= eyes_y$y_max & #filter for differences in y coordinates less than or equal to the set maximum difference defined in eyes_y
         delta_y >= eyes_y$y_min, #filter for differences in x coordinates greater than or equal to the set minimum difference defined in eyes_y
         yes = "no", no = "yes")) 

# inspect histograms again
test <- data %>%
  filter(trackloss == "no")
hist(test$delta_x)
hist(test$delta_y)

# average the two eyes
data <- data %>%
  mutate(gaze_x = (as.numeric(as.character(data$`point_of_regard_left_x_px`)) #turning each observation into a character form then numeric form for code to work
                   + as.numeric(as.character(data$`point_of_regard_right_x_px`)))/2,
         gaze_y = (as.numeric(as.character(data$`point_of_regard_left_y_px`))
                   + as.numeric(as.character(data$`point_of_regard_right_y_px`)))/2) %>%
  select(-c(delta_x, delta_y)) # remove to allow easy binding with one eye data


# Only one eye has data ---------------------------------------------------
d_one_eye <- d_filter %>%
  filter(eye_valid == "left" |
           eye_valid == "right") %>%
  mutate(gaze_x = ifelse(eye_valid == "left", x_left, x_right),
         gaze_y = ifelse(eye_valid == "left", y_left, y_right)) 


# Merge data --------------------------------------------------------------
data <- data %>%
  rbind(d_one_eye)


# Defining trials ---------------------------------------------------------
# Not all trials are test trials; the only way to tell is by trial number
# Adding a column to indicate that to make analysis easier
data_trial_type <- data %>%
  mutate(trial_type = ifelse(trial_id == 4 | trial_id == 9 | trial_id == 14 | trial_id == 19,
                             "test_phase_1", "familiarization"),
         trial_type = ifelse(trial_id == 5 | trial_id == 10 | trial_id == 15 | trial_id == 20,
                             "test_phase_2", trial_type))

# Preparing data for eyetrackingR ------------------------------------------
# adding aois
# Before adding aois, need to clean up trial name
eyetrackingR_data <- data %>%
  separate(col = type,
           into = c("trial", "suffix"),
           sep = "\\.")
eyetrackingR_data$trial <- gsub("[[:digit:]]", "", eyetrackingR_data$trial) # remove numbers

eyetrackingR_data <- add_aoi(data = eyetrackingR_data, aoi_dataframe = protagonist_aoi,
                             x_col = "gaze_x", y_col = "gaze_y",
                             aoi_name = "protagonist",
                             x_min_col = "Left", x_max_col = "Right",
                             y_min_col = "Top", y_max_col = "Bottom")
table(eyetrackingR_data$protagonist)
table(is.na(eyetrackingR_data$protagonist))

eyetrackingR_data <- add_aoi(data = eyetrackingR_data, aoi_dataframe = blue_agent_aoi,
                             x_col = "gaze_x", y_col = "gaze_y",
                             aoi_name = "blue_agent",
                             x_min_col = "Left", x_max_col = "Right",
                             y_min_col = "Top", y_max_col = "Bottom")
table(eyetrackingR_data$blue_agent)
table(is.na(eyetrackingR_data$blue_agent))

eyetrackingR_data <- add_aoi(data = eyetrackingR_data, aoi_dataframe = yellow_agent_aoi,
                             x_col = "gaze_x", y_col = "gaze_y",
                             aoi_name = "yellow_agent",
                             x_min_col = "Left", x_max_col = "Right",
                             y_min_col = "Top", y_max_col = "Bottom")
table(eyetrackingR_data$yellow_agent)
table(is.na(eyetrackingR_data$yellow_agent))

eyetrackingR_data <- add_aoi(data = eyetrackingR_data, aoi_dataframe = tree_aoi,
                             x_col = "gaze_x", y_col = "gaze_y",
                             aoi_name = "tree",
                             x_min_col = "Left", x_max_col = "Right",
                             y_min_col = "Top", y_max_col = "Bottom")
table(eyetrackingR_data$tree)
table(is.na(eyetrackingR_data$tree))

eyetrackingR_data <- add_aoi(data = eyetrackingR_data, aoi_dataframe = candy_aoi,
                             x_col = "gaze_x", y_col = "gaze_y",
                             aoi_name = "candy",
                             x_min_col = "Left", x_max_col = "Right",
                             y_min_col = "Top", y_max_col = "Bottom")
table(eyetrackingR_data$candy)
table(is.na(eyetrackingR_data$candy))

# Adding columns needed for analysis
eyetrackingR_data <- eyetrackingR_data %>%
  mutate(target = trial) %>%
  separate(col = target,
           into = c("target", "target_background", "target_side")) %>%
  mutate(condition = ifelse(target == "yellow" | target == "blue",
                            yes = "social",
                            no = "nonsocial"))
# Trackloss
trackloss <- trackloss_analysis(data = eyetrackingR_data)

# Creating eyetrackingR dataset -------------------------------------------

final_data <- make_eyetrackingr_data(eyetrackingR_data,
                                     participant_column = "participant",
                                     trial_column = "trial_num",
                                     time_column = "trial_t")
