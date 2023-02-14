library(tidyverse)
library(here)
library(janitor)
pilot_1 <- read_csv(here("data/raw_data", "condition_1_pilot_data.csv")) %>%
  clean_names() %>%
  separate(participant, sep = 4,
           into = c("participant", "block")) %>%
  mutate(block = ifelse(block == "", 1, 2))
pilot_1_p <- read_csv(here("data/raw_data", "condition_1_pilot_participant.csv")) %>%
  clean_names()

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
                               y_left > 0 & y_right > 0 ~ "both"))

# Filter out any impossible coordinates (x must be smaller than 1920, y must be smaller than 1080)
# these are the dimensions of the screen. any coordinates larger than this are therefore, errors
d_filter <- d_filter %>% 
  filter(x_left < 1920 &
           y_left < 1080 &
           x_right < 1920 &
           y_right < 1080)

# Both eyes have data -----------------------------------------------------
d_both_eyes <- d_filter %>%
  filter(eye_valid == "both",
         x_left > 0 & # by filtering out gaze points where the x and y coordinates have to be greater than 0
           y_left > 0 & # we remove all points of 0 and negative values.
           x_right > 0 &
           y_right > 0)

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
  filter(delta_x <= eyes_x$x_max, #filter for differences in x coordinates less than or equal to the set maximum difference defined in eyes_x
         delta_x >= eyes_x$x_min, #filter for differences in x coordinates greater than or equal to the set minimum difference defined in eyes_x
         delta_y <= eyes_y$y_max, #filter for differences in y coordinates less than or equal to the set maximum difference defined in eyes_y
         delta_y >= eyes_y$y_min) #filter for differences in x coordinates greater than or equal to the set minimum difference defined in eyes_y

# inspect histograms again
hist(data$delta_x)
hist(data$delta_y)

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



