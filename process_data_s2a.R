
# Explanation -------------------------------------------------------------

# This script shouldn't be run until process_data_s1a.R, process_data_s1b.R, process_data_s1c.R, and process_data_s1d.R have been completed
# The script is for session 2 data (i.e. the final test 2 days after initial encoding and retrieval practice)


# Intro -------------------------------------------------------------------

# load in required packages, and install if not already present
requiredPackages = c('broom', 'janitor', 'tidyverse')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# Identify Data Files -----------------------------------------------------

# Gorilla outputs a separate csv file for each task node, so specify which csv files correspond to which task.

# Final test
final_test_files <- c("data/session2/data_exp_22514-v6_task-q152.csv", 
                      "data/session2/data_exp_28312-v1_task-q152.csv")

exit_files2 <- c("data/session2/data_exp_22514-v6_questionnaire-k57k.csv",
                 "data/session2/data_exp_28312-v1_questionnaire-k57k.csv")


# excluded participants from session 1
excluded_ppts <- read_csv("output/excluded_ppts_s1b.csv")

# excluded participants who didn't return for s2
excluded_no_return <- read_csv("data/prolific_nonreturners.csv")

# read in ID key
id_key <- read_csv("data/id_key.csv") %>%
  set_names(c("id_1", "participant_private_id"))

# each participant did 324 trials
trials_per_person <- 324

# NOTE for the below sections:
# when reading in data readr will throw up a parsing failure warning
# this is just for rows where Gorilla puts "END OF FILE" for each participant in column 1, and the other columns are left blank
# the warning can be safely ignored


# Process exit questionnaire from session 2 -------------------------------------------------

exit_raw2 <- exit_files2 %>%  # for the list of
  lapply(read_csv) %>%     # read in data
  plyr::rbind.fill() %>%  # bind together
  clean_names()          # put everything in snake_case for consistency

exit_data2 <- exit_raw2 %>%
  merge(id_key) %>%  # need to match participants up with their ID from session 1
  mutate(participant_private_id = id_1) %>%    # replace session 2 ID with their session 1 ID
  select(-c(id_1)) %>%  # drop unnecessary variable
  filter(question_key == "strategy-misc") %>%
  select(participant_private_id,
         response) %>%
  set_names(c("ID", "other_comments_s2"))

# Process final data ------------------------------------------------------

final_raw <- final_test_files %>% # for the list of files
  lapply(read_csv) %>%     # read in data
  plyr::rbind.fill() %>%  # bind together
  clean_names()          # put everything in snake_case for consistency

final_data <- final_raw %>%
  merge(id_key) %>% # need to match participants up with their ID from session 1
  mutate(participant_private_id = id_1) %>%   # replace session 2 ID with their session 1 ID
  select(-c(id_1)) %>%  # drop unnecessary variable
  filter(screen_name == "stimuli",     # only include rows where a response (or timeout) would be recorded
         eventno != "55",            # remove practice trial
         eventno != "56") %>%        # remove practice trial
  mutate(correct = ifelse(is.na(response), "miss", correct)) %>% # replace NAs with misses if they didn't respond
  select(participant_private_id,
         trial_number,
         reaction_time,
         correct,
         category_cue,
         eventno,
         pairtype)




# Check data against exclusion criteria -----------------------------------

accuracy_final_test <- final_data %>%
  group_by(participant_private_id) %>%
  mutate(perc_correct = (sum(correct == "1")/trials_per_person)*100,
         perc_error = (sum(correct == "0")/trials_per_person)*100,
         perc_missed = (sum(correct == "miss")/trials_per_person)*100
         ) %>%
  select(perc_correct,
         perc_error,
         perc_missed,
         ) %>%
  unique()

excluded_final_test_accuracy <- accuracy_final_test %>%
  mutate(included = if_else((perc_correct >= 30 & perc_correct <= 95), 1, 0)
  ) %>%
  filter(included == 0) %>%
  mutate(exc_reason = if_else(perc_correct < 30, "low_accuracy_s2",
                             if_else(perc_correct > 95, "high_accuracy_s2", ""))) %>%
  select(ID = participant_private_id,
         exc_reason)

excluded_ppts <- excluded_ppts %>%
  full_join(excluded_final_test_accuracy) %>%
  full_join(excluded_no_return)

exit_data_s2 <- exit_data2 %>%
  mutate(researcher_rating = "")

exit_data_s2_inc <- exit_data_s2[!exit_data_s2$ID %in% excluded_final_test_accuracy$ID,]

write_csv(exit_data_s2_inc, "output/exit_q_check_s2.csv")

# save files that we'll need for later
write_csv(excluded_ppts, "output/excluded_ppts_s2a.csv")
write_csv(final_data, "output/final_data_s2a.csv")

print("The exit data questionnaire has been saved and requires researcher input. After manually completing this file, please proceed to running the next script process_data_s2b.csv")

# clear the environment before running the next script
rm(list = ls())
gc()

