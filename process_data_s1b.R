# Explanation -------------------------------------------------------------

# This script shouldn't be run until process_data_s1a.R has been completed
# and the output/exit_q_check_s1.csv file filled in and saved as
# data/exit_q_checked_s1.csv


# Intro -------------------------------------------------------------------

# load in required packages, and install if not already present
requiredPackages = c('broom', 'janitor', 'tidyverse')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


# read in files we need from process_data_s1a.R
excluded_ppts <- read_csv("output/excluded_ppts_s1a.csv")
rr_accuracy <- read_csv("output/rr_accuracy_s1a.csv")
demo <- read_csv("output/demographics_s1a.csv")

# Continue assessing excluded participants --------------------------------

# read in the checked file
exit_data_checked <- read_csv("data/exit_q_checked_s1.csv")

# identify if any extra participants now need to be excluded
excluded_exit <- exit_data_checked %>%
  filter(researcher_rating == 0) %>%
  mutate(exc_reason = "exit_s1") %>%
  select(ID,
         exc_reason)

# add to the list of excluded participants
excluded_ppts <- excluded_ppts %>%
  full_join(excluded_exit)

# exclude these participants from the retrieval data file
rr_accuracy_inc <- rr_accuracy[!rr_accuracy$participant_private_id %in% excluded_ppts$ID,]

# create a rating file for researchers to manually check remaining trials that have not yet been categorised  
rating_file <- rr_accuracy_inc %>%
  filter(accuracy == "CHECK") %>%
  select(participant_private_id,
         trial_number,
         eventno,
         category_cue,
         response,
         target) %>%
  mutate(manual_rating = "") # add this ready for raters

write_csv(rating_file, "output/retrieval_rating_file.csv")

# # create an allowlist of Prolific IDs for session 2
# prolific_allowlist_private <- demo[!demo$ID %in% excluded_ppts$ID,] %>%
#   select(ID)

# save files that we'll need for later
write_csv(rr_accuracy_inc, "output/rr_accuracy_inc_s1b.csv")
write_csv(excluded_ppts, "output/excluded_ppts_s1b.csv")

# create csvs of all the data needed for s2 analysis
#write_csv(prolific_allowlist_private, "output/prolific_allowlist_private.csv")

# print to console for researcher's attention
print("The retrieval data has been saved and requires researcher input. After manually completing this file, please proceed to running the next script process_data_s1c.csv")

# clear the environment before running the next script
rm(list = ls())
gc()


