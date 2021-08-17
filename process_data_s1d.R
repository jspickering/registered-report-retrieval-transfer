

# Explanation -------------------------------------------------------------

# This script shouldn't be run until process_data_s1a.R, process_data_s1b.R, and process_data_s1c.R have been completed


# Intro -------------------------------------------------------------------

# load in required packages, and install if not already present
requiredPackages = c('broom', 'janitor', 'tidyverse')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


# read in files we need from process_data_s1a.R and process_data_s1b.R
rr_accuracy_inc <- read_csv("output/rr_accuracy_inc_s1b.csv")
excluded_ppts <- read_csv("output/excluded_ppts_s1b.csv")
demo <- read_csv("output/demographics_s1a.csv")
rated_file <- read_csv("data/rated_file.csv")


# Researcher input required: retrieval accuracy ---------------------------

retrieval_rated <- rated_file %>%
  mutate(final_rating = ifelse(agreement == "1", rater_1, # if rater 1 and 2 agreed, just use rater 1
                               ifelse(agreement == "0", rater_3, "check")), # if rater 1 and 2 disagreed, rater 3 gets final say
         final_rating = ifelse(final_rating == "correct event, wrong category", "we_cat_error",
                                    ifelse(final_rating == "wrong event, correct category", "wc_event_error",
                                           ifelse(final_rating == "wrong event, wrong category", "be_cat_error", final_rating)))
         ) %>%
  select(participant_private_id,
         trial_number,
         final_rating)

rr_accuracy_final <- rr_accuracy_inc %>%
  full_join(retrieval_rated) %>%
  mutate(accuracy_types = if_else(is.na(final_rating), accuracy_types, final_rating),
         accuracy = ifelse((accuracy_types == "wc_event_error" | accuracy_types == "we_cat_error" | accuracy_types == "be_cat_error" | accuracy_types == "miss" | accuracy_types == "error"), "error", 
                           ifelse(accuracy_types == 1, 1,  accuracy))
  ) %>%
  select(-c(true_typo,
            final_rating))


# create an allowlist of Prolific IDs for session 2
#prolific_allowlist <- demo[!demo$ID %in% excluded_ppts$ID,] %>%
#  select(ID)

# create csvs of all the data needed for s2 analysis
#write_csv(prolific_allowlist, "output/prolific_allowlist.csv")
write_csv(rr_accuracy_final, "data/retrieval_accuracy.csv")

# as a sanity check, we want to make sure there are no more CHECK trials left (if there are, it means we've probably made a mistake when manually rating)
unique(rr_accuracy_final$accuracy)
unique(rr_accuracy_final$accuracy_types)

print("Two unique() function outputs have been printed into the console. Verify that there are none that say CHECK or are BLANK or are NA still. If they do, go back and find the error from the manual_ratings in retrieval_rating_file.csv ")

# clear the environment before running the next script
rm(list = ls())
gc()
