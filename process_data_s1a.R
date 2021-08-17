####
# Processing data for retrieval practice/transfer study
# Hosted on Gorilla, recruited via Prolific
# Code by Jade Pickering (2020)
# Based on some original code by Emma James (2019) https://osf.io/cqm7v/
#
# This script extracts the relevant data from the csv output files of the experiment on Gorilla.
####

# Setup -------------------------------------------------------------------

# load in required packages, and install if not already present
requiredPackages = c('janitor', 'lubridate', 'tidyverse')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# Identify Data Files -----------------------------------------------------

# Gorilla outputs a separate csv file for each task node, so specify which csv files correspond to which task.

# Demographic information
demo_files_dob <- c("data/session1/data_exp_20655-v18_questionnaire-clby.csv",
                    "data/session1/data_exp_20655-v19_questionnaire-clby.csv")
demo_files_my <- c("data/session1/data_exp_20655-v20_questionnaire-clby.csv")

# Exit questionnaire
exit_files1 <- c("data/session1/data_exp_20655-v18_questionnaire-kzn1.csv",
                 "data/session1/data_exp_20655-v19_questionnaire-kzn1.csv",
                 "data/session1/data_exp_20655-v20_questionnaire-kzn1.csv")

# Encoding data
enc_files <- c("data/session1/data_exp_20655-v18_task-mr21.csv",
               "data/session1/data_exp_20655-v19_task-mr21.csv",
               "data/session1/data_exp_20655-v20_task-mr21.csv")

# Retrieval and re-exposure data where each .csv file is one of the counterbalanced conditions (sometimes from different experiment versions due to bug fixes)
ret_reex_files <- c("data/session1/data_exp_20655-v18_task-4r7u.csv", # 1
                    "data/session1/data_exp_20655-v18_task-k56j.csv", # 2
                    "data/session1/data_exp_20655-v18_task-2ieh.csv", # 5
                    "data/session1/data_exp_20655-v18_task-8pn4.csv", # 7
                    "data/session1/data_exp_20655-v18_task-baa9.csv", # 8
                    "data/session1/data_exp_20655-v18_task-wg9l.csv", # 9
                    "data/session1/data_exp_20655-v18_task-eo88.csv", # 10
                    "data/session1/data_exp_20655-v18_task-ll7u.csv", # 11
                    "data/session1/data_exp_20655-v18_task-4tl9.csv", # 14
                    "data/session1/data_exp_20655-v18_task-fnyr.csv", # 16
                    "data/session1/data_exp_20655-v18_task-qed5.csv", # 17
                    "data/session1/data_exp_20655-v18_task-fwmg.csv", # 18
                    "data/session1/data_exp_20655-v19_task-4r7u.csv", # 1
                    "data/session1/data_exp_20655-v19_task-k56j.csv", # 2
                    "data/session1/data_exp_20655-v19_task-kvyc.csv", # 3
                    "data/session1/data_exp_20655-v19_task-grd5.csv", # 4
                    "data/session1/data_exp_20655-v19_task-2ieh.csv", # 5
                    "data/session1/data_exp_20655-v19_task-i679.csv", # 6
                    "data/session1/data_exp_20655-v19_task-8pn4.csv", # 7
                    "data/session1/data_exp_20655-v19_task-baa9.csv", # 8
                    "data/session1/data_exp_20655-v19_task-wg9l.csv", # 9
                    "data/session1/data_exp_20655-v19_task-eo88.csv", # 10
                    "data/session1/data_exp_20655-v19_task-ll7u.csv", # 11
                    "data/session1/data_exp_20655-v19_task-js7a.csv", # 12
                    "data/session1/data_exp_20655-v19_task-kwgy.csv", # 13
                    "data/session1/data_exp_20655-v19_task-4tl9.csv", # 14
                    "data/session1/data_exp_20655-v19_task-cj8m.csv", # 15
                    "data/session1/data_exp_20655-v19_task-fnyr.csv", # 16
                    "data/session1/data_exp_20655-v19_task-qed5.csv", # 17
                    "data/session1/data_exp_20655-v19_task-fwmg.csv", # 18
                    "data/session1/data_exp_20655-v20_task-4r7u.csv", # 1
                    "data/session1/data_exp_20655-v20_task-k56j.csv", # 2
                    "data/session1/data_exp_20655-v20_task-kvyc.csv", # 3
                    "data/session1/data_exp_20655-v20_task-grd5.csv", # 4
                    "data/session1/data_exp_20655-v20_task-2ieh.csv", # 5
                    "data/session1/data_exp_20655-v20_task-i679.csv", # 6
                    "data/session1/data_exp_20655-v20_task-8pn4.csv", # 7
                    "data/session1/data_exp_20655-v20_task-baa9.csv", # 8
                    "data/session1/data_exp_20655-v20_task-wg9l.csv", # 9
                    "data/session1/data_exp_20655-v20_task-eo88.csv", # 10
                    "data/session1/data_exp_20655-v20_task-ll7u.csv", # 11
                    "data/session1/data_exp_20655-v20_task-js7a.csv", # 12
                    "data/session1/data_exp_20655-v20_task-kwgy.csv", # 13
                    "data/session1/data_exp_20655-v20_task-4tl9.csv", # 14
                    "data/session1/data_exp_20655-v20_task-cj8m.csv", # 15
                    "data/session1/data_exp_20655-v20_task-fnyr.csv", # 16
                    "data/session1/data_exp_20655-v20_task-qed5.csv", # 17
                    "data/session1/data_exp_20655-v20_task-fwmg.csv") # 18

remove_files <- c("data/session1/data_exp_20655-v18_task-kvyc.csv", # 3
                  "data/session1/data_exp_20655-v18_task-grd5.csv", # 4
                  "data/session1/data_exp_20655-v18_task-i679.csv", # 6
                  "data/session1/data_exp_20655-v18_task-js7a.csv", # 12
                  "data/session1/data_exp_20655-v18_task-kwgy.csv", # 13
                  "data/session1/data_exp_20655-v18_task-cj8m.csv") # 15
  


# info for each event in the stimulus set
event_key <- read_csv("data/event_key.csv") %>%
  set_names(c("event_no", "ANIMAL", "OBJECT", "LOCATION")) # removes error with first column header when reading csv made in excel

# custom dictionary created with https://adresults.com/tools/keyword-typo-generator/ and all options set to TRUE
# used for checking typos later on
dictionary <- read_csv("data/dictionary.csv")


# NOTE for the below sections:
# when reading in data readr will throw up a parsing failure warning
# this is just for rows where Gorilla puts "END OF FILE" for each participant in column 1, and the other columns are left blank
# the warning can be safely ignored


# Remove participants due to researcher error -----------------------------

# some participants had to be removed as there was an error with some of the stimuli in Gorilla on the first two days of testing
# therefore we don't need the data files for analysis and can generate the list of excluded participants here instead
excluded_incorrect_spreadsheet <- remove_files %>%  # for the list of demographic files...
  lapply(read_csv) %>% # read in each
  plyr::rbind.fill() %>%  # bind together
  clean_names() %>% # put everything in snake_case for consistency
  select(participant_private_id) %>%
  unique() %>%
  drop_na() %>%
  mutate(ID = as.character(participant_private_id),
         exc_reason = "spreadsheet_error") %>%
  select(-c(participant_private_id))


# Process demographic data ------------------------------------------------

# read in data
demo_raw_dob <- demo_files_dob %>%  # for the list of demographic files...
  lapply(read_csv) %>% # read in each
  plyr::rbind.fill() %>%  # bind together
  clean_names() # put everything in snake_case for consistency

# read in data
demo_raw_my <- demo_files_my %>%  # for the list of demographic files...
  lapply(read_csv) %>% # read in each
  plyr::rbind.fill() %>%  # bind together
  clean_names() # put everything in snake_case for consistency

demo_dob <- demo_raw_dob %>%
  mutate(screening_eligibility = ifelse((branch_1wjh == "screenout" |
                                           branch_sxfc == "screenout"), 0, 1)) %>%    # only include ppts who passed attention checks
  filter(question_key == "gender" |                       # drop down gender option
           question_key == "gender-text" |                  # if participants wanted to self-describe gender it's stored here
           question_key == "response-2-inmonths") %>%          # select rows with gender/age info
  droplevels() %>% # remove everything else
  select(participant_private_id,
         question_key,
         response,
         screening_eligibility) %>% # select IDs, questions, responses, and eligibility based on attention checks
  pivot_wider(names_from = question_key,
              values_from = response) %>% # change from long to wide format (one row per ppt)
  set_names(c("ID",
              "screening_eligibility",
              "gender",
              "gender_described",
              "age_months")) %>% # rename columns
  mutate(ID = as.character(ID),      # makes it easier to join with some other dfs later
         age_months = as.numeric(as.character(age_months)), # convert age to numeric
         age = (age_months / 12), # change age from months to years
         age_eligibility = ifelse(age >= 18 & age < 36, 1, 0), # check they meet age criteria
         included = if_else((screening_eligibility == 1 & age_eligibility == 1), 1, 0)) %>%  # mark for inclusion/exclusion based on age and attention check criteria
  select(-c(age_months)) # drop irrelevant column

# FOR DATA COLLECTED AFTER CHANGING HOW WE RECORD DATE OF BIRTH

demo_my <- demo_raw_my %>%
  mutate(screening_eligibility = ifelse((branch_1wjh == "screenout" |
                                           branch_sxfc == "screenout"), 0, 1)) %>%    # only include ppts who passed attention checks
  filter(question_key == "gender" |                       # drop down gender option
           question_key == "gender-text" |                  # if participants wanted to self-describe gender it's stored here
           question_key == "response-2-month" |
           question_key == "response-2-year") %>%          # select rows with gender/age info
  droplevels() %>% # remove everything else
  select(participant_private_id,
         local_date,
         question_key,
         response,
         screening_eligibility) %>% # select IDs, questions, responses, and eligibility based on attention checks
  pivot_wider(names_from = question_key,
              values_from = response) %>% # change from long to wide format (one row per ppt)
  clean_names() %>%
  mutate(month = month(dmy_hms(local_date)),
         year = year(dmy_hms(local_date)),
         day = "01",   # as we don't have their actual birthdate, fill in all dobs and participation dates as 1st of the month
         dob = make_date(response_2_year, response_2_month, day),
         participation_date = make_date(year, month, day),
         age = as.period(interval(start = dob, end = participation_date))$year) %>%
  select(ID = participant_private_id,
         screening_eligibility,
         gender,
         gender_described = gender_text,
         age) %>%
  mutate(ID = as.character(ID),      # makes it easier to join with some other dfs later
         age_eligibility = ifelse(age >= 18 & age < 36, 1, 0), # check they meet age criteria
         included = if_else((screening_eligibility == 1 & age_eligibility == 1), 1, 0)) # mark for inclusion/exclusion based on age and attention check criteria

demo <- demo_dob %>%
  rbind(demo_my)


# Process exit questionnaire from session 1 ------------------------------------------------

# read in data
exit_raw1 <- exit_files1 %>%  # for the list of files
  lapply(read_csv) %>%     # read in data
  plyr::rbind.fill() %>%  # bind together
  clean_names()          # put everything in snake_case for consistency

exit_data1 <- exit_raw1 %>%
  filter(question_key == "strategy-describe" |                      
         question_key == "strategy-people" |
         question_key == "strategy-aid" |
         question_key == "strategy-misc"  ) %>%   # select rows with relevant info
  select(participant_private_id,
         question_key,
         response) %>%
  unique() %>% # there is a participant who gets their info duplicated in the exit strategy (maybe they clicked submit twice quickly?). this gets rid of the extra set.
  pivot_wider(names_from = question_key,
              values_from = response) %>%
  set_names(c("ID", "describe_strategy", "people_present", "strategy_aids", "other_comments_s1")) # make it more readable

# create file to be checked by researcher later
exit_data_s1 <- exit_data1 %>%
  mutate(researcher_rating = "")


# Process encoding data ---------------------------------------------------

# read in data
enc_raw <- enc_files %>%
  lapply(read_csv) %>%  # read in each
  plyr::rbind.fill() %>%  # bind together
  clean_names() # put in snake_case for consistency

# double check that everyone that should've been screened out for failing attention checks has been screened out by Gorilla
# if not we can add to exclusion list later
excluded_enc_attention <- enc_raw %>%
  filter(screen_name == "attention") %>%      # look at the attention trials only
  mutate(ID = as.character(participant_private_id)) %>%
  select(ID,
         correct) %>%
  group_by(ID) %>%
  mutate(accuracy = mean(correct)) %>%   # calculate mean accuracy on attention trials
  select(-c(correct)) %>%
  unique() %>%
  mutate(exc_reason = if_else(accuracy < 1, "attention_encoding", "NA")) %>%  # if's not 100% accuracy, exclude them
  filter(exc_reason == "attention_encoding") %>%
  select(-c(accuracy))


# Process retrieval data --------------------------------------------------

# read in data
rr_raw <- ret_reex_files %>% # for the list of retrieval and re-exposure files
  lapply(read_csv) %>%  # read in each
  plyr::rbind.fill() %>%  # bind together
  clean_names() # put in snake_case for consistency

# double check there aren't any that should've been exclude for failing attention checks
excluded_rr_attention <- rr_raw %>%
  filter(screen_name == "attention") %>%
  mutate(ID = as.character(participant_private_id)) %>%
  select(ID,
         correct) %>%
  group_by(ID) %>%
  mutate(accuracy = mean(correct)) %>%
  select(-c(correct)) %>%
  unique() %>%
  mutate(exc_reason = if_else(accuracy < 1, "attention_rr", "NA")) %>%
  filter(exc_reason == "attention_rr") %>%
  select(-c(accuracy))

  
# get the file ready to be scored automatically by R and then manually by two researchers
rr_data <- rr_raw %>%
  filter(zone_type == "response_text_entry",   # select rows where participants should have typed a response
         eventno != "55",
         eventno != "56") %>% # remove practice trials
  select(participant_private_id,
         trial_number,
         display,
         reaction_time,
         response,
         cue,
         category_cue,
         target,
         eventno,
         pairtype,
         untested_element)

# convert all responses to lowercase so that it's easier to check against dictionary
rr_data$response = tolower(rr_data$response)


# identify correct and missed responses
rr_accuracy <- rr_data %>%
  mutate(response = as.character(response),
         accuracy = ifelse(response == target, 1,       # check whether the typed response matches the cue OR it was timed out (and is therefore incorrect)
                           ifelse(is.na(response), "miss", "CHECK") # haven't worked out why it does "NA" instead of "miss" here so corrected on next line
                           ),
         accuracy = ifelse(is.na(accuracy), "miss", accuracy), 
         )

# find out how many were correct/missed trials, and how many still need checking
accuracy_sum_1 <- rr_accuracy %>%
  group_by(participant_private_id) %>%
  mutate(perc_correct = (sum(accuracy == "1")/36)*100,
         perc_missed = (sum(accuracy == "miss")/36)*100,
         perc_check = (sum(accuracy == "CHECK")/36)*100) %>%
  select(participant_private_id,
         perc_correct,
         perc_missed,
         perc_check) %>%
  unique()

write_csv(accuracy_sum_1, "output/accuracy_summary_unprocessed_1.csv")


# Implement spell check ---------------------------------------------------

spell_test <- rr_accuracy %>%   
  mutate(correct_suggest = "",                 # add empty columns to fill in during spell check
         true_typo = "",
         typed_response = response)            # make a copy of original responses so that we don't lose the info


for (row in 1:nrow(spell_test)) {
  if (spell_test$accuracy[row] == "CHECK") {        # if the response needs checking
    typed_response <- spell_test$response[row]      # find out what the participant typed
    typo_loc <- which(dictionary == typed_response, arr.ind = TRUE)    # check if that response exists as a typo in our "dictionary of typos"
    if ((dim(typo_loc)[1] != 0)) {                     # if the response does exist (i.e. the typo_loc df is NOT empty)
      typo_row <- as.numeric(typo_loc[1,1])            # find out which row of the dictionary it occurred on
      spell_test$correct_suggest[row] <- dictionary$element[typo_row]   # then take the correctly spelled element from that row, and put it in the original df
    } else {                                         # if the typed response wasn't found
      spell_test$correct_suggest[row] <- "CHECK"     # mark it for checking again later   
    }  
  } else {
    # do nothing and move to the next row           # if it didn't need checking at all we can just move on to the next row
  }
}

# because it picks up both typos AND  correct elements,
# we need to check if the "typo" is already in our event key
# (in which case it's just a word from our stimulus set spelled correctly and we'll pick this up later!)
for (row in 1:nrow(spell_test)) {
  if (spell_test$correct_suggest[row] == "CHECK") {     # if it couldn't be identified as a typo or stimulus word
    spell_test$true_typo[row] <- "CHECK"   # note that it still needs checking later
  } else if (spell_test$accuracy[row] == "CHECK") {        # if we'd marked it to  be checked
    if (spell_test$response[row] == spell_test$correct_suggest[row]) {    # if the suggested response matches the typed response (i.e. it was a correctly typed error)
      spell_test$true_typo[row] <- "FALSE"                    # then we don't want to call it a typo
    } else if (spell_test$response[row] != spell_test$correct_suggest[row]) {   # if the typed response doesn't match the suggested correction
      spell_test$true_typo[row] <- "TRUE"             # we want to flag this as a typo
      spell_test$accuracy[row] <- "CHECK"              # and categorise the error type as a typo
      spell_test$response[row] <- spell_test$correct_suggest[row]   # and update the response to the correctly typed word
    }
  } else {
    spell_test$true_typo[row] <- "FALSE"         # mark if it didn't need checking for now
  }
}

# now that we know it all works, can put back into the rr_accuracy df
rr_accuracy <- spell_test %>%
  select(-c(correct_suggest)) # drop the columns we no longer need


# check again after finding typos
accuracy_sum_2 <- rr_accuracy %>%
  group_by(participant_private_id) %>%
  mutate(perc_correct = (sum(accuracy == "1")/36)*100,
         perc_missed = (sum(accuracy == "miss")/36)*100,
         perc_check = (sum(accuracy == "CHECK")/36)*100,
         perc_typo = (sum(true_typo == TRUE)/36)*100) %>%
  select(participant_private_id,
         perc_correct,
         perc_missed,
         perc_check,
         perc_typo) %>%
  unique()

write_csv(accuracy_sum_2, "output/accuracy_summary_processed_2.csv")


# Categorise errors -------------------------------------------------------

# identify whether any corrected typos were actually correct responses after all
rr_accuracy <- rr_accuracy %>%
  mutate(accuracy = ifelse((true_typo == TRUE & response == target), 1, accuracy)  # check whether the corrected responses match the cue
         )

# identify whether the response is from the correct category but wrong event
rr_accuracy <- rr_accuracy %>%
  mutate(be_obj_err = "",
         be_loc_err = "",
         be_ani_err = "")

for (row in 1:nrow(rr_accuracy)) {
  if (rr_accuracy$category_cue[row] == "OBJECT") {                                   # where the category cue was OBJECT
    rr_accuracy$be_obj_err[row] <- rr_accuracy$response[row] %in% event_key$OBJECT   # find if their incorrect answer was another OBJECT in our list of stimuli
    rr_accuracy$be_loc_err[row] <- "FALSE"                                           # fill FALSE for the two other categories
    rr_accuracy$be_ani_err[row] <- "FALSE"                                           # fill FALSE for the two other categories
  } else if (rr_accuracy$category_cue[row] == "LOCATION") {
    rr_accuracy$be_loc_err[row] <- rr_accuracy$response[row] %in% event_key$LOCATION
    rr_accuracy$be_obj_err[row] <- "FALSE"
    rr_accuracy$be_ani_err[row] <- "FALSE"
  } else if (rr_accuracy$category_cue[row] == "ANIMAL") {
    rr_accuracy$be_ani_err[row] <- rr_accuracy$response[row] %in% event_key$ANIMAL
    rr_accuracy$be_obj_err[row] <- "FALSE"
    rr_accuracy$be_loc_err[row] <- "FALSE"
  }
}

# replace "CHECK" with "wc_event_error" (within category event error)
rr_accuracy <- rr_accuracy %>%
  mutate(accuracy = ifelse(accuracy == "CHECK" &
                             (be_obj_err == TRUE |
                                be_loc_err == TRUE |
                                be_ani_err == TRUE), "wc_event_error", accuracy)) %>%  # replace accuracy with error type if one was detected
  select(-c(be_obj_err,
            be_loc_err,
            be_ani_err)) # remove the columns we don't need anymore


# categorise remaining errors

# set up empty columns
rr_accuracy <- rr_accuracy %>%
  mutate(lookup_event_no = "",
         lookup_cat = "",
         lookup_word = "")

for (row in 1:nrow(rr_accuracy)) {
  if (rr_accuracy$accuracy[row] == "CHECK" ) {
  event_number <- rr_accuracy$eventno[row]   # find the event number for this trial
  resp <- rr_accuracy$response[row]    # find out the response for this trial
  resp_loc <- which(event_key == resp, arr.ind = TRUE)  # find out if that response exists in the event_key
    if ((dim(resp_loc)[1] != 0)) {   # if the response does exist (i.e. the resp_loc df is NOT empty)
      lookup_row <- as.numeric(resp_loc[1,1])   # find out which row of the dictionary it occurred in
      lookup_col <- as.numeric(resp_loc[1,2])   # find out the column it occurred in
      rr_accuracy$lookup_event_no[row] <- event_key$event_no[lookup_row] # take the eventno and put in the original df
      rr_accuracy$lookup_cat[row] <- names(event_key)[lookup_col] # take the column name (i.e the category) and put in the original df
      rr_accuracy$lookup_word[row] <- event_key[lookup_row,lookup_col] # take the word and put in the original df
    }
  } else {
    # do nothing if it doesn't need checking
  }
}

# fill empty cells with NAs
rr_accuracy <- rr_accuracy %>%
  mutate(lookup_event_no = na_if(lookup_event_no, ""),
         lookup_cat = na_if(lookup_cat, ""),
         lookup_word = na_if(lookup_word, "")
         )

# identify within-event category errors and between-event category errors
for (row in 1:nrow(rr_accuracy)) {
  if (!is.na(rr_accuracy$lookup_event_no[row])) {
    if (rr_accuracy$eventno[row] == rr_accuracy$lookup_event_no[row]) { # if the event numbers match
      if (rr_accuracy$category_cue[row] != rr_accuracy$lookup_cat[row]) {  # and if the categories DON'T match
        rr_accuracy$accuracy[row] <- "we_cat_error" # code it as a within-event category error (correct event, wrong category)
      }
    } else if (rr_accuracy$eventno[row] != rr_accuracy$lookup_event_no[row]) { # if the event numbers DON'T match
      if (rr_accuracy$category_cue[row] != rr_accuracy$lookup_cat[row]) {  # and if the categories DON'T match
        rr_accuracy$accuracy[row] <- "be_cat_error" # code it as a between-event category error (incorrect event, incorrect category but still in the stimuli somewhere)
      }
    }
    } else {
    # do nothing if the above conditions aren't met, as it means there's no data in the "lookup..." columns
  }
}

rr_accuracy <- rr_accuracy %>%
  select(-c(lookup_event_no,
            lookup_cat,
            lookup_word)) %>%
  mutate(accuracy_types = accuracy,
         accuracy = if_else((accuracy == "wc_event_error" | accuracy == "we_cat_error" | accuracy == "be_cat_error" | accuracy == "miss"), "error", accuracy))


# check overall accuracy again after categorising errors
accuracy_sum_3 <- rr_accuracy %>%
  group_by(participant_private_id) %>%
  mutate(perc_correct = (sum(accuracy == "1")/36)*100,
         perc_error = (sum(accuracy == "error")/36)*100,
         perc_missed = (sum(accuracy_types == "miss")/36)*100,
         perc_typo = (sum(true_typo == TRUE)/36)*100,
         perc_wc_event_error = (sum(accuracy_types == "wc_event_error")/36)*100,
         perc_we_cat_error = (sum(accuracy_types == "we_cat_error")/36)*100,
         perc_be_cat_error = (sum(accuracy_types == "be_cat_error")/36)*100,
         perc_check = (sum(accuracy == "CHECK")/36)*100,         
         ) %>%
  select(participant_private_id,
         perc_correct,
         perc_error,
         perc_missed,
         perc_typo,
         perc_wc_event_error,
         perc_we_cat_error,
         perc_be_cat_error,
         perc_check) %>%
  unique()

write_csv(accuracy_sum_3, "output/accuracy_summary_processed_3.csv")


# Identify participants for exclusion -------------------------------------


# * they report using a memory aid/help from another person/significant technical issues during the exit questionnaire

# if they're outside our age range
excluded_age <- demo %>%
  filter(age_eligibility == 0) %>%
  mutate(ID = as.character(ID),
         exc_reason = "age") %>%
  select(ID,
         exc_reason)

# if they failed attention checks
excluded_attention <- demo %>%
  filter(screening_eligibility == 0) %>%
  mutate(ID = as.character(ID),
         exc_reason = "attention") %>%
  select(ID,
         exc_reason)

# if they achieved <20% accuracy on retrieval trials
excluded_accuracy <- accuracy_sum_3 %>%
  mutate(included = if_else(perc_correct >= 20, 1, 0)
         ) %>%
  filter(included == 0) %>%
  ungroup() %>% 
  rename(ID = participant_private_id) %>%
  mutate(ID = as.character(ID),
         exc_reason = "accuracy_s1") %>%
  select(exc_reason,
         ID)

# join together to create df of all excluded ppts
excluded_ppts <- excluded_incorrect_spreadsheet %>%
  full_join(excluded_attention) %>%
  full_join(excluded_enc_attention) %>%
  full_join(excluded_rr_attention) %>%
  full_join(excluded_age) %>%
  full_join(excluded_accuracy)

# remove excluded participants from exit questionnaire data
exit_data_s1_inc <- exit_data_s1[!exit_data_s1$ID %in% excluded_ppts$ID,]

# save dfs that we need for the next part
write_csv(excluded_ppts, "output/excluded_ppts_s1a.csv")
write_csv(rr_accuracy, "output/rr_accuracy_s1a.csv")
write_csv(demo, "output/demographics_s1a.csv")
write_csv(exit_data_s1_inc, "output/exit_q_check_s1.csv")

# print to the console for the researcher's attention
print("The exit data questionnaire has been saved and requires researcher input. After manually completing this file, please proceed to running the next script process_data_s1b.csv.")

# clear the environment before running the next script
rm(list = ls())
gc()


