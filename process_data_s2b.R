# Explanation -------------------------------------------------------------

# This script shouldn't be run until process_data_s2a.R has been completed
# and the output/exit_q_check_s2.csv file filled in and saved as
# data/exit_q_checked_s2.csv


# Intro -------------------------------------------------------------------

# load in required packages, and install if not already present
requiredPackages = c('broom', 'janitor', 'rstatix', 'BayesFactor', 'cowplot', 'tidyverse')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


# Identify Data Files -----------------------------------------------------

# Gorilla outputs a separate csv file for each task node, so specify which csv files correspond to which task.

# Retrieval and re-exposure data
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


# exit_files2 <- c("data/session2/data_exp_22514-v6_questionnaire-k57k.csv",
#                  "data/session2/data_exp_28312-v1_questionnaire-k57k.csv")


event_key <- read_csv("data/event_key.csv") %>%
  set_names(c("event_no", "ANIMAL", "OBJECT", "LOCATION")) # removes error with first column header when reading csv made in excel

# # due to the nature of the pilot study, participant IDs have changed from session 1 to session 2 so we need to manually match them all up
# id_key <- read_csv("data/id_key.csv") %>%
#   set_names(c("id_1", "participant_private_id"))

counterbalance_key <- read_csv("data/control_counterbalance.csv") %>%
  rename(eventno = event_no,
         condition = group)

# excluded participants from session 1 and 2
excluded_ppts <- read_csv("output/excluded_ppts_s2a.csv")

# due to an error with the stimuli at final test, 4 events need to be excluded from an exploratory analysis because one element in each event was incorrect
# but for core pre-registered analysis we want to keep them, and then just show (hopefully!) that it didn't change the results
#excluded_events <- c("18", "27", "45", "51")
excluded_events <- c()

# NOTE for the below sections:
# when reading in data readr will throw up a parsing failure warning
# this is just for rows where Gorilla puts "END OF FILE" for each participant in column 1, and the other columns are left blank
# the warning can be safely ignored



# Exclusion based on exit questionnaire -----------------------------------

# read in the checked file
exit_data_checked <- read_csv("data/exit_q_checked_s2.csv")

# identify if any extra participants now need to be excluded
excluded_exit <- exit_data_checked %>%
  filter(researcher_rating == 0) %>%
  mutate(exc_reason = "exit_s2") %>%
  select(ID,
         exc_reason)

# add to the list of excluded participants
excluded_ppts <- excluded_ppts %>%
  full_join(excluded_exit)

# Exclude extra participant -----------------------------------------------

# save list of all excluded participants
write_csv(excluded_ppts, "output/excluded_participants_final.csv")

# summarise excluded participants
summary_excluded <- excluded_ppts %>%
  group_by(exc_reason) %>%
  summarise(n_ppts = n())

# Read in remaining files and add exclusion -------------------------------

# demographic info from session 1
demo <- read_csv("output/demographics_s1a.csv")
# remove all excluded participants from the df
demo_inc <- demo[!demo$ID %in% excluded_ppts$ID,] %>%
  rename(participant_private_id = ID)

# retrieval data from session 1
retrieval_data <- read_csv("data/retrieval_accuracy.csv")
# remove all excluded participants from the df
retrieval_data_inc <- retrieval_data[!retrieval_data$participant_private_id %in% excluded_ppts$ID,]

# test data from session 2
final_data <- read_csv("output/final_data_s2a.csv")
# remove all excluded participants from the final test df
final_data_inc <- final_data[!final_data$participant_private_id %in% excluded_ppts$ID,]

# quick check to make sure the dfs have equal numbers of ppts as expected
test_demo <- demo_inc %>%
  select(participant_private_id) %>%
  unique()
# N = 113

test_retrieval <- retrieval_data_inc %>%
  select(participant_private_id) %>%
  unique()
# N = 113

test_final <- final_data_inc %>%
  select(participant_private_id) %>%
  unique()
# N = 113

# Process retrieval and reexposure data -----------------------------------

rr_raw <- ret_reex_files %>% # for the list of retrieval and re-exposure files
  lapply(read_csv) %>%  # read in each
  plyr::rbind.fill() %>%  # bind together
  clean_names() # put in snake_case for consistency

# find out which of the assigned counterbalanced conditions each participant was assigned to
counterbalance_info <- rr_raw %>%
  select(participant_private_id,
         spreadsheet_name) %>%
  drop_na() %>%
  unique()


counterbalance_info_inc <- counterbalance_info[!counterbalance_info$participant_private_id %in% excluded_ppts$ID,] %>% # remove excluded ppts
  separate(spreadsheet_name, c("condition", "misc", "misc2"), sep = "_") %>% # separate the condition number from the rest of the info
  select(-c(misc,misc2)) # drop the columns we don't need
  
# identify whether they were tested retrieval practice or re-exposure trials
rr_final_test_inc <- rr_raw[!rr_raw$participant_private_id %in% excluded_ppts$ID,] %>% # remove excluded ppts
  mutate(trial_type = ifelse(display == "retrieval_trial" & zone_type == "response_text_entry", "RP_tested",                # identify rows with key retrieval practice info
                         #    ifelse(display == "retrieval_trial" & zone_type == "timelimit_screen" & response == "n/a", "RP_tested",    # same as above but timeout = "n/a"
                                    ifelse((display == "reexposure_trial" & screen_name == "reex_stimuli"), "RX_tested", "")             # identify rows with key reexposure info
                            ),
        event_type = ifelse(trial_type == "RP_tested", "RP",
                            ifelse(trial_type == "RX_tested", "RX", ""))
        ) %>%
  filter(event_type == "RP" | event_type == "RX",        # get rid of all the irrelevant rows
         eventno != "55", eventno != "56") %>% #,            # remove practice trials
        # pairtype != "98", pairtype != "99") %>%      # plus practice trials coded wrong in one of the spreadsheets
  select(participant_private_id,
         eventno,
         category_cue,
         pairtype,
         untested_element,
         trial_type,
         event_type) %>%
  full_join(counterbalance_info_inc) # add the counterbalance condition info


# Merge Phase 2 and Phase 3 data together ---------------------------------

# we need to incorporate the processed retrieval practice data (phase 2), the original reexposure data (phase 2), and the final test data (phase 3) all into one

# get the retrieval and reexposure trials from Phase 2
phase_2_rr_rx <- rr_final_test_inc %>%
  mutate(eventno = as.numeric(eventno)) # to make it match the processed data

# remove ineligible events (we'll do this in exploratory analysis instead)
#phase_2_rr_rx <- phase_2_rr_rx[!phase_2_rr_rx$eventno %in% excluded_events,]

# get the retrieval trials from Phase 2 including accuracy of their typed responses
phase2_retrieval_acc <- retrieval_data_inc[!retrieval_data_inc$participant_private_id %in% excluded_ppts$ID,] %>% # remove ineligible participants
  select(participant_private_id,
         eventno,
         category_cue,
         pairtype,
         untested_element,
         accuracy_p2 = accuracy,
         accuracy_types_p2 = accuracy_types) %>% # to set up naming conventions when they're merged
  mutate(accuracy_p2 = if_else(accuracy_p2 == "error", "0", accuracy_p2), # remove error labels and have accuracy as binary 0 or 1
         accuracy_p2 = as.numeric(accuracy_p2))

## remove ineligible events (we'll do this in exploratory analysis instead)
#phase2_retrieval_acc <- phase2_retrieval_acc[!phase2_retrieval_acc$eventno %in% excluded_events,]


# merge the retrieval accuracy into the overall retrieval/reexposure phase 2 data
phase2_data <- phase_2_rr_rx %>%
  full_join(phase2_retrieval_acc) %>%
  mutate(test_cat = if_else(trial_type == "RP_tested", "A",      # code tested RP trials as category A for analysis
                            if_else(trial_type == "RX_tested", "C", ""))) # code reexposed trials as category C for analysis

# check to see that participants have 324 trials each (as expected)
trials_summary <- final_data_inc %>%
  group_by(participant_private_id) %>%
  summarise(trials_per_person = n())
# they do!

# get the Phase 3 data
phase3_data <- final_data_inc %>%
  mutate(accuracy_p3 = correct,   # match variable naming conventions from phase2 data
         accuracy_types_p3 = accuracy_p3,
         accuracy_p3 = as.numeric(if_else(accuracy_p3 == "miss", "0", accuracy_p3)), # binary 0 or 1
         rt_p3 = reaction_time) %>%
  select(-c(correct, # remove unnecessary columns
            reaction_time))

# merge phase 2 data with phase 3 data
merged_data <- phase3_data %>%
  merge(phase2_data, all = TRUE) %>%
  arrange(participant_private_id, eventno)



# Identify all RP, RX, and CT trials --------------------------------------

# using the counterbalance key we can identify all CT trials
merged_data2 <- merged_data %>%
  mutate(condition = as.numeric(condition)) %>%
  group_by(participant_private_id) %>%
  fill(condition, .direction= "downup") %>% # only tested RX and RP trials have condition info but we want this for all rows within each participant
  full_join(counterbalance_key) %>% # adds the untested element to all events that were control trials, allowing us to identify them
  mutate(event_type = if_else(!is.na(untested), "CT", event_type)) %>%
  group_by(participant_private_id, eventno) %>%
  fill(c(untested_element,event_type), .direction= "downup") %>%
  mutate(untested_element = if_else(is.na(untested_element), untested, untested_element)) %>% # if no data get it from untested column
  select(-c(untested))%>% # no longer need this column
  mutate(
    # using str_detect we can see if the untested element is featured in the pairtype for that trial
    # if it is, we know it's an untested trial and can label accordingly
    trial_type = if_else(event_type == "RP" & str_detect(pairtype, untested_element) == TRUE, "RP_untested",
                        if_else(event_type == "RX" & str_detect(pairtype, untested_element) == TRUE, "RX_untested",
                                if_else(event_type == "CT" & str_detect(pairtype, untested_element) == TRUE, "CT_untested",
                                        if_else(event_type == "CT" & str_detect(pairtype, untested_element) == FALSE, "CT_tested", trial_type))))
         ) 


merged_data3 <- merged_data2 %>%
  # can now label all test categories for analysis
  # so far we only have A and C labelled
  mutate(test_cat = if_else(trial_type == "RP_untested", "B",
                            if_else(trial_type == "RX_untested", "D",
                                    if_else(trial_type == "CT_tested", "E",
                                            if_else(trial_type == "CT_untested", "F", test_cat)))),
  # we can also label trials based on stimulus-response arrangement       
        s_r_arrange  = if_else(pairtype == "AL" & untested_element == "O", "full_repeat",    
                          if_else(pairtype == "AO" & untested_element == "O", "cue_repeat",
                                  if_else(pairtype == "LA" & untested_element == "O", "full_repeat",
                                          if_else(pairtype == "LO" & untested_element == "O", "cue_repeat",
                                                  if_else(pairtype == "OA" & untested_element == "O", "target_repeat",
                                                          if_else(pairtype == "OL" & untested_element == "O", "target_repeat",
                                                                  if_else(pairtype == "AL" & untested_element == "A", "target_repeat",
                                                                          if_else(pairtype == "AO" & untested_element == "A", "target_repeat",
                                                                                  if_else(pairtype == "LA" & untested_element == "A", "cue_repeat",
                                                                                          if_else(pairtype == "LO" & untested_element == "A", "full_repeat",
                                                                                                  if_else(pairtype == "OA" & untested_element == "A", "cue_repeat",
                                                                                                          if_else(pairtype == "OL" & untested_element == "A", "full_repeat",
                                                                                                                  if_else(pairtype == "AL" & untested_element == "L", "cue_repeat",
                                                                                                                          if_else(pairtype == "AO" & untested_element == "L", "full_repeat",
                                                                                                                                  if_else(pairtype == "LA" & untested_element == "L", "target_repeat",
                                                                                                                                          if_else(pairtype == "LO" & untested_element == "L", "target_repeat",
                                                                                                                                                  if_else(pairtype == "OA" & untested_element == "L", "full_repeat",
                                                                                                                                                          if_else(pairtype == "OL" & untested_element == "L", "cue_repeat", ""
                                                                                                                                                          )))))))))))))))))),
    sr_cat = if_else(event_type == "RP" & s_r_arrange == "cue_repeat", "RP_cue_repeat",
                     if_else(event_type == "RP" & s_r_arrange == "target_repeat", "RP_target_repeat",
                             if_else(event_type == "RX" & s_r_arrange == "cue_repeat", "RX_cue_repeat",
                                     if_else(event_type == "RX" & s_r_arrange == "target_repeat", "RX_target_repeat",
                                             if_else(event_type == "RP" & s_r_arrange == "full_repeat", "RP_full_repeat",
                                                     if_else(event_type == "RX" & s_r_arrange == "full_repeat", "RX_full_repeat", "NA"))))))
         ) %>%
  ungroup()



# Basic summary data ------------------------------------------------------

# age summary
age <- demo_inc %>%
  summarise(
    mean_age = mean(age),
    sd_age = sd(age),
    min_age = min(age),
    max_age = max(age)
  )

# gender summary
gender <- demo_inc %>%
  mutate(genders = ifelse(gender == "Male", gender,
                       ifelse(gender == "Female", gender,
                              ifelse(gender == "Prefer to self-describe (please specify below)", gender_described, "missing data")))) %>%
  group_by(genders) %>%
  summarise(n_gender = n())

# cued recall summary
cued_recall_data <- merged_data3 %>%
  filter(trial_type == "RP_tested") %>%
  select(participant_private_id,
         accuracy_types_p2) %>%
  group_by(participant_private_id) %>%
  mutate(perc_correct = (sum(accuracy_types_p2 == "1")/length(participant_private_id))*100,
         perc_missed = (sum(accuracy_types_p2 == "miss")/length(participant_private_id))*100,
         perc_wc_event_error = (sum(accuracy_types_p2 == "wc_event_error")/length(participant_private_id))*100,
         perc_we_cat_error = (sum(accuracy_types_p2 == "we_cat_error")/length(participant_private_id))*100,
         perc_be_cat_error = (sum(accuracy_types_p2 == "be_cat_error")/length(participant_private_id))*100,         
         perc_error = (sum(accuracy_types_p2 == "error")/length(participant_private_id))*100,
         # and just to verify this adds up to 100% as expected:
         total = perc_correct + perc_error + perc_missed + perc_wc_event_error + perc_we_cat_error + perc_be_cat_error
  ) %>%
  select(-c(accuracy_types_p2)) %>% # drop irrelevant row
  unique()

cued_recall_summary <- cued_recall_data %>%
  ungroup() %>%
  summarise(
    mean_correct = mean(perc_correct),
    sd_correct = sd(perc_correct),
    mean_missed = mean(perc_missed),
    sd_missed = sd(perc_missed),
    mean_wc_event_error = mean(perc_wc_event_error),
    sd_wc_event_error = sd(perc_wc_event_error),
    mean_we_cat_error = mean(perc_we_cat_error),
    sd_we_cat_error = sd(perc_we_cat_error),
    mean_be_cat_error = mean(perc_be_cat_error),
    sd_be_cat_error = sd(perc_be_cat_error),
    mean_error = mean(perc_error),
    sd_error = sd(perc_error)
    )


# Summarise and reformat data ---------------------------------------------

summary_data_long <- merged_data3 %>%
  group_by(participant_private_id, test_cat) %>%
  summarise(mean = mean(accuracy_p3, na.rm = TRUE))

summary_data_wide <- summary_data_long %>%
  ungroup() %>%
  spread(key = test_cat, value = mean)

stim_resp_data_long <- merged_data3 %>%
  filter(sr_cat != "NA") %>%
  group_by(participant_private_id, sr_cat) %>%
  summarise(mean = mean(accuracy_p3, na.rm = TRUE))

stim_resp_data_wide <- stim_resp_data_long %>%
  ungroup() %>%
  spread(key = sr_cat, value = mean)

# summary data for retrieval practice and transfer effects
summary_test_cat <- summary_data_wide %>%
  summarise(mean_A = mean(A)*100,
            sd_A = sd(A)*100,
            mean_B = mean(B)*100,
            sd_B = sd(B)*100,
            mean_C = mean(C)*100,
            sd_C = sd(C)*100,
            mean_D = mean(D)*100,
            sd_D = sd(D)*100,
            mean_E = mean(E)*100,
            sd_E = sd(E)*100,
            mean_F = mean(F)*100,
            sd_F = sd(F)*100
            )

# summary data for stimulus response arrangement
summary_stim_resp <- stim_resp_data_wide %>%
  summarise(mean_RP_cue_repeat = mean(RP_cue_repeat)*100,
            sd_RP_cue_repeat = sd(RP_cue_repeat)*100,
            mean_RP_full_repeat = mean(RP_full_repeat)*100,
            sd_RP_full_repeat = sd(RP_full_repeat)*100,
            mean_RP_target_repeat = mean(RP_target_repeat)*100,
            sd_RP_target_repeat = sd(RP_target_repeat)*100,
            mean_RX_cue_repeat = mean(RX_cue_repeat)*100,
            sd_RX_cue_repeat = sd(RX_cue_repeat)*100,
            mean_RX_full_repeat = mean(RX_full_repeat)*100,
            sd_RX_full_repeat = sd(RX_full_repeat)*100,
            mean_RX_target_repeat = mean(RX_target_repeat)*100,
            sd_RX_target_repeat = sd(RX_target_repeat)*100
            )


# Data visualisation ------------------------------------------------------

summary_data_plot <- summary_data_long %>%
  mutate(test_cat_labels = if_else(test_cat == "A", "(A)\ntested",
                                   if_else(test_cat == "B", "(B)\nuntested",
                                           if_else(test_cat == "C", "(C)\nre-exposed",
                                                   if_else(test_cat == "D", "(D)\nnon-exposed",
                                                           if_else(test_cat == "E", "(E)\ncontrol\n(\"tested\")",
                                                                   if_else(test_cat == "F", "(F)\ncontrol\n(\"untested\")", "NA")))))),
         test_cat_labels = as_factor(test_cat_labels))



# read in the rainclouds script downloaded from https://github.com/RainCloudPlots/RainCloudPlots
source("R_rainclouds.R")
w = 6
h = 4

# basic raincloud for main hypotheses
p2 <- ggplot(summary_data_plot, aes(x = test_cat_labels, y = mean, fill = test_cat_labels, colour = test_cat_labels)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = .6) +
  geom_point(position = position_jitter(width = .15), size = .6, shape = 16) +
  geom_boxplot(aes(x = as.numeric(test_cat_labels)+0.2, y = mean), outlier.shape = NA, alpha = 0.3, width = .1, colour = "black") +
  ylab('Proportion correct') +
  #xlab('Pair types at final test') +
  theme_cowplot() +
  scale_y_continuous(limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10)) +
  scale_fill_manual(values=c("#6b0a82", "#9e71a8", "#238a8d", "#8bc8c9", "#666666", "#bfbfbf")) +
  scale_colour_manual(values=c("#6b0a82", "#9e71a8", "#238a8d", "#8bc8c9", "#666666", "#bfbfbf")) +
  theme(legend.position = "none")#,
  #       panel.background = element_rect(fill = "#e3e5e5",
  #                                       colour = "#e3e5e5"),
  #       plot.background = element_rect(fill = "#e3e5e5"))
ggsave('figs/p2.png', width = w, height = h)


summary_stim_data_plot <- stim_resp_data_long %>%
  mutate(sr_cat_labels = if_else(sr_cat == "RP_full_repeat", "RP\nrepeat both",
                                 if_else(sr_cat == "RP_cue_repeat", "RP\nrepeat cue",
                                         if_else(sr_cat == "RP_target_repeat", "RP\nrepeat target",
                                                 if_else(sr_cat == "RX_full_repeat", "Re-exp.\nrepeat both",
                                                         if_else(sr_cat == "RX_cue_repeat", "Re-exp.\nrepeat cue",
                                                                 if_else(sr_cat == "RX_target_repeat", "Re-exp.\nrepeat target", "NA")))))))

summary_stim_data_plot$sr_cat_labels <- factor(summary_stim_data_plot$sr_cat_labels, levels = c("RP\nrepeat both",
                                                                                                "RP\nrepeat cue",
                                                                                                "RP\nrepeat target",
                                                                                                "Re-exp.\nrepeat both",
                                                                                                "Re-exp.\nrepeat cue",
                                                                                                "Re-exp.\nrepeat target"))


# basic raincloud for main hypotheses
p4 <- ggplot(summary_stim_data_plot, aes(x = sr_cat_labels, y = mean, fill = sr_cat_labels, colour = sr_cat_labels)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = .6) +
  geom_point(position = position_jitter(width = .15), size = .6, shape = 16) +
  geom_boxplot(aes(x = as.numeric(sr_cat_labels)+0.2, y = mean), outlier.shape = NA, alpha = 0.3, width = .1, colour = "black") +
  ylab('Proportion correct') +
  xlab('Stimulus-response arrangement') +
  theme_cowplot() +
  scale_y_continuous(limits = c(0, 1.00)) + 
  theme(axis.text.x = element_text(angle = 0,
                                   size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  scale_fill_manual(values=c("#6b0a82", "#cc6a70", "#f68f46", "#238a8d", "#13306d", "#0c2a50b2")) +
  scale_colour_manual(values=c("#6b0a82", "#cc6a70", "#f68f46", "#238a8d", "#13306d", "#0c2a50b2")) +
  theme(legend.position = "none") #+
  # annotate("segment", x = 0.7, xend = 3.65, y = 0.13, yend = 0.13, colour = "#424242") +
  # annotate("segment", x = 0.7, xend = 0.7, y = 0.13, yend = 0.16, colour = "#424242") +
  # annotate("segment", x = 3.65, xend = 3.65, y = 0.13, yend = 0.16, colour = "#424242") +
  # annotate("text", x = 2.3, y = 0.08, label = "Retrieval practice", colour = "#424242")
#       panel.background = element_rect(fill = "#e3e5e5",
#                                       colour = "#e3e5e5"),
#       plot.background = element_rect(fill = "#e3e5e5"))
ggsave('figs/p4.png', width = w, height = h)


# Inferential statistics: Hypothesis 1a ------------------------------------

# Hypothesis 1a: one tailed t-test, A vs E, a = 0.025

# get the relevant data
hyp_1a_data_long <- summary_data_long %>%
  filter(test_cat == "A" | test_cat == "E") %>%
  mutate(test_cat_labels = if_else(test_cat == "A", "Retrieval practice", 
                            if_else(test_cat == "E", "Control", "NA")))

h1a_plot <- ggplot(hyp_1a_data_long, aes(x = test_cat_labels, y = mean)) +
  geom_violin() +
  geom_jitter(alpha = 0.6, width = 0.15, colour = "blue", size = 2) +
#  geom_point(position = "jitter", alpha = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 18) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Condition") +
  ylab("Mean % correct")
h1a_plot

# t-test in tidy format
hyp_1a_ttest <- t.test(mean ~ test_cat, data = hyp_1a_data_long, alternative = "greater", paired = TRUE) %>%
  tidy()

# bayes factor
hyp_1a_bf <- extractBF(ttestBF(x = summary_data_wide$A, y = summary_data_wide$E, paired = TRUE))

# effect size (Cohen's d)
hyp_1a_es <- ((mean(summary_data_wide$A) - mean(summary_data_wide$E)) / (sqrt(((sd(summary_data_wide$A))^2 + (sd(summary_data_wide$E))^2) / 2)))


# Inferential statistics: Hypothesis 1b -----------------------------------

# Hypothesis 1b: one tailed t-test, A vs C, a = 0.025

# get the relevant data
hyp_1b_data_long <- summary_data_long %>%
  filter(test_cat == "A" | test_cat == "C") %>%
  mutate(test_cat_labels = if_else(test_cat == "A", "Retrieval practice", 
                                   if_else(test_cat == "C", "Re-exposure", "NA")))

h1b_plot <- ggplot(hyp_1b_data_long, aes(x = test_cat_labels, y = mean)) +
  geom_violin() +
  geom_jitter(alpha = 0.6, width = 0.15, colour = "blue", size = 2) +
  #  geom_point(position = "jitter", alpha = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 18) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Condition") +
  ylab("Mean % correct")
h1b_plot

# t-test in tidy format
hyp_1b_ttest <- t.test(mean ~ test_cat, data = hyp_1b_data_long, alternative = "greater", paired = TRUE) %>%
  tidy()

# bayes factor
hyp_1b_bf <- extractBF(ttestBF(x = summary_data_wide$A, y = summary_data_wide$C, paired = TRUE))

# effect size (Cohen's d)
hyp_1b_es <- ((mean(summary_data_wide$A) - mean(summary_data_wide$C)) / (sqrt(((sd(summary_data_wide$A))^2 + (sd(summary_data_wide$C))^2) / 2)))


# EXPLORATORY inferential statistics: "Hypothesis" 1c ---------------------

# additional one-tailed exploratory test to compare re-exposure to control i.e. C vs E, one-tailed
# alpha level = 0.016 (i.e. its the third test in the family, although 1a and 1b were pre-reg'd at 0.025 this no longer makes sense for the third - exploratory - test)

# get the relevant data
hyp_1c_data_long <- summary_data_long %>%
  filter(test_cat == "C" | test_cat == "E") %>%
  mutate(test_cat_labels = if_else(test_cat == "C", "Re-exposure",
                                   if_else(test_cat == "E", "Control", "NA")))

h1c_plot <- ggplot(hyp_1c_data_long, aes(x = test_cat_labels, y = mean)) +
  geom_violin() +
  geom_jitter(alpha = 0.6, width = 0.15, colour = "blue", size = 2) +
  #  geom_point(position = "jitter", alpha = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 18) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Condition") +
  ylab("Mean % correct")
h1c_plot

# t-test in tidy format
hyp_1c_ttest <- t.test(mean ~ test_cat, data = hyp_1c_data_long, alternative = "greater", paired = TRUE) %>%
  tidy()

# bayes factor
hyp_1c_bf <- extractBF(ttestBF(x = summary_data_wide$C, y = summary_data_wide$E, paired = TRUE))

# effect size (Cohen's d)
hyp_1c_es <- ((mean(summary_data_wide$C) - mean(summary_data_wide$E)) / (sqrt(((sd(summary_data_wide$C))^2 + (sd(summary_data_wide$E))^2) / 2)))


# Inferential statistics: Hypothesis 2a -----------------------------------

# Hypothesis 2a: one tailed t-test, B vs F, a = 0.025

# get the relevant data
hyp_2a_data_long <- summary_data_long %>%
  filter(test_cat == "B" | test_cat == "F") %>%
  mutate(test_cat_labels = if_else(test_cat == "B", "Untested pairs from retrieval practice events",
                                   if_else(test_cat == "F", "Control", "NA")))

h2a_plot <- ggplot(hyp_2a_data_long, aes(x = test_cat_labels, y = mean)) +
  geom_violin() +
  geom_jitter(alpha = 0.6, width = 0.15, colour = "blue", size = 2) +
  #  geom_point(position = "jitter", alpha = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 18) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Condition") +
  ylab("Mean % correct")
h2a_plot



# t-test in tidy format
hyp_2a_ttest <- t.test(mean ~ test_cat, data = hyp_2a_data_long, alternative = "greater", paired = TRUE) %>%
  tidy()

# bayes factor
hyp_2a_bf <- extractBF(ttestBF(x = summary_data_wide$B, y = summary_data_wide$F, paired = TRUE))

# effect size (Cohen's d)
hyp_2a_es <- ((mean(summary_data_wide$B) - mean(summary_data_wide$F)) / (sqrt(((sd(summary_data_wide$B))^2 + (sd(summary_data_wide$F))^2) / 2)))



# Inferential statistics: Hypothesis 2b -----------------------------------

# Hypothesis 2b: one tailed t-test, B vs D, a = 0.025

# get the relevant data
hyp_2b_data_long <- summary_data_long %>%
  filter(test_cat == "B" | test_cat == "D") %>%
  mutate(test_cat_labels = if_else(test_cat == "B", "Untested pairs (retrieval practice)",
                                   if_else(test_cat == "D", "Non-exposed pairs (re-exposure)", "NA")))

h2b_plot <- ggplot(hyp_2b_data_long, aes(x = test_cat_labels, y = mean)) +
  geom_violin() +
  geom_jitter(alpha = 0.6, width = 0.15, colour = "blue", size = 2) +
  #  geom_point(position = "jitter", alpha = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 18) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Condition") +
  ylab("Mean % correct")
h2b_plot

# t-test in tidy format
hyp_2b_ttest <- t.test(mean ~ test_cat, data = hyp_2b_data_long, alternative = "greater", paired = TRUE) %>%
  tidy()

# bayes factor
hyp_2b_bf <- extractBF(ttestBF(x = summary_data_wide$B, y = summary_data_wide$D, paired = TRUE))

# effect size (Cohen's d)
hyp_2b_es <- ((mean(summary_data_wide$B) - mean(summary_data_wide$D)) / (sqrt(((sd(summary_data_wide$B))^2 + (sd(summary_data_wide$D))^2) / 2)))



# EXPLORATORY inferential statistics: "Hypothesis" 2c ---------------------

# additional one-tailed exploratory test to compare re-exposure to control i.e. D vs F, one-tailed
# alpha level = 0.016 (i.e. its the third test in the family, although 2a and 2b were pre-reg'd at 0.025 this no longer makes sense for the third - exploratory - test)

# get the relevant data
hyp_2c_data_long <- summary_data_long %>%
  filter(test_cat == "D" | test_cat == "F") %>%
  mutate(test_cat_labels = if_else(test_cat == "D", "Non-exposed pairs (re-exposure)",
                                   if_else(test_cat == "F", "Control", "NA")))

h2c_plot <- ggplot(hyp_2c_data_long, aes(x = test_cat_labels, y = mean)) +
  geom_violin() +
  geom_jitter(alpha = 0.6, width = 0.15, colour = "blue", size = 2) +
  #  geom_point(position = "jitter", alpha = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 18) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Condition") +
  ylab("Mean % correct")
h2c_plot


# t-test in tidy format
hyp_2c_ttest <- t.test(mean ~ test_cat, data = hyp_2c_data_long, alternative = "greater", paired = TRUE) %>%
  tidy()

# bayes factor
hyp_2c_bf <- extractBF(ttestBF(x = summary_data_wide$D, y = summary_data_wide$F, paired = TRUE))

# effect size (Cohen's d)
hyp_2c_es <- ((mean(summary_data_wide$D) - mean(summary_data_wide$F)) / (sqrt(((sd(summary_data_wide$D))^2 + (sd(summary_data_wide$F))^2) / 2)))



# Inferential statistics: Hypothesis 3a -----------------------------------

# Hypothesis 3a: one tailed t-test, repeat_cue_diff_target for RP vs repeat_cue_diff_target for RX, a = 0.016

# get the relevant data
hyp_3a_data_long <- stim_resp_data_long %>%
  filter(sr_cat == "RP_cue_repeat" | sr_cat == "RX_cue_repeat") %>%
  mutate(sr_cat_labels = if_else(sr_cat == "RP_cue_repeat", "RP: same cue, different target",
                                 if_else(sr_cat == "RX_cue_repeat", "RX: same cue, diffrent target", "NA")))

h3a_plot <- ggplot(hyp_3a_data_long, aes(x = sr_cat_labels, y = mean)) +
  geom_violin() +
  geom_jitter(alpha = 0.6, width = 0.15, colour = "blue", size = 2) +
  #  geom_point(position = "jitter", alpha = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 18) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Condition") +
  ylab("Mean % correct")
h3a_plot

# t-test in tidy format
hyp_3a_ttest <- t.test(mean ~ sr_cat, data = hyp_3a_data_long, alternative = "greater", paired = TRUE) %>%
  tidy()

# bayes factor
hyp_3a_bf <- extractBF(ttestBF(x = stim_resp_data_wide$RP_cue_repeat, y = stim_resp_data_wide$RX_cue_repeat, paired = TRUE))

# effect size (Cohen's d)
hyp_3a_es <- ((mean(stim_resp_data_wide$RP_cue_repeat) - mean(stim_resp_data_wide$RX_cue_repeat)) / (sqrt(((sd(stim_resp_data_wide$RP_cue_repeat))^2 + (sd(stim_resp_data_wide$RX_cue_repeat))^2) / 2)))



# Inferential statistics: Hypothesis 3b -----------------------------------

# Hypothesis 3b: one tailed t-test, diff_cue_repeat_target for RP vs diff_cue_repeat_target for RX, a = 0.016

# get the relevant data
hyp_3b_data_long <- stim_resp_data_long %>%
  filter(sr_cat == "RP_target_repeat" | sr_cat == "RX_target_repeat") %>%
  mutate(sr_cat_labels = if_else(sr_cat == "RP_target_repeat", "RP: different cue, same target",
                                 if_else(sr_cat == "RX_target_repeat", "RX: different cue, same target", "NA")))  


h3b_plot <- ggplot(hyp_3b_data_long, aes(x = sr_cat_labels, y = mean)) +
  geom_violin() +
  geom_jitter(alpha = 0.6, width = 0.15, colour = "blue", size = 2) +
  #  geom_point(position = "jitter", alpha = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 18) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Condition") +
  ylab("Mean % correct")
h3b_plot


# t-test in tidy format
hyp_3b_ttest <- t.test(mean ~ sr_cat, data = hyp_3b_data_long, alternative = "greater", paired = TRUE) %>%
  tidy()

# bayes factor
hyp_3b_bf <- extractBF(ttestBF(x = stim_resp_data_wide$RP_target_repeat, y = stim_resp_data_wide$RX_target_repeat, paired = TRUE))

# effect size (Cohen's d)
hyp_3b_es <- ((mean(stim_resp_data_wide$RP_target_repeat) - mean(stim_resp_data_wide$RX_target_repeat)) / (sqrt(((sd(stim_resp_data_wide$RP_target_repeat))^2 + (sd(stim_resp_data_wide$RX_target_repeat))^2) / 2)))



# Inferential statistics: Hypothesis 3c -----------------------------------

# Hypothesis 3c: two tailed t-test, repeat_cue_diff_target for RP vs diff_cue_repeat_target for RP, a = 0.016

# get the relevant data
hyp_3c_data_long <- stim_resp_data_long %>%
  filter(sr_cat == "RP_cue_repeat" | sr_cat == "RP_target_repeat") %>%
  mutate(sr_cat_labels = if_else(sr_cat == "RP_cue_repeat", "RP: same cue, different target",
                                 if_else(sr_cat == "RP_target_repeat", "RP: different cue, same target", "NA")))    


h3c_plot <- ggplot(hyp_3c_data_long, aes(x = sr_cat_labels, y = mean)) +
  geom_violin() +
  geom_jitter(alpha = 0.6, width = 0.15, colour = "blue", size = 2) +
  #  geom_point(position = "jitter", alpha = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 18) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Condition") +
  ylab("Mean % correct")
h3c_plot

# t-test in tidy format
hyp_3c_ttest <- t.test(mean ~ sr_cat, data = hyp_3c_data_long, paired = TRUE) %>%
  tidy()

# bayes factor
hyp_3c_bf <- extractBF(ttestBF(x = stim_resp_data_wide$RP_cue_repeat, y = stim_resp_data_wide$RP_target_repeat, paired = TRUE))

# effect size (Cohen's d)
hyp_3c_es <- ((mean(stim_resp_data_wide$RP_cue_repeat) - mean(stim_resp_data_wide$RP_target_repeat)) / (sqrt(((sd(stim_resp_data_wide$RP_cue_repeat))^2 + (sd(stim_resp_data_wide$RP_target_repeat))^2) / 2)))


# EXPLORATORY inferential statistics: "Hypothesis" 3d ---------------------

# additional two-tailed exploratory test to compare repeat_cue_diff_target for RX vs diff_cue_repeat_target for RX
# alpha level = 0.0125 (i.e. its the fourth test in the family, although 3a/3b/3c were pre-reg'd at 0.016 this no longer makes sense for the fourth - exploratory - test)


# get the relevant data
hyp_3d_data_long <- stim_resp_data_long %>%
  filter(sr_cat == "RX_cue_repeat" | sr_cat == "RX_target_repeat") %>%
  mutate(sr_cat_labels = if_else(sr_cat == "RX_cue_repeat", "RX: same cue, different target",
                                 if_else(sr_cat == "RX_target_repeat", "RX: different cue, same target", "NA"))) 


h3d_plot <- ggplot(hyp_3d_data_long, aes(x = sr_cat_labels, y = mean)) +
  geom_violin() +
  geom_jitter(alpha = 0.6, width = 0.15, colour = "blue", size = 2) +
  #  geom_point(position = "jitter", alpha = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 18) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Condition") +
  ylab("Mean % correct")
h3d_plot

# t-test in tidy format
hyp_3d_ttest <- t.test(mean ~ sr_cat, data = hyp_3d_data_long, paired = TRUE) %>%
  tidy()

# bayes factor
hyp_3d_bf <- extractBF(ttestBF(x = stim_resp_data_wide$RX_cue_repeat, y = stim_resp_data_wide$RX_target_repeat, paired = TRUE))

# effect size (Cohen's d)
hyp_3d_es <- ((mean(stim_resp_data_wide$RX_cue_repeat) - mean(stim_resp_data_wide$RX_target_repeat)) / (sqrt(((sd(stim_resp_data_wide$RX_cue_repeat))^2 + (sd(stim_resp_data_wide$RX_target_repeat))^2) / 2)))



# EXPLORATORY inferential statistics: "Hypothesis" 3d alternative ---------

# similar to above but with a 2x2 ANOVA
# alpha level = 0.0125 (if used as an alternative to the original 3d, otherwise will need to make the alpha level even smaller)

hyp_3d_data_long_alt <- stim_resp_data_long %>%
  filter(sr_cat == "RP_cue_repeat" | sr_cat == "RP_target_repeat" | sr_cat == "RX_cue_repeat" | sr_cat == "RX_target_repeat") %>%
  mutate(condition = if_else(startsWith(sr_cat, "RP"), "RP",
                             if_else(startsWith(sr_cat, "RX"), "RX", "error")),
         sr = if_else(endsWith(sr_cat, "cue_repeat"), "cue_repeat",
                      if_else(endsWith(sr_cat, "target_repeat"), "target_repeat", "error")),
         condition = as_factor(condition),
         sr = as_factor(sr)) %>%
  select(!sr_cat)

# anova
hyp_3d_anova <- aov(mean ~ condition + sr + condition:sr, data = hyp_3d_data_long_alt) %>%
  tidy()
summary(hyp_3d_anova)



# EXPLORATORY inferential statistics: RP or RX first ----------------------

hyp_5_data_long <- merged_data3 %>%
  select(participant_private_id,
         test_cat,
         condition,
         accuracy_p3) %>%
  mutate(condition = as_factor(condition),
         first_block = if_else(condition == "1" | condition == "2" | condition == "3" | condition == "4" |
                                 condition == "5" | condition == "6" | condition == "7" | condition == "8" | condition == "9", "retrieval",
                               if_else(condition == "10" | condition == "11" | condition == "12" | condition == "13" |
                                         condition == "14" | condition == "15" | condition == "16" | condition == "17" | condition == "18", "reexposure", "error")),
         first_block = as_factor(first_block)) %>%
  filter(test_cat == "A" | test_cat == "C") %>%
  group_by(participant_private_id, first_block, test_cat) %>%
  summarise(mean = mean(accuracy_p3, na.rm = TRUE))


hyp_5_anova <- aov(mean ~ first_block + test_cat + first_block:test_cat, data = hyp_5_data_long)
summary(hyp_5_anova)
