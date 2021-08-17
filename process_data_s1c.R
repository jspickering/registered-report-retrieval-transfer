
# Intro -------------------------------------------------------------------

# load in required packages, and install if not already present
requiredPackages = c('broom', 'janitor', 'tidyverse')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


# Check agreement between accuracy raters ---------------------------------

# after rater 1 and rater 2 have finished rating the files, read them in and check the agreement

rater1 <- read_csv("data/retrieval_rating_file_1.csv") %>%
  rename(rater_1 = manual_rating)

rater2 <- read_csv("data/retrieval_rating_file_2.csv") %>%
  rename(rater_2 = manual_rating)

rated_file <- rater1 %>%
  full_join(rater2) %>%
  mutate(agreement = if_else(rater_1 == rater_2, "1", "0"))

needs_checking <- rated_file %>%
  filter(agreement == 0) %>%
  select(-c(rater_1,
            rater_2,
            agreement)) %>%
  mutate(manual_rating = "")


write_csv(needs_checking, "output/retrieval_rating_file_3.csv")


# After rating by third person --------------------------------------------

rater1 <- read_csv("data/retrieval_rating_file_1.csv") %>%
  rename(rater_1 = manual_rating)

rater2 <- read_csv("data/retrieval_rating_file_2.csv") %>%
  rename(rater_2 = manual_rating)

rater3 <- read_csv("data/retrieval_rating_file_3.csv") %>%
  rename(rater_3 = manual_rating)

rated_file <- rater1 %>%
  full_join(rater2) %>%
  mutate(agreement = if_else(rater_1 == rater_2, "1", "0")) %>%
  full_join(rater3)

write_csv(rated_file, "data/rated_file.csv")

# clear the environment before running the next script
rm(list = ls())
gc()
