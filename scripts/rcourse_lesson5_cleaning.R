## LOAD PACKAGES ####
library(dplyr)
library(purrr)

## READ IN THE DATA ####
# We'll be using the package purrr here and the cool thing about the list.files functions is that
# one can read in multiple files at the same time (but I think they have to have the same headers)

data_election_results <- list.files(path = "data/elections", full.names = TRUE) %>%
    map(read.table, header = TRUE, sep = "\t") %>%
    reduce(rbind)

data_elections = read.table("data/rcourse_lesson5_data_elections.txt",
                            header = TRUE, sep = "\t")
data_states = read.table("data/rcourse_lesson5_data_states.txt",
                         header = TRUE, sep = "\t")

## CLEAN DATA ####
# Make dataset balanced for Union and Confederacy states.  Start by removing states that
# were not around during the civil war

data_states_clean <- data_states %>%
    filter(!is.na(civil_war))

# Now we'll balance the two groups (ANOVA prefers this strongly) and do so by removing the Union
# states after the first eleven who entered (because there were also 11 Confederate states)

data_states_clean <- data_states_clean %>%
    group_by(civil_war) %>%
    arrange(order_enter) %>%
    filter(row_number() <=11) %>%
    ungroup()

# Combine three data frames
data_clean = data_election_results %>%
    inner_join(data_elections) %>%
    inner_join(data_states_clean) %>%
    mutate(state=factor(state))


