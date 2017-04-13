library(tidyr)
library(ez)

## READ IN DATA ####
load('scripts/rcourse_lesson5_cleaning.R')

# Make data for statistics
# we want to reorder leels for "civil_war" and then use 'group_by' to summarize over year

## ORGANIZE DATA FOR STATISTICAL ANALYSIS ####
data_stats <- data_clean %>%
    mutate(civil_war = factor(civil_war, levels = c("union", "confederacy"))) %>%
    group_by(state, incumbent_party, civil_war) %>%
    summarise(perc_incumbent_mean = mean(perc_votes_incumbent, na.rm=T)) %>%
    ungroup()


## BUILD MODELS ####
incumbent.aov <- aov(perc_incumbent_mean ~ incumbent_party * civil_war +
                         Error(state/incumbent_party), data = data_stats)

incumbent.aov_sum = summary(incumbent.aov)

# ezANOVA
incumbent.ezanova <- ezANOVA(data.frame(data_stats),
                             dv = perc_incumbent_mean,
                             wid = state,
                             within = incumbent_party,
                             between = civil_war,
                             type = 3)


# Now to analyze the results of the ANOVAs further, we will do t-tests both paired and unpaired
# (1) Percent Republican and Democrat for Union States
# (2) Percent Republican and Democrat for Confederate States
# (3) Democratic incumbents, Union vs. Conferderacy
# (4) Republican incumbents, Union vs. Confederacy

#The first two are paired t-tests and the latter two are grouped t-tests (grouped for party)


data_union_stats <- data_stats %>%
    filter(civil_war == "union") %>%
    spread(incumbent_party, perc_incumbent_mean)

data_confederacy_stats <- data_stats %>%
    filter(civil_war == "confederacy") %>%
    spread(incumbent_party, perc_incumbent_mean)

data_democratic_stats <- data_stats %>%
    filter(incumbent_party == "democrat")

data_republican_stats <- data_stats %>%
    filter(incumbent_party == "republican")


incumbent_union.ttest = t.test(data_union_stats$democrat,
                               data_union_stats$republican,
                               paired = T)

incumbent_confederacy.ttest = t.test(data_confederacy_stats$democrat,
                               data_confederacy_stats$republican,
                               paired = T)

incumbent_democratic.ttest = t.test(perc_incumbent_mean ~ civil_war,
                                    data = data_democratic_stats,
                                    paired = FALSE)



