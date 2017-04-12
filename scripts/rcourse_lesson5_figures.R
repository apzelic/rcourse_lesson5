## READ IN DATA ####
source('data/rcourse_lesson5_cleaning.R')

## LOAD PACKAGES ####
library(ggplot2)

## ORGANIZE DATA FOR FIGURES ####

data_figs <- data_clean %>%
    mutate(civil_war = factor(civil_war,
           levels = c("union", "confederacy"),
           labels = c("Union", "Confederacy"))) %>%
    mutate(incumbent_party = factor(incumbent_party,
           levels = c("democrat", "republican"),
           labels = c("Democrat", "Republican")))

# For ANOVA we want to summarize over year.  This new data frame will be used for boxplotting
data_figs_state_sum <- data_figs %>%
    group_by(state, incumbent_party, civil_war) %>%
    summarise(perc_incumbent_mean =
                  mean(perc_votes_incumbent, na.rm=T)) %>%
    ungroup()

# We can also create the (less desirable) bar plots.  Now we need to average over years and also
# average over states (since we can't get the distribution of states in a bar plot, just the mean)

data_figs_sum <- data_figs_state_sum %>%
    group_by(incumbent_party, civil_war) %>%
    summarise(mean = mean(perc_incumbent_mean, na.rm=T),
              sd = sd(perc_incumbent_mean, na.rm=T),
              n = n()) %>%
    ungroup() %>%
    
    mutate(se = sd/sqrt(n)) %>%
    mutate(se_high = mean + se) %>%
    mutate(se_low = mean - se )



## CREATE FIGURES ####
incumbent_histogram_full.plot <- ggplot(data = data_figs, aes(x=perc_votes_incumbent, 
                                                               fill = incumbent_party)) + 
                    geom_histogram(bins=10) + 
                    facet_grid(incumbent_party ~ civil_war) +
                        scale_fill_manual(values = c("blue", "red"))

data_figs2 <- data_figs %>%
    filter(!is.na(votes_incumbent))

incumbent_histogram_full.plot2 <- ggplot(data = data_figs2, aes(x=perc_votes_incumbent, 
                                                              fill = incumbent_party)) + 
    geom_histogram(bins=10) + 
    facet_grid(incumbent_party ~ civil_war) +
    scale_fill_manual(values = c("blue", "red"))


incumbent_boxplot.plot <- ggplot(data = data_figs_state_sum, aes(x = civil_war, 
                                                                 y = perc_incumbent_mean,
                                                  fill = incumbent_party)) +
                                geom_boxplot() +
                                ylim(0,100) +
                                geom_hline(yintercept = 50) + 
                                scale_fill_manual(values = c("blue", "red"))




