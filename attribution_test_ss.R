# Author: Sami Salem
# Date: 15/08/2025


# Loading packages
library(tidyverse)

jeyre_fix <- read_csv('jeyre_fix.csv')

# setting the seed for the random sample 
set.seed(1774231)

# extracting 100 rows, without replacement
jeyre_sample <- sample_n(jeyre_fix,100)


# assigning two new columns to check accuracy in speaker and addressee entries

jeyre_test <- tibble(speaker = numeric(100),addressee=numeric(100))

###### At this step a manual check will need to take place. Follow these steps for the manual check

# For a row i in jeyre_test - do the following

# 1 - Find jeyre_test$dialogue[i]. Copy and paste into the jeyre.txt file, manually find the speaker and addressee
# 2 - If speaker correct, copy past the command - 'jeyre$speaker[i] = 1' into terminal
# 3 - If addressee correct, copy past the command - 'jeyre$speaker[i] = 1' into terminal

################################################################################

# Checking accuracy

# Values for Table 11
speaker_accuracy <- sum(jeyre_test$speaker)/100
addressee_accuracy <- sum(jeyre_test$addressee)/100
full_accuracy_tibble <- (jeyre_test %>% filter(speaker == 1) %>% filter(addressee == 1))/100
full_accuracy <- length(full_accuracy_tibble$speaker)/100


