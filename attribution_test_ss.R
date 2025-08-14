# Author: Sami Salem
# Date: 11/08/2025


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

speaker_accuracy <- sum(jeyre_test$speaker)/100
addressee_accuracy <- sum(jeyre_test$addressee)/100
full_accuracy <- sum(jeyre_test %>% filter(speaker == 1) %>% filter(addressee == 1))/100



########### FOR FIRST HALF NOT CHECKING IF PAIR CORRECT, BUT ORDER WRONG, SECOND I AM

# Populated first 20 as 0f 9/8/25

# speaker accuracy 0.5, addressee 0.4, both same time 0.25

# Populated first 50 as of 10/8/2025 21:42. Speaker accuracy 0.48, Addressee 0.42, both 0.28

# Populated first 60 as of 10/8/2025 21:58. Speaker accuracy 0.53, Addressee 0.5, both 0.35

# Populated first 70 as of 10/8/2025 22:51. Speaker accuracy 0.57, Addressee 0.51, both 0.37

# Populated first 80 as of 10/8/2025 23:02. Speaker accuracy 0.58, Addressee 0.49, both 0.35

# Populated all 100 as of 11/8/2025 12:51. Speaker accuracy 0.52, Addressee 0.47, both 0.32, (5 out of 50 has speaker-addressee-swap)

############## COMPLETE ########################################################






