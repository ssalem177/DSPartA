# Author: Sami Salem
# Last Modified: Aug 15, 2025

# loading required packages
library(tidyverse)
library(dplyr)
library(tidytext)
library(textstem)

# Loading the populated jeyre database

jeyre_fix <- read_csv("jeyre_fix.csv")

###########THE FOLLOWING CODE IS USED TO ANALYSE DIFFERENT DICTIONARIES#########

# Comparing "afinn" and "bing" dictionary by coverage

dialogue <- bind_cols(interactions = jeyre_fix$dialogue,
                      # how many matches for each dictionary
                      counts_bing = numeric(487), 
                      counts_afinn = numeric(487),
                      counts_nrc = numeric(487),
                      # the length of the interaction
                      length = numeric(487))
        
# Checking how many matches for each row in each dialogue

for (i in 1:length(dialogue$interactions)){
  
  text <- str_remove_all(dialogue$interactions[i], '[:punct:]') # removing punctuation
  
  tibble_text <- tibble(text) # getting a tibble, used for conversion into tokens
  
  tokens <- tibble_text %>% unnest_tokens(word, text) # splitting it into words for tokens
  
  tokens_sr <- tokens %>% anti_join(stop_words) #removal of stop words
  
  tokens_sr$word <- lemmatize_words(tokens_sr$word) # lemmatization
  
  sentiments_bing <- get_sentiments("bing") # loading bing dictionary, pos-neg
  sentiments_afinn <- get_sentiments("afinn") # loading afinn dictionary -5 to 5
  sentiments_nrc <- get_sentiments("nrc") # loading nrc dictionary, range of emotions
  
  # assinging sentiments for each dictionary
  
  tokens_bing <- tokens_sr %>%
    inner_join(sentiments_bing)
  
  tokens_afinn <- tokens %>%
    inner_join(sentiments_afinn)
  
  tokens_nrc <- tokens_sr %>%
    inner_join(sentiments_nrc)
  
  
  # Finding the matches in each row, assigning it to the relevant columns in 'dialogue'
  
  dialogue$counts_bing[i] = length(tokens_bing$sentiment)
  dialogue$counts_afinn[i] = length(tokens_afinn$value)
  dialogue$counts_nrc[i] = length(unique(tokens_nrc$word)) # nrc has many duplicates, so only consider unique words
  dialogue$length[i] = length(tokens_sr$word) # these are the total words that are considered for matches 
  
}

# Finding the coverage for each dictionary

# Values for Table 12

sum(dialogue$counts_nrc)/sum(dialogue$length)
sum(dialogue$counts_afinn)/sum(dialogue$length)
sum(dialogue$counts_bing)/sum(dialogue$length)


#############THE FOLLOWING IS CODE FOR RELATIONSHIP EXTRACTION##################

# Removing punctuation from all dialogue

for (i in 1:length(jeyre_fix$dialogue)){
  jeyre_fix$dialogue[i] = str_remove_all(jeyre_fix$dialogue[i], '[:punct:]')
}

# Doing sentiment analysis now

string_to_sentiment <- function(text){
  
  tibble_text <- tibble(text) # getting a tibble, used for conversion into tokens
  
  tokens <- tibble_text %>% unnest_tokens(word, text) # splitting it into words for tokens
  
  tokens_sr <- tokens %>% anti_join(stop_words) #removal of stop words
  
  tokens_sr$word <- lemmatize_words(tokens_sr$word) # lemmatization
  
  sentiments <- get_sentiments("afinn") # afinn is the chosen dictionary
  
  tokens_sr <- tokens_sr %>% # finalizing the sentiment analysis, assigning scores
    inner_join(sentiments)
  
  return(mean(tokens_sr$value,na.rm=TRUE))
}

# Now that the sentiment calculator function is done, now to populate the sentiment column

# However if it picks up a NaN, this just means that there is no matches in sentiment. On this
# case, simply make the sentiment value 0 in this case

for (i in 1:length(jeyre_fix$sentiment)){
  jeyre_fix$sentiment[i] = string_to_sentiment(jeyre_fix$dialogue[i])
  
  if (is.na(jeyre_fix$sentiment[i])){
    jeyre_fix$sentiment[i] = 0
  }
}

# grouping by speakers, and tracking counts of interactions as chapters progress

jeyre_fix <- jeyre_fix %>% mutate(dummy = c(1)) # adding this in to count interactions

# interactions from Jane

jeyre_relationships <- 
  jeyre_fix %>% 
  group_by(speaker, addressee, chapter) %>%
  summarise(sentiment = mean(sentiment, na.rm = TRUE), # the actual sentiment, omitting NAs
            counts = sum(dummy)) %>% # how many interactions they had 
  filter(speaker == "Jane Eyre") %>%
  filter(addressee != "[]") %>%
  filter(addressee != "Jane Eyre")

# We only consider relationships where it goes through at least 2 chapters, or else it is pointless
# As we are not considering any dynamic changes

# Getting a weighted average of all sentiment from Jane

mean_sentiment_from_jane <- 
  sum(jeyre_relationships$sentiment * jeyre_relationships$counts)/sum(jeyre_relationships$counts)

# creating new column to track addressees that will need to be removed

jeyre_relationships$remove = numeric(length(jeyre_relationships$addressee))

for (i in 1:length(jeyre_relationships$addressee)){
  
  # finding how many times the element i appears in speaker list
  
  count = 0
  
  for (j in 1:length(jeyre_relationships$addressee)){
    if (jeyre_relationships$addressee[i] == jeyre_relationships$addressee[j]){
      count = count + 1
    }
  }
  
  # marking relationships with only one chapter interaction 
  
  if (count < 2){

    jeyre_relationships$remove[i] = 1 
  }
  
}  


# removing less than 2 chapter interactions. Isn't necessary because the aim of 
# the project is tracking dynamic changes in relationships

jeyre_relationships <- jeyre_relationships %>% filter(remove != 1)

# removing the 'remove' column

jeyre_relationships <- jeyre_relationships %>% select(-remove)

# Figure 2
jeyre_relationships %>% ggplot(aes(chapter, sentiment, colour = addressee)) + geom_line(se = FALSE) + 
  labs(title = "Tracking sentiment from Jane thoughout novel", x = 'Chapter', y = 'Sentiment', color = 'Addressee') + 
  lims(x=c(0,40), y = c(-5,5)) +
  theme_bw()

# Figure 4
jeyre_relationships %>% ggplot(aes(chapter, counts, colour = addressee)) + geom_line(se = FALSE) + 
  labs(title = "Tracking interactions from Jane thoughout novel", x = 'Chapter', y = 'Interactions', color = 'Addressee') + 
  lims(x=c(0,40), y = c(0,20)) +
  theme_bw()

# finding the mean of all sentiments from jane

# Left Column of Table 14

mean_sentiment_from_jane <- mean(jeyre_relationships$sentiment)

# interactions to Jane

jeyre_relationships <- 
  jeyre_fix %>% 
  group_by(speaker, addressee, chapter) %>%
  summarise(sentiment = mean(sentiment, na.rm = TRUE), # the actual sentiment, omitting NAs
            counts = sum(dummy)) %>% # how many interactions they had 
  filter(addressee == 'Jane Eyre') %>%
  filter(speaker != "[]") %>%
  filter(speaker != 'Jane Eyre')

# creating new column to track speakers that will need to be removed

jeyre_relationships$remove = numeric(length(jeyre_relationships$speaker))

for (i in 1:length(jeyre_relationships$speaker)){
  
  # finding how many times the element i appears in speaker list
  
  count = 0
  
  for (j in 1:length(jeyre_relationships$speaker)){
    if (jeyre_relationships$speaker[i] == jeyre_relationships$speaker[j]){
      count = count + 1
    }
  }
  
  # marking relationships with only one chapter interaction 
  
  if (count < 2){
    
    jeyre_relationships$remove[i] = 1 
  }
  
}  

# removing less than 2 chapter interactions. Isn't necessary because the aim of 
# the project is tracking dynamic changes in relationships

jeyre_relationships <- jeyre_relationships %>% filter(remove != 1)

# removing the 'remove' column

jeyre_relationships <- jeyre_relationships %>% select(-remove)

# Figure 1
jeyre_relationships %>% ggplot(aes(chapter, sentiment, colour = speaker)) + geom_line(se = FALSE) + 
  labs(title = "Tracking sentiment to Jane thoughout Novel", x = 'Chapter', y = 'Sentiment', color = 'Speaker') + 
  lims(x=c(0,40), y = c(-5,5)) +
  theme_bw()

# Figure 3
jeyre_relationships %>% ggplot(aes(chapter, counts, colour = speaker)) + geom_line(se = FALSE) + 
  labs(title = "Tracking interactions to Jane thoughout Novel", x = 'Chapter', y = 'Interactions', color = 'Speaker') + 
  lims(x=c(0,40), y = c(0,20)) + 
  theme_bw()

# Getting a weighted average of all sentiment from Jane

# Right Column of Table 14
mean_sentiment_from_jane <- 
  sum(jeyre_relationships$sentiment * jeyre_relationships$counts)/sum(jeyre_relationships$counts)

