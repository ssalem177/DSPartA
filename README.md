# DSPartA

This code provides the results of the DSProjectA course. 

As a preliminary task, please download the jeyre.txt file, this is the text file of the Jane Eyre novel. 

The timeline of running the files are shown as follows - 

1. Run data_cleaning_ss.R file
   - This will convert the text file of the novel to a csv file
   - It will save a csv file called 'jeyrewide.csv' to your directory
  
2. Run the dialogue_attribution_ss.pynb file
   - This runs the dialogue attribution on 'jeyrewide.csv'
   - It will then save a new file to your directory, called 'jeyrefix.csv', that has all successfully attributed dialogue
  
3. Run the sentiment_extractor_ss.R file
   - This will perform sentiment analysis on 'jeyrefix.csv'
   - On RStudio the figures from the report will be visible on the right sidebar, click the left and right icons to browse
   - The initial part of the code is focused on the performance of different sentiment dictionaries, with the latter half containing the
     sentiment analysis


There is also the 'speaker_test.ss.R' file, which isn't required to run but contains details on a random test on 100 rows from 'jeyrefix.csv'
