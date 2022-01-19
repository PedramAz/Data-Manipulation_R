# Data manipulation pipeline 
Here we have the code to split and clean the .csv files imported from the clinical trial REDCap database. 
Primary exported .csv file contains multiple rows per patient and also contains many empty cells which need to be handeled. 
First two functions in the code will do the following:
  1- Split the dataset into 9 separate dataframes based on "Study arm" column/variable, so we will have one row per patient in each dataframe.
  2- Remove the columns with all-empty cells from all 9 dataframes. 
