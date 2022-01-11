
setwd("C:/Users/pazimzadeh/OneDrive - University of Maryland School of Medicine/Desktop/Temp")
getwd()
# import dataset into R
df <- read.csv("C:/Users/pazimzadeh/OneDrive - University of Maryland School of Medicine/Desktop/Temp/ptdf.csv", fileEncoding="UTF-8-BOM")
df$redcap_event_name <- as.factor(df$redcap_event_name)
summary(df$redcap_event_name)
# Split the dataset into separate dataframes for each study "arm" from REDCap
X <- split(df, df$redcap_event_name)
Y <- lapply(seq_along(X), function(x)as.data.frame(X[[x]])[1:194])
A <- Y[[1]] # follow_up__14_days_arm_1
B <- Y[[]] # follow_up__180_day_arm_1
C <- Y[[3]] # follow_up__90_days_arm_1
D <- Y[[4]] # molecular_testing_arm_1
E <- Y[[5]] # operative_data_arm_1
F <- Y[[6]] # pathology_arm_1
G <- Y[[7]] # patient_enrollment_arm_1
H <- Y[[8]] # pre_operation_arm_1 <- includes "imaging" data 
I <- Y[[9]] # testing_status_arm_1 

# Convert split lists into dataframes
follow_up__14 <- as.data.frame(A)
follow_up__180 <- as.data.frame(B)
follow_up__90 <- as.data.frame(C)
molecular_testing <- as.data.frame(D)
operative_data <- as.data.frame(E)
pathology <- as.data.frame(F)
patient_enroll <- as.data.frame(G)
pre_operation <- as.data.frame(H)
testing_status <- as.data.frame(I)


# Export single dataframes
write.csv(follow_up__14, "followup14.csv")
write.csv(follow_up__180, "followup180.csv")
write.csv(follow_up__90, "followup90.csv")
write.csv(molecular_testing, "moleculartesting.csv")
write.csv(operative_data, "operativedata.csv")
write.csv(pathology, "pathology.csv")
write.csv(patient_enroll, "patientenroll.csv")
write.csv(pre_operation, "preoperation.csv")
write.csv(testing_status, "testingstatus.csv")


# remove the columns with "ALL NA's" from the split dataframes
library(tidyverse)
remove_empty_col <- function (dir) {
  files <- list.files(path = dir, pattern = "*.csv", full.names = TRUE)
  
  for (file in files) {
    
    new_name <- str_replace(file, "\\.csv$", "_cleaned.csv") 
    
    df <- read_csv(file)
    
    df <- df %>%
      select_if(~!(all(is.na(.)) | all(. == "")))

    write.csv(df, paste0(new_name), row.names=FALSE) }
  
}

out <- remove_empty_col("C:/Users/pazimzadeh/OneDrive - University of Maryland School of Medicine/Desktop/Temp")

# remove the index column from the split dataframes
library(tidyverse)
remove_index_col <- function (dir) {
  files <- list.files(path = dir, pattern = "*cleaned.csv", full.names = TRUE)
  
  for (file in files) {
    
    new_name <- str_replace(file, "\\.csv$", "_2.csv") 
    
    df <- read_csv(file)
    
    df <- df %>% select(-1)
    
    write.csv(df, paste0(new_name), row.names=FALSE) }
  
}

out <- remove_index_col("C:/Users/pazimzadeh/OneDrive - University of Maryland School of Medicine/Desktop/Temp")













