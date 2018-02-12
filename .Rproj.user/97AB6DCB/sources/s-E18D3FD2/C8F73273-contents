# Load libraries
library(plyr)
library(dplyr)
library(tidyr)

# Load data and create local data frame
titanic_original<- read.csv("C:/springboard_capstone_project/rprojects/titanic_original.csv")
titanic <- as.data.frame(titanic_original)
View(titanic)
class(titanic_original)
class(titanic)

# Replace the missing values
titanic <- titanic %>%
mutate(embarked = gsub("^$","s",embarked))
titanic %>% select(embarked) %>% unique()

#Calculate the mean of the Age column and use that value to populate the missing values.
titanic$age <- as.integer(titanic$age)
titanic <- titanic %>%
mutate ( age = ifelse(is.na(age), mean(titanic$age, na.rm = TRUE),age))
titanic %>% select(age) %>% unique()

# Life boat .Fill the empty slots with a dummy value e.g. the string 'None' or 'NA'
titanic <- titanic %>%
mutate(boat = gsub("^$", "NA",boat ))
titanic %>% select(boat) %>% unique()

# Cabin ( create new column)
titanic <- titanic %>%
mutate(has_cabin_no = if_else (grepl("^$", cabin), 0, 1) )
titanic %>% select(has_cabin_no) %>% distinct()









