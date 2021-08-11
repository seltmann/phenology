#uses occurrence records to create spreadsheet when bees are active
#Katja C. Seltmann (enicospilus@gmail.com) August 2021 - phenology

# Load libraries
library(tidyverse)
library(janitor)
library(lubridate)

#set directory
setwd("~/Documents/phenology")

##########Import Data##########
#load taxon names from checklist and occurrence data
taxa <- read.delim("bees-SCI_2021_08_04.tsv", stringsAsFactors = TRUE, header = TRUE, sep = "\t", quote = "\"")
head(taxa$Family)
colnames(taxa)

allOccurrenceData <- read.csv("Hymenoptera-SCI/occurrences.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",", quote = "\"")
head(allOccurrenceData)
colnames(allOccurrenceData)

##########Clean Imported Data##########
#trim big dataset to only those families
bee_family <- c("Apidae", "Megachilidae", "Halictidae", "Andrenidae", "Colletidae", "Melittidae", "Stenotritidae")

trimOccurrenceData <- filter(allOccurrenceData, family %in% bee_family)

#check the results
unique(trimOccurrenceData$family)

#check the class of eventDate
class(trimOccurrenceData$eventDate)

#change to class date. Will throw a warning that some fail to parse. This is ok, those are empty strings.
trimOccurrenceData <- trimOccurrenceData %>%
  mutate(eventDate = ymd(eventDate))

#create new column with just dates
trimOccurrenceData <- trimOccurrenceData %>%
  mutate(eventMonth = month(eventDate))

#remove rows with empty month
trimOccurrenceData <- trimOccurrenceData %>% drop_na(eventMonth)

#have a look, values returned as numbers
trimOccurrenceData$eventMonth

##########Create New Phenology Table##########
#create data frame table with 0=absent, 1=present, species and months. Start by setting all values to 0

nrow(taxa)
scientificName <- taxa$ScientificName
Jan <- replicate(142, 0)
Feb <- replicate(142, 0)
Mar <- replicate(142, 0)
Apr <- replicate(142, 0)
May <- replicate(142, 0)
Jun <- replicate(142, 0)
Jul <- replicate(142, 0)
Aug <- replicate(142, 0)
Sep <- replicate(142, 0)
Oct <- replicate(142, 0)
Nov <- replicate(142, 0)
Dec <- replicate(142, 0)

phenologyTable <- data.frame(scientificName, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) 

colnames(phenologyTable)

#update phenologyTable, set months = 1 where eventDate = that month in trimOccurrenceData

colnames(trimOccurrenceData)
#make list of names we are looking for
checklistNames <- phenologyTable$scientificName

#trim full dataset to only be those with names we are interested
onlyListNames <- filter(trimOccurrenceData, scientificName %in% checklistNames)

colnames(onlyListNames)

for(i in onlyListNames$eventMonth){
  ifelse(i == "1",print("TRUE"),FALSE)
}

for(i in onlyListNames){
  ifelse(onlyListNames$eventMonth == "2",print("TRUE"),FALSE)
}



