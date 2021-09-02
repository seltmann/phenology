#uses occurrence records to create spreadsheet when bees are active
#Katja C. Seltmann (enicospilus@gmail.com) August 2021 - phenology
#TODO: months for plants and people

# Load libraries
library(tidyverse)
library(janitor)
library(lubridate)

#set directory
setwd("~/Documents/phenology")

##############################################
##########Import Data##########
#load taxon names from checklist and occurrence data
taxa <- read.delim("bees-SCI_2021_08_04.tsv", stringsAsFactors = TRUE, header = TRUE, sep = "\t", quote = "\"")
head(taxa$Family)
colnames(taxa)

allOccurrenceData <- read.csv("Hymenoptera-SCI/occurrences.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",", quote = "\"")
head(allOccurrenceData)
colnames(allOccurrenceData)

##############################################
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

#create a column to return a 1 when is TRUE. Needed later.
trimOccurrenceData <- trimOccurrenceData %>%
  mutate(set_number = "1")

##############################################
#################name matching####################
#check the results
unique(trimOccurrenceData$scientificName)
unique(trimOccurrenceData$genus)

#update scientificName to remove subgenus. Also include synonyms?
trimOccurrenceData <- trimOccurrenceData %>%
  mutate(cleaned_scientificName = scientificName)

#TODO add synonyms for scientific names of bees

trimOccurrenceData$cleaned_scientificName <- gsub("[(].+[)].",'',trimOccurrenceData$cleaned_scientificName)

trimOccurrenceData$cleaned_scientificName <- as.factor(trimOccurrenceData$cleaned_scientificName)    

colnames(trimOccurrenceData)
unique(trimOccurrenceData$cleaned_scientificName)
class(trimOccurrenceData$cleaned_scientificName)

###############test for using gsub examples#################
string_test <- c('Halictus tripartitus','Lasioglossum (Evylaeus) sp. E', 'Lasioglossum (Dialictus)','Andrena (Derandrena) vandykei')
string_test <- gsub("[(].+[)].",'',string_test)



##############################################
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
class(phenologyTable$scientificName)

#make list of names we are looking for
#checklistNames <- phenologyTable$scientificName
#trim full dataset to only be those with names we are interested
#onlyListNames <- filter(trimOccurrenceData, scientificName %in% checklistNames)

######Jan#######
Jan_match <- filter(trimOccurrenceData, eventMonth == "1")
phenologyTable$Jan<-Jan_match[match(phenologyTable$scientificName, Jan_match$cleaned_scientificName),90]

######Feb#######
Feb_match <- filter(trimOccurrenceData, eventMonth == "2")
phenologyTable$Feb<-Feb_match[match(phenologyTable$scientificName, Feb_match$cleaned_scientificName),90]

######Mar#######
Mar_match <- filter(trimOccurrenceData, eventMonth == "3")
phenologyTable$Mar<-Mar_match[match(phenologyTable$scientificName, Mar_match$cleaned_scientificName),90]

######Apr#######
Apr_match <- filter(trimOccurrenceData, eventMonth == "4")
phenologyTable$Apr<-Apr_match[match(phenologyTable$scientificName, Apr_match$cleaned_scientificName),90]

######May#######
May_match <- filter(trimOccurrenceData, eventMonth == "5")
phenologyTable$May<-May_match[match(phenologyTable$scientificName, May_match$cleaned_scientificName),90]

######Jun#######
Jun_match <- filter(trimOccurrenceData, eventMonth == "6")
phenologyTable$Jun<-Jun_match[match(phenologyTable$scientificName, Jun_match$cleaned_scientificName),90]

######Jul#######
Jul_match <- filter(trimOccurrenceData, eventMonth == "7")
phenologyTable$Jul<-Jul_match[match(phenologyTable$scientificName, Jul_match$cleaned_scientificName),90]

######Aug#######
Aug_match <- filter(trimOccurrenceData, eventMonth == "8")
phenologyTable$Aug<-Aug_match[match(phenologyTable$scientificName, Aug_match$cleaned_scientificName),90]

######Sep#######
Sep_match <- filter(trimOccurrenceData, eventMonth == "9")
phenologyTable$Sep<-Sep_match[match(phenologyTable$scientificName, Sep_match$cleaned_scientificName),90]

######Oct#######
Oct_match <- filter(trimOccurrenceData, eventMonth == "10")
phenologyTable$Oct<-Oct_match[match(phenologyTable$scientificName, Oct_match$cleaned_scientificName),90]

######Nov#######
Nov_match <- filter(trimOccurrenceData, eventMonth == "11")
phenologyTable$Nov<-Nov_match[match(phenologyTable$scientificName, Nov_match$cleaned_scientificName),90]

######Dec#######
Dec_match <- filter(trimOccurrenceData, eventMonth == "12")
phenologyTable$Dec<-Dec_match[match(phenologyTable$scientificName, Dec_match$cleaned_scientificName),90]

phenologyTable <- replace(phenologyTable, is.na(phenologyTable), 0)

write_tsv(phenologyTable,"phenologyTable.tsv")


##############################
#look for collectors
##############################
collectors<- data.frame(unique(trimOccurrenceData$recordedBy))
write_tsv(collectors,"collectors.tsv")

