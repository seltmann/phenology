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

allOccurrenceData <- read.csv("data/California/occurrences.tab", header = TRUE, stringsAsFactors = TRUE, sep = "\t", quote = "\"")
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

colnames(trimOccurrenceData)

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

trimOccurrenceData$cleaned_scientificName <- gsub(".[(].+[)]",'',trimOccurrenceData$cleaned_scientificName)

#trimOccurrenceData$cleaned_scientificName <- as.factor(trimOccurrenceData$cleaned_scientificName)    

colnames(trimOccurrenceData)
unique(trimOccurrenceData$cleaned_scientificName)
class(trimOccurrenceData$cleaned_scientificName)


##############################
#scientific names improve matching
##############################
#not able to find California recoreds for:
#Nomada semisuavis
#Dioxys productus cismontanicus

#Heterosarus californicus not found in data = Pseudopanurgus californicus
trimOccurrenceData<- trimOccurrenceData %>%
  mutate(cleaned_scientificName = if_else(trimOccurrenceData$cleaned_scientificName == "Pseudopanurgus californicus","Heterosarus californicus",trimOccurrenceData$cleaned_scientificName))

#Exomalopsis cerei = Anthophorula cerei
trimOccurrenceData<- trimOccurrenceData %>%
  mutate(cleaned_scientificName = if_else(trimOccurrenceData$cleaned_scientificName == "Anthophorula cerei","Exomalopsis cerei",trimOccurrenceData$cleaned_scientificName))

#Nomada semisuavis

#Lasioglossum miguelense = Evylaeus miguelensis
trimOccurrenceData<- trimOccurrenceData %>%
  mutate(cleaned_scientificName = if_else(trimOccurrenceData$cleaned_scientificName == "Evylaeus miguelensis","Lasioglossum miguelense",trimOccurrenceData$cleaned_scientificName))

#Ashmeadiella cactorum basalis = Ashmeadiella cactorum
trimOccurrenceData<- trimOccurrenceData %>%
  mutate(cleaned_scientificName = if_else(trimOccurrenceData$cleaned_scientificName == "Ashmeadiella cactorum","Ashmeadiella cactorum basalis",trimOccurrenceData$cleaned_scientificName))

#Ashmeadiella chumashae
#Dioxys productus cismontanicus

###############test for using gsub examples#################
string_test <- c('Halictus tripartitus','Lasioglossum (Evylaeus) sp. E', 'Lasioglossum (Dialictus)','Andrena (Derandrena) vandykei')
string_test <- gsub(".[(].+[)]",'',string_test)

##############################################
##########Create New Phenology Table##########
#create data frame table with 0=absent, 1=present, species and months. Start by setting all values to 0

nrow(taxa)
scientificName <- gsub(".[(].+[)]",'',taxa$ScientificName)
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

######Jan#######
Jan_match <- filter(trimOccurrenceData, eventMonth == "1")
phenologyTable$Jan<-Jan_match[match(phenologyTable$scientificName, Jan_match$cleaned_scientificName),91]

######Feb#######
Feb_match <- filter(trimOccurrenceData, eventMonth == "2")
phenologyTable$Feb<-Feb_match[match(phenologyTable$scientificName, Feb_match$cleaned_scientificName),91]

######Mar#######
Mar_match <- filter(trimOccurrenceData, eventMonth == "3")
phenologyTable$Mar<-Mar_match[match(phenologyTable$scientificName, Mar_match$cleaned_scientificName),91]

######Apr#######
Apr_match <- filter(trimOccurrenceData, eventMonth == "4")
phenologyTable$Apr<-Apr_match[match(phenologyTable$scientificName, Apr_match$cleaned_scientificName),91]

######May#######
May_match <- filter(trimOccurrenceData, eventMonth == "5")
phenologyTable$May<-May_match[match(phenologyTable$scientificName, May_match$cleaned_scientificName),91]

######Jun#######
Jun_match <- filter(trimOccurrenceData, eventMonth == "6")
phenologyTable$Jun<-Jun_match[match(phenologyTable$scientificName, Jun_match$cleaned_scientificName),91]

######Jul#######
Jul_match <- filter(trimOccurrenceData, eventMonth == "7")
phenologyTable$Jul<-Jul_match[match(phenologyTable$scientificName, Jul_match$cleaned_scientificName),91]

######Aug#######
Aug_match <- filter(trimOccurrenceData, eventMonth == "8")
phenologyTable$Aug<-Aug_match[match(phenologyTable$scientificName, Aug_match$cleaned_scientificName),91]

######Sep#######
Sep_match <- filter(trimOccurrenceData, eventMonth == "9")
phenologyTable$Sep<-Sep_match[match(phenologyTable$scientificName, Sep_match$cleaned_scientificName),91]

######Oct#######
Oct_match <- filter(trimOccurrenceData, eventMonth == "10")
phenologyTable$Oct<-Oct_match[match(phenologyTable$scientificName, Oct_match$cleaned_scientificName),91]

######Nov#######
Nov_match <- filter(trimOccurrenceData, eventMonth == "11")
phenologyTable$Nov<-Nov_match[match(phenologyTable$scientificName, Nov_match$cleaned_scientificName),91]

######Dec#######
Dec_match <- filter(trimOccurrenceData, eventMonth == "12")
phenologyTable$Dec<-Dec_match[match(phenologyTable$scientificName, Dec_match$cleaned_scientificName),91]

phenologyTable <- replace(phenologyTable, is.na(phenologyTable), 0)

phenologyTable<- phenologyTable2

##############################
#fill in gaps so that all occurrences are contiguous
##############################

phenologyTable<- phenologyTable %>%
  mutate(Jan = if_else(phenologyTable$Jan == "0" & phenologyTable$Dec == "1" & phenologyTable$Feb == "1","1",phenologyTable$Jan))

#test for finding missing values
#filter(phenologyTable,phenologyTable$Jan == "0" & phenologyTable$Dec == "1" & phenologyTable$Feb == "1")

phenologyTable<- phenologyTable %>%
  mutate(Feb = if_else(phenologyTable$Feb == "0" & phenologyTable$Jan == "1" & phenologyTable$Mar == "1","1",phenologyTable$Feb))

phenologyTable<- phenologyTable %>%
  mutate(Mar = if_else(phenologyTable$Mar == "0" & phenologyTable$Feb == "1" & phenologyTable$Apr == "1","1",phenologyTable$Mar))

phenologyTable<- phenologyTable %>%
  mutate(Apr = if_else(phenologyTable$Apr == "0" & phenologyTable$Mar == "1" & phenologyTable$May == "1","1",phenologyTable$Apr))

phenologyTable<- phenologyTable %>%
  mutate(May = if_else(phenologyTable$May == "0" & phenologyTable$Apr == "1" & phenologyTable$Jun == "1","1",phenologyTable$May))

phenologyTable<- phenologyTable %>%
  mutate(Jun = if_else(phenologyTable$Jun == "0" & phenologyTable$May == "1" & phenologyTable$Jul == "1","1",phenologyTable$Jun))

phenologyTable<- phenologyTable %>%
  mutate(Jul = if_else(phenologyTable$Jul == "0" & phenologyTable$Jun == "1" & phenologyTable$Aug == "1","1",phenologyTable$Jul))

phenologyTable<- phenologyTable %>%
  mutate(Aug = if_else(phenologyTable$Aug == "0" & phenologyTable$Jul == "1" & phenologyTable$Sep == "1","1",phenologyTable$Aug))

phenologyTable<- phenologyTable %>%
  mutate(Sep = if_else(phenologyTable$Sep == "0" & phenologyTable$Aug == "1" & phenologyTable$Oct == "1","1",phenologyTable$Sep))

phenologyTable<- phenologyTable %>%
  mutate(Oct = if_else(phenologyTable$Oct == "0" & phenologyTable$Sep == "1" & phenologyTable$Nov == "1","1",phenologyTable$Oct))

phenologyTable<- phenologyTable %>%
  mutate(Nov = if_else(phenologyTable$Nov == "0" & phenologyTable$Oct == "1" & phenologyTable$Dec == "1","1",phenologyTable$Nov))

phenologyTable<- phenologyTable %>%
  mutate(Dec = if_else(phenologyTable$Dec == "0" & phenologyTable$Nov == "1" & phenologyTable$Jan == "1","1",phenologyTable$Dec))

#write table
write_tsv(phenologyTable,"All-California-phenologyTable.tsv")

##############################
#if day of month is earlier, than include month before
##############################


##############################
#look for collectors to create a clean map of those collectors
##############################
collectors<- data.frame(unique(trimOccurrenceData$recordedBy))
write_tsv(collectors,"collectors.tsv")

