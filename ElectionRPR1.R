library(interactions)
library(sandwich)
library(ggplot2)
library(dslabs)
library(dplyr)
library(tidyverse)
library(stringr)
library(rio)

# section 1: loading data ------------------------------------------------------------
#creates a function that takes a folder as input, and converts all spss files into csv files.
#single use code
RIO_SPSS2CSV <- function(filepath) {
  setwd("C:\\Users\\mndau\\Documents\\R\\ElectionRPData\\") #this is the root dir where SPSS data files/folders are located; .csv files will be stored in the same dir
  files <- list.files(path = filepath, 
                      pattern = '.por', 
                      recursive = TRUE) #recursive option to check all folders inside the root dir
  for (f in files) {
    convert(f, 
            paste0(strsplit(f, 
                            split = '.', 
                            fixed = TRUE)[[1]][1],'.csv'))
  }
}

RIO_SPSS2CSV("C:\\Users\\mndau\\Documents\\R\\ElectionRPData\\")

#loading 12 datasets
filepath <- "C:\\Users\\mndau\\Documents\\R\\ElectionRPData\\"
files <- list.files(path = filepath, 
                    pattern = '.csv', 
                    recursive = FALSE) #recursive option to check all folders inside the root dir
setwd("C:\\Users\\mndau\\Documents\\R\\ElectionRPData\\")
d32 <- read.csv(files[1])
d33 <- read.csv(files[2])
d34 <- read.csv(files[3])
d15 <- read.csv(files[4])
d3 <- read.csv(files[5])
d2 <- read.csv(files[6])
d35 <- read.csv(files[7])
d13 <- read.csv(files[8])
d6 <- read.csv(files[9])
d11 <- read.csv(files[10])
d30 <- read.csv(files[11])
d31 <- read.csv(files[12])


# section 2: selecting potential variables prejoining  --------------------------------
#selecting the desired columns/vars for each datasets prejoining
d2prep <- d2 %>% select(respo, project, date8, 
                        partlean, q13_1, q13_1net, 
                        q910, q909, q909a, 
                        edubreak, colleduc, educnew, 
                        income, income2, q918, 
                        racenet, hisprace, q924, 
                        q924net, abcnum, stcode, 
                        stcode2, weight, pidwgt, 
                        reg4, nreg4, censdiv, 
                        ncensdiv, usr, nusr)  %>% 
  mutate(q910=as.numeric(q910))

d3prep <- d3 %>% select(respo, project, date8,
                        partlean, q19_1, q19_1net, 
                        q910, q909, q909a, 
                        edubreak, colleduc, educnew,
                        income, income2, q918, 
                        racenet, hisprace, q924, 
                        q924net, abcnum, stcode, 
                        stcode2, pidwgt, reg4, 
                        nreg4, censdiv, ncensdiv, 
                        usr, nusr) %>% 
  mutate(q910=as.numeric(q910))

d6prep <- d6 %>% select(respo, date8, partlean, 
                        q8, q8net, q910, 
                        q909, q909a, edubreak, 
                        colleduc, educnew, income, 
                        income2, q918, racenet, 
                        hisprace, q924, q924net, 
                        abcnum, stcode, stcode2, 
                        weight, reg4, nreg4, 
                        ncensdiv, usr, nusr) %>% 
  mutate(q910=as.numeric(q910))

d11prep <- d11 %>% select(respo, project, date8, 
                          partlean, q8, q8net, 
                          q910, q909, q909a, 
                          edubreak, colleduc, educnew,
                          income, income2, q918, 
                          racenet, hisprace, q924, 
                          q924net, abcnum, stcode, 
                          stcode2, weight, reg4, 
                          nreg4,censdiv, ncensdiv, 
                          usr, nusr) %>% 
  mutate(q910=as.numeric(q910))

d13prep <- d13 %>% select(respo, project, date8, 
                          partlean, q2, q2net, 
                          q910, q909, q909a, 
                          edubreak, colleduc, educnew,
                          income, income2, q918, 
                          racenet, hisprace, q924, 
                          q924net, abcnum, stcode, 
                          stcode2, weight, reg4, 
                          nreg4, censdiv, ncensdiv, 
                          usr, nusr) %>% 
  mutate(q910=as.numeric(q910))

d15prep <- d15 %>% select(respo, project, date8, 
                          partlean, q2, q2net, 
                          q910, q909, q909a, 
                          edubreak, colleduc, 
                          educnew,income, income2, 
                          q918, racenet, hisprace, 
                          q924, q924net, abcnum, 
                          stcode, stcode2, weight, 
                          reg4, nreg4, censdiv, 
                          ncensdiv, usr, nusr) %>% 
  mutate(q910=as.numeric(q910))


d35prep <- d35 %>% select(ID, WEEK, WP3, Z7, 
                          Z8, Z9, Z10, Z10A, 
                          Z11, Z11A, SEX, METRO, 
                          REGION, HWEIGHT, PWEIGHT, STATE) 



d34prep <- d34 %>% select(ID, WEEK, AW4, Z7, 
                          Z8, Z9, Z10, Z10A, 
                          Z11, Z11A, SEX, METRO, 
                          REGION, HWEIGHT, WEIGHT, STATE) 

d33prep <- d33 %>% select(ID, WEEK, AW4, Z7, 
                          Z8, Z9, Z10, Z10A, 
                          Z11, Z11A, SEX, METRO, 
                          REGION, HWEIGHT, PWEIGHT, STATE) 

d32prep <- d32 %>% select(ID, WEEK, AW4, Z7, 
                          Z8, Z9, Z10, Z10A, 
                          Z11, Z11A, SEX, METRO, 
                          REGION, HWEIGHT, WEIGHT, STATE) 

d31prep <- d31 %>% select(ID, WEEK, AW4, Z7, 
                          Z8, Z9, Z10, Z10A, 
                          Z11, Z11A, SEX, METRO, 
                          REGION, HWEIGHT, PWEIGHT, STATE) 

d30prep <- d30 %>% select(ID, WEEK, AW4, Z7, 
                          Z8, Z9, Z10, Z10A, 
                          Z11, Z11A, SEX, METRO, 
                          REGION, HWEIGHT, PWEIGHT, STATE) 


# section 3: creating joinlists and merging into 2 datasets ---------------------------------

#joining the prepped datasets together. first doing a batch of 2020, then a 1992 batch, then merging

joinlist2020 <- c("respo", "date8", "partlean",
                  'q910', 'q909', 'q909a', 
                  'edubreak', 'colleduc', 'educnew', 
                  'income', 'income2', 'q918', 
                  'racenet', 'hisprace', 'q924', 
                  'q924net', 'abcnum', 'stcode', 
                  'stcode2', 'reg4', 'nreg4', 
                  'ncensdiv', 'usr', 'nusr')
#manual: project, weight, pidwgt, censdiv
d2020 <- full_join(d2prep, 
                   d3prep, 
                   by=c(joinlist2020, 
                        "project", "censdiv", 
                        "pidwgt", "q13_1"="q19_1", 
                        "q13_1net"="q19_1net")) %>% 
  full_join(d6prep, 
            by=c(joinlist2020, 
                 "weight", "q13_1"="q8", 
                 "q13_1net"="q8net")) %>% 
  full_join(d11prep, 
            by=c(joinlist2020, 
                 "project", "censdiv", "weight", 
                 "q13_1"="q8", "q13_1net"="q8net")) %>% 
  full_join(d13prep, 
            by=c(joinlist2020, 
                 "project", "censdiv", "weight", 
                 "q13_1"="q2", "q13_1net"="q2net")) %>% 
  full_join(d15prep, 
            by=c(joinlist2020, 
                 "project", "censdiv", "weight", 
                 "q13_1"="q2", "q13_1net"="q2net"))

#joinlist2 includes all variables except for pweight/weight
joinlist1992 <- c("WP3"="AW4", "ID", "WEEK", 
                  'Z7', 'Z8', 'Z9', 
                  'Z10','Z10A', 'Z11', 
                  'Z11A', 'SEX', 'METRO', 
                  'REGION', 'HWEIGHT','STATE')

d1992 <- full_join(d35prep, 
                   d34prep, 
                   by=c(joinlist1992, "PWEIGHT"="WEIGHT")) %>% 
  full_join(d33prep, 
            by=c(joinlist1992,"PWEIGHT")) %>% 
  full_join(d32prep, 
            by=c(joinlist1992, "PWEIGHT"="WEIGHT")) %>% 
  full_join(d31prep, 
            by=c(joinlist1992, "PWEIGHT")) %>% 
  full_join(d30prep, 
            by=c(joinlist1992, "PWEIGHT"))

# section 4: recoding vars pre-cross year merge ---------------------------------------------

#recoding and merging 2020 variables to conform to 1992 var cases
#converts 1992 presidential vote into bush, clinton, other, or na, turning other into na later
d1992$WP3 <- recode(d1992$WP3, 
                    "1"="Bush", "2"="Clinton", 
                    "3"="Other/NA", "0"="Other/NA",
                    "7"="Other/NA","8"="Other/NA",
                    "9"="Other/NA",.default="Other/NA")


d2020$q13_1net <- recode(d2020$q13_1net, 
                         "Biden"="Biden", "Biden and Harris"="Biden", 
                         "Trump"="Trump", "Trump and Pence"="Trump",
                         "DK/No Opinion"="Other/NA", "Neither"="Other/NA",
                         "Other Candidate"="Other/NA", "Would not vote"="Other/NA")

d1992<- d1992 %>% mutate(racenet = case_when(
  (d1992$Z10==1) ~ "Hispanic",
  (d1992$Z10!=1 & d1992$Z11==1) ~ "white",
  (d1992$Z10!=1 & d1992$Z11==2) ~ "black",
  (d1992$Z10!=1 & d1992$Z11==5) ~ "OtherNA",
  (d1992$Z10!=1 & d1992$Z11==9) ~ "OtherNA"))

d1992$Z11A <- recode(d1992$Z11A, 
                     '1'="a Republican", '2'="a Democrat", 
                     '3'="an independent", '0'="Other/NA",
                     '8'="Other/NA",'9'="Other/NA")

d1992$SEX <- recode(d1992$SEX, 
                    "1"="Male", "2"="Female")

d1992$REGION <- recode(d1992$REGION, 
                       "1"="Northeast", "2"="Midwest",
                       "3"="South","4"="West")

d1992$Z9 <- recode(d1992$Z9, 
                   "1"="Under 10 thousand dollars",
                   "2"="10 to under 15 thousand dollars",
                   "3"="15 to under 20 thousand dollars",
                   "4"="20 to under 25 thousand dollars",
                   "5"="25 to under 30 thousand dollars",
                   "6"="30 to under 40 thousand dollars",
                   "7"="40 to under 50 thousand dollars",
                   "8"="50 to under 75 thousand dollars",
                   "9"="75 thousand or more",
                   "98"="Other/NA","99"="Other/NA")


d1992$METRO <- recode(d1992$METRO, "1"="M", "2"="NM")

d1992$Z8 <- recode(d1992$Z8, 
                   "1"="Some high school", "2"="Graduated high school",
                   "3"="Some college", "4"="Graduated College",
                   "5"="Post graduate", "6"="Technical school/other",
                   "9"="Other/NA")

d2020$nusr <- recode(d2020$nusr, 
                     "R"="NM", "S"="M",
                     "U"="M")


#also adding a variable to describe origin year of each dataset
d1992 <- d1992 %>% mutate(year = 1992)
d2020 <- d2020 %>% mutate(year = 2020)

# section 5: merging 1992 and 2020 data, and dropping unused variables -------------------------------------


joinlist3 <- c("q13_1net"="WP3", "q910"="Z7", 
               "income"="Z9", "racenet",
               "partlean"="Z11A", "q924net"="SEX",
               "nusr"="METRO", "nreg4"="REGION", 
               "q909"="Z8", "weight"="PWEIGHT", "year")

#dropping variables im not using
d2020 <- d2020 %>% select(!(respo | project | date8 | 
                              q13_1 | q924 | income2 | 
                              reg4 | censdiv | usr | 
                              q918 | hisprace | q909a | 
                              edubreak | colleduc | educnew | 
                              abcnum | stcode | stcode2 | 
                              pidwgt | ncensdiv))

d1992<-d1992 %>% select(!(ID | WEEK | STATE | 
                            Z10 | Z10A | Z11 | 
                            HWEIGHT))

#merging 1992 and 2020 datasets
fulldata <- full_join(d2020, d1992, joinlist3)


# section 6: renaming columns for readability ----------------------------------------

colnames(fulldata)[1] <- "partyID"
colnames(fulldata)[2] <- "presVote"
colnames(fulldata)[3] <- "age"
colnames(fulldata)[4] <- "education"
#column 5 is already named income
colnames(fulldata)[6] <- "race"
colnames(fulldata)[7] <- "sex"
#column 8 is population weighting
colnames(fulldata)[9] <- "region"
colnames(fulldata)[10] <- "metropolitan"
#column 11 is year

# section 7: recoding certain variables to fix duplicates postmerge ------------------

fulldata$race <- recode(fulldata$race, 
                        "Asian"="asian", 
                        "Hispanic"="hispanic",  
                        "other"=NA_character_, 
                        "OtherNA"=NA_character_, 
                        "DK/No opinion"=NA_character_, 
                        " "=NA_character_)

fulldata$presVote <- recode(fulldata$presVote, 
                            " "=NA_character_, 
                            "Other/NA"=NA_character_)

fulldata$education <- recode(fulldata$education, 
                             "(VOL) DK/No opinion"=NA_character_, 
                             "(VOL) NA/Refused"=NA_character_, 
                             "Some college (ASK IF TECHNICAL SCHOOL; IF YES, PUNCH CODE 3, FOR HIGH SCHOOL)"="Some college",
                             "Other/NA"=NA_character_, 
                             "8th grade or less"="8th grade or less", 
                             "Graduated College"="Graduated College",
                             "Graduated high school"="Graduated high school",
                             "Post graduate"="Post graduate",
                             "Some college"="Some college",
                             "Some high school"="Some high school",
                             "Technical school/other"="Technical school/other",
                             .default=NA_character_)

fulldata <- fulldata %>% mutate(metropolitan = case_when(
  metropolitan=="M" ~ "M",
  metropolitan=="NM" ~ "NM",
  TRUE ~ NA_character_
))
fulldata$race <- relevel(factor(fulldata$race), ref="white") #making it so that white, nonmetropolitan is the intercept case
fulldata$metropolitan <- relevel(factor(fulldata$metropolitan), ref="NM")
 

# section 8: logit  -------------------------------------------------------------------

md1992 <- fulldata %>% 
  filter(year=="1992") %>% 
  na.omit(cols=c("presVote","race","metropolitan"))

md2020 <- fulldata %>% 
  filter(year=="2020") %>% 
  na.omit(cols=c("presVote","race","metropolitan"))


#recoding presvote for 1992 so clinton/dem=success(1), and bush/repub=failure(0)

md1992$presVote <- recode(md1992$presVote, 
                          "Clinton"=1, "Bush"=0)
#recoding presvote for 1992 so biden/dem=success(1), and trump/repub=failure(0)

md2020$presVote <- recode(md2020$presVote, 
                          "Biden"=1, "Trump"=0)
#presvote is my response var, with metro and race as my explanatory vars

m1992prm <- glm(presVote ~ factor(race) + factor(metropolitan), 
                family = binomial(link = 'logit'), 
                data = md1992)

m2020prm <- glm(presVote ~ factor(race) + factor(metropolitan), 
                family = binomial(link = 'logit'), 
                data = md2020)

summary(m1992prm)
summary(m2020prm)
exp(coef(m1992prm))[-1] #gives odds ratios for 1992
exp(coef(m2020prm))[-1] #odds ratios 2020

#confirms that for both 1992 and 2020, biden and clinton both recieved the majority of the black vote in my dataset, as was in reality
table(md1992$presVote, md1992$race)
table(md2020$presVote, md2020$race)
table(md1992$presVote, md1992$metro)
table(md2020$presVote, md2020$metro)
