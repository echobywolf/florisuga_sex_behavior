#What is known already
## 107  rows of data
## 31 unique trials had sex behavior
## 79 unique bouts
## 7 is highest number of bouts in one trial


#are courtships or copulations more frequent?
fsb = read.csv("SexBehavior.csv")
fsb_court <- fsb %>%
  filter(fsb$court.copul == "court")
view(fsb_court)
nrow(fsb_court) #38

fsb_copul <- fsb %>%
  filter(fsb$court.copul == "copul")
view(fsb_copul)
nrow(fsb_copul) #36

##ANS: There are more instances of courtship than instances of copulations (38>36)

#add new columns that show amount of time that has passed since aggresion rather than time of day via mutate()
fsb_elapse <- fsb %>%
  mutate(fsb, elapse_before = fsb$time - fsb$aggression_before.) %>%
  mutate(fsb, elapse_after = fsb$aggression_after.- fsb$time )
    ###error:Error: Column `fsb` is of unsupported class data.frame


data = read.csv("Sexual Behavior Data Sheets.csv")
library(tidyverse)
lm.rf <- data %>%
  filter(data$L == "M", data$R == "F") 
lm.rf #10 all together, 3 w/ occurence

lf.rm <- data %>%
  filter(data$L == "F", data$R == "M") 
lf.rm #22 all together, 7 w/ occurence

#thus 10 F/M / M/F trials w/ occurences

lm.rmlf <- data %>%
  filter(data$L == "M", data$R == "MLF")
lm.rmlf

lmlf.m <- data %>%
  filter(data$L == "MLF", data$R == "M")
lm.rmlf

lf.rmlf <- data %>%
  filter(data$L == "F", data$R == "MLF")
lf.rmlf

lmlf.rf <- data %>%
  filter(data$L == "MLF", data$R == "F")
lmlf.rf








###question i want to look into...frequency of what was chosen in each trial type?