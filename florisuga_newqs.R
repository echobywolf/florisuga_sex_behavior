#What is known already
## 107  rows of data
## 31 unique trials had sex behavior
## 79 unique bouts
## 7 is highest number of bouts in one trial
## 74 courts & coups


#are courtships or copulations more frequent?
fsb <- read_csv("SexBehavior.csv")
fsb_court <- fsb %>%
  filter(court.copul == "court")
view(fsb_court)
nrow(fsb_court) #39



fsb_copul <- fsb %>%
  filter(court.copul == "copul")
view(fsb_copul)
nrow(fsb_copul) #64

nrow(fsb)
view(fsb)

##ANS: There are more instances of courtship than instances of copulations (64>39)

#add new columns that show amount of time that has passed since aggresion rather than time of day via mutate()
fsb_elapse <- fsb %>%
  mutate(fsb, elapse_before = fsb$time - fsb$aggression_before) %>%
  mutate(fsb, elapse_after = fsb$aggression_after - fsb$time )
    ###error:Error: Column `fsb` is of unsupported class data.frame

fsb


data = read.csv("Sexual Behavior Data Sheets.csv")
library(tidyverse)

lm.rf <- data %>%
  filter(data$L == "M", data$R == "F") 
lm.rf #10 all together, 3 w/ occurence

lf.rm <- data %>%
  filter(data$L == "F", data$R == "M") 
lf.rm #22 all together, 7 w/ occurence

m.f <-data %>% 
  filter(trial_type == "M/F")
m.f

#thus 10 F/M / M/F trials w/ occurences
#22 no observations (22/32) 0.6875
#0.3125 observations

lm.rmlf <- data %>%
  filter(data$L == "M", data$R == "MLF")
lm.rmlf #16 all together, 2 w/occurence

lmlf.m <- data %>%
  filter(data$L == "MLF", data$R == "M")
lmlf.m #17 all together, 5 w/occurence

m.mlf <- data %>%
  filter(trial_type == "M/MLF")
m.mlf

#thus 7 M/MLF / MLF/M trials w/ occurences
#26 no observations (26/33) 0.787878
#0.212122 observations

lf.rmlf <- data %>%
  filter(data$L == "F", data$R == "MLF")
lf.rmlf  #16 all together, 7 w/ occurences

lmlf.rf <- data %>%
  filter(data$L == "MLF", data$R == "F")
lmlf.rf #19 all together, 7 w/occurence

f.mlf <- data %>% 
  filter(trial_type == "F/MLF")
f.mlf

#thus 14 F/MLF / MLF/F w/occurences
#21 no observations (21/35) 0.6
#0.4 observations


##I predict that there would less observations in the MLF/M trials vs. the F/MLF and F/M trials
## test: one-way ANOVA
## H0: no difference among the occurance of court/copul amongst the different trial types
## HA: at least one mean differs significantly

#fit <- aov(y ~ A, data=mydataframe)

