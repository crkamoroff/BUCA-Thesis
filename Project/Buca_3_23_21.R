##--------------------------------------
# BUCA Data check and cleanup script
##--------------------------------------
library(tidyverse)
library(janitor)
library(skimr)
library(httr)
library(readr)
library(lubridate)
library(R2jags)
library(RMark) #just using a single function from RMark, convert.inp()

# Get data----------------------------------------------
toad <- read.csv("/Users/corrinakamoroff/Desktop/BUCA_ Thesis/Project/Working Data/Data_CSV.csv") 
view(toad)

#Date---------------------
toad<-toad %>%
  mutate(Date=as.Date(Date, format = "%m/%d/%y")) %>%
  mutate(Year = format(Date, "%Y"))
view(toad)

#Tidyup the column names----
toad<-clean_names(toad)
view(toad)

names(toad)

#changing columns-----
toad$'life_stage_sex'[which(toad$'life_stage_sex' == "10")] <- "AM"
toad$'life_stage_sex'[which(toad$'life_stage_sex' == "11")] <- "AF"
toad$'life_stage_sex'[which(toad$'life_stage_sex' == "12")] <- "E"
toad$'life_stage_sex'[which(toad$'life_stage_sex' == "4")] <- "M"
toad$'life_stage_sex'[which(toad$'life_stage_sex' == "3")] <- "T"
toad$'life_stage_sex'[which(toad$'life_stage_sex' == "8")] <- "J"
toad$'life_stage_sex'[which(toad$'life_stage_sex' == "13")] <- "EM"
toad$'life_stage_sex'[which(toad$'life_stage_sex' == "9")] <- "AU"
toad$'life_stage_sex'[which(toad$'life_stage_sex' == "16")] <- NA

#SVL_MM and mass_G----
toad$svl_mm[which(toad$svl_mm == "999")] <- NA
toad$mass_g[which(toad$mass_g == "999")] <- NA


#Meadow Names-----
toad$`meadow_code`[which(toad$`meadow_code` == "1")] <- "HM"
toad$`meadow_code`[which(toad$`meadow_code` == "2")] <- "CT"
toad$`meadow_code`[which(toad$`meadow_code` == "3")] <- "ST"
toad$`meadow_code`[which(toad$`meadow_code` == "4")] <- "EX"
toad$`meadow_code`[which(toad$`meadow_code` == "5")] <- "BP"
toad$`meadow_code`[which(toad$`meadow_code` == "6")] <- "CM"
toad$`meadow_code`[which(toad$`meadow_code` == "7")] <- "BB"
toad$`meadow_code`[which(toad$`meadow_code` == "8")] <- "WC"
toad$`meadow_code`[which(toad$`meadow_code` == "9")] <- "MM"
toad$`meadow_code`[which(toad$`meadow_code` == "10")] <- "BT"
toad$`meadow_code`[which(toad$`meadow_code` == "11")] <- "RT"
toad$`meadow_code`[which(toad$`meadow_code` == "12")] <- "SN"
toad$`meadow_code`[which(toad$`meadow_code` == "13")] <- "CS"
toad$`meadow_code`[which(toad$`meadow_code` == "14")] <- "GH"
toad$`meadow_code`[which(toad$`meadow_code` == "15")] <- "LT"
toad$`meadow_code`[which(toad$`meadow_code` == "16")] <- "MT"
toad$`meadow_code`[which(toad$`meadow_code` == "17")] <- "MA"
toad$`meadow_code`[which(toad$`meadow_code` == "18")] <- "RP"
toad$`meadow_code`[which(toad$`meadow_code` == "19")] <- "Cl"
toad$`meadow_code`[which(toad$`meadow_code` == "20")] <- "TP"
toad$`meadow_code`[which(toad$`meadow_code` == "21")] <- "TM"
toad$`meadow_code`[which(toad$`meadow_code` == "22")] <- "HL"
toad$`meadow_code`[which(toad$`meadow_code` == "23")] <- "FP"
toad$`meadow_code`[which(toad$`meadow_code` == "25")] <- "SC"

#mark_info_pit_id---
toad$`mark_info_pit_id`[which(toad$`mark_info_pit_id` == "NONE")] <- NA
toad$`mark_info_pit_id`[which(toad$`mark_info_pit_id` == "BLANK")] <- NA
toad$`mark_info_pit_id`[which(toad$`mark_info_pit_id` == 99.0)] <- NA
toad$`mark_info_pit_id`[which(toad$`mark_info_pit_id` == "9.85E+14")] <- NA
toad$`cap_recap`[which(toad$`cap_recap` == "NONE")] <- NA
toad$`cap_recap`[which(toad$`cap_recap` == "BlANK")] <- NA

#lifestage by year table---
toad %>% with(.,table(date, life_stage_sex))

toad %>% with(.,table(year, life_stage_sex))

#lifestsge by meadow table---
toad %>% with (.,table(meadow_code, life_stage_sex))

# or this is another way to do this
toad %>% count(life_stage_sex, date) %>% spread(life_stage_sex,n)

#Count of unique pits in each year:
toad %>%
  group_by(year) %>%
  summarize(uniquePits = n_distinct(mark_info_pit_id))

#Number of unique pits caputured in each survey:
toad %>%
  group_by(date) %>%
  summarize(uniquePits = n_distinct(mark_info_pit_id)) %>%
  View

 toad %>%
  group_by(year) %>%
  summarize(uniquePits = n_distinct(mark_info_pit_id)) %>%
  view 

## Graphing -------------------------------
#Graph showing size (y) and date (x) of pits captured:

#### GGHightlight--------------------------------
library(gghighlight)

Plot_1<-toad %>% filter(mark_info_pit_id != "") %>%
  ggplot(aes(x=year,y=mass_g,group=mark_info_pit_id, col=life_stage_sex)) +
  geom_point() 

#LifeStage and Sex by time of year (julian day):
toad %>%
  mutate(JDay = format(date,"%j")%>%
           as.numeric()) %>%
  ggplot(aes(x=JDay,y=svl_mm,col=life_stage_sex)) + geom_point(alpha=.6)

### Capture history -------------------------------------------------------
 #Attempt_1
 ##ch=Capture History
toad %>% filter(!is.na(mark_info_pit_id)) %>%
  group_by(mark_info_pit_id, year) %>%
  count(mark_info_pit_id) %>%
  ungroup() %>%
  tidyr::complete(year, mark_info_pit_id,fill=list(n=0)) %>%
  mutate(Year = paste0("X", year)) %>% 
  spread(Year,n)  %>% 
  tidyr::unite(ch,starts_with("X"),sep="") %>% 
  mutate(ch2 = gsub(pattern="[23456]",replacement = 1,x = ch)) %>% 
  count(ch2) %>% View()


## Function to collapse capture histories into one vector (combine columns of events)

###Need to figure out CH first
pasty<-function(x) 
{
  k<-ncol(x) # number of events
  n<-nrow(x) # number of individuals
  out<-array(dim=n)
  for (i in 1:n)
  {
    y<-(x[i,]>0)*1
    out[i]<-paste(y, sep = "", collapse = "")
  }
  return(out)
}

capt.hist <- data.frame(ch = pasty(y[,-1]), # ch = capture history. "-1" means exclude first column, which has id
                        id = y[,1])  %>% view# id column


# Plot CH
ggplot(reals$p, aes(x = sex, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ucl, ymax = lcl), width = 0.1) +
  labs(y = "Detection probability") +
  coord_cartesian(ylim = c(0,1))
