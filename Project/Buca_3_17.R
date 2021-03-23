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

 #Attempt_2
 
# Function to collapse capture histories into one vector (combine columns of events)
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


##### 3. Run basic Cormack-Jolly-Seber model on BUCA ------------------------

# Cormack-Jolly-Seber models estimate:
# Apparent survival (Phi), and
# Detection probability (p) for open populations

library(RMark)
# Load example dipper dataset
data(dipper)

# Loak at data
head(dipper)

# Create a basic CJS model (constant survival and detection) with: 
cjs.m1 <- crm(dipper)

?crm

# Examine model and coefficient estimates
cjs.m1

# Re-fit model with precision estimates
cjs.m1 <- cjs.hessian(cjs.m1)
cjs.m1

# The estimates are on a log-linear scale, let's change them back.
# The beta for Phi and p was estimated with a logit link
# The inverse logit will convert beta for survival back to the real survival estimate
# exp(x)/(1+exp(x)) or R function "plogis"
exp(cjs.m1$results$beta$Phi)/(1+exp(cjs.m1$results$beta$Phi))
plogis(cjs.m1$results$beta$Phi)
plogis(cjs.m1$results$beta$p)

# Alternatively, we can use the "predict" function
predict(cjs.m1, 
        newdata = data.frame(sex = c("Female", "Male")),
        se=TRUE)  # In this case there are no groups or covariates, so "new data" not used

# IMPORTANT NOTE: we assumed all time intervals between captures = 1, 
# you can change this with a vector of time intervals
# Often, time intervals are not equal (e.g. due to weather constraints or missing surveys)
cjs.m1.unequaltime <- crm(dipper, time.intervals = c(1,3,1,1,4,5))
predict(cjs.m1.unequaltime)

# Process data
dipper.proc <- process.data(dipper, 
                            group = "sex")

# Make design data (from processed data)
dipper.ddl <- make.design.data(dipper.proc)

# Look at design data
dipper.ddl

# Outine formulas for each parameter
Phi.dot <- list(formula=~1)  # ~1 is always a constant (or single estimate)
Phi.sex <- list(formula=~sex)
p.sex <- list(formula=~sex) # Be careful of case-sensitive names. Use the exact group column that was in data

# Make new model (using design data) with constant survival, but different detection probabilities between sexes
cjs.m2 <- crm(dipper.proc, 
              dipper.ddl,
              model.parameters = list(Phi = Phi.dot, p = p.sex),
              accumulate = FALSE)
cjs.m2
# Is this a better fit than Phi.dot.p.dot (i.e. "model.1")? (hint: look at AIC)

cjs.m3 <- crm(dipper.proc, 
              dipper.ddl,
              model.parameters = list(Phi = Phi.sex, p = p.sex),
              accumulate = FALSE)
cjs.m3

# You almost always will fit more than a few models, so this approach would get lengthy.
# It is more efficient (and tidy) to use a function to automate model creation 
# and put them in a table to rank based on AIC or QAIC
fit.dipper.cjs.models <- function(){
  
  # Apparent survival (Phi)
  Phi.sex.time <- list(formula=~sex*time)  # Just like in other linear models "*" includes main effects and an interaction
  Phi.time <- list(formula=~time) # differs between discrete times
  Phi.sex <- list(formula=~sex) # differs between males and females
  Phi.dot <- list(formula=~1) # constant survival
  
  # Detection probability (p)
  # p.sex.time <- list(formula=~sex*time)
  p.sex <- list(formula=~sex)  # differs between males and females
  p.time <- list(formula=~time)  # one discrete estimate of p per capture event
  p.dot <- list(formula=~1) # constant detection
  
  # Construct all combinations and put into one model table
  cml <- create.model.list(c("Phi","p")) # makes all possibile combinations of those parameter formulas
  results <- crm.wrapper(cml, data = dipper.proc, ddl = dipper.ddl,
                         external = FALSE, accumulate = FALSE, hessian = TRUE)
  return(results)
}

# Run function
dipper.cjs.models <- fit.dipper.cjs.models()

# Display model table
dipper.cjs.models

# Look at estimates of top model (number on left, or using name)
dipper.cjs.models[[1]] # or dipper.cjs.models[["Phi.dot.p.dot"]] or dipper.cjs.models$Phi.dot.p.dot

# Getting "real" estimates using "predict"
# First, create new data to predict differences
newdipper <- data.frame(sex = as.factor(c("Female", "Male")))

reals <- predict(dipper.cjs.models[[2]], # NOTE, i'm deliberately calling a model that isn't the "top-performing
                 # model" in order to demonstate the similar detection probabilities between males and females
                 newdata = newdipper, 
                 se = TRUE)
reals

# Plot
ggplot(reals$p, aes(x = sex, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ucl, ymax = lcl), width = 0.1) +
  labs(y = "Detection probability") +
  coord_cartesian(ylim = c(0,1))
