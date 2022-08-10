### All data sets --> Ages 30-34 

################## Set directory + Library ################ 

mypackages <- c("skimr","tidyverse","readxl","Hmisc","MASS","dplyr", "lubridate","stringr","janitor", "ggplot2", "tidyr","magrittr", "reshape2", "gtsummary", "mice", "zoo", "VIM", "naniar","mvoutlier")

for(p in mypackages){
  if(!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p,character.only = TRUE)
  }
}

setwd("/Users/yasmina/Desktop")

################## Load Data ################ 

sweep30= read.table("bcs2000.tab",sep="\t", header=TRUE,fill=TRUE)
sweep34=read.table("bcs_2004_followup.tab",sep="\t", header=TRUE,fill=TRUE)
sweep46= read.table("bcs_age46_main.tab",sep="\t", header=TRUE,fill=TRUE)

################## Sweep Age 30 ################## 
colkeep1=c(1,23,31,43,44,72,3345,3596,3598,3603,3604,3605,3606,3607,3608,3609,3642,3643,3644) 
data30=sweep30[,colkeep1] 
data30 <- data30[!is.na(data30$bcsid),] ## remove na in rows

### Any child in hh age 30
data30 <- data30[!is.na(data30$anychd),] # remove missing parenthood status

data30 = data30 %>%
  mutate(parentst30=case_when(data30$anychd == 1 ~ "Parent",
                              data30$anychd == 2 ~ "Non parent"))

### Physical activity 30
data30 = data30 %>%
  mutate(pa30 = case_when(data30$breathls == 1 ~ 1, # Everyday
                          data30$breathls == 2 ~ 2, # 4-5 days a week
                          data30$breathls == 3 ~ 3, # 2-3 days a week
                          data30$breathls == 4 ~ 4, # once a week
                          data30$breathls == 5 ~ 5, # 2-3 a month
                          data30$breathls == 6 ~ 5, # less often
                          data30$breathls == 7 ~ NA_real_,
                          data30$breathls == 8 ~ NA_real_,
                          data30$breathls == 9 ~ NA_real_)) 

data30 = data30 %>% 
  mutate(pa30=ifelse(data30$exercise==1, data30$breathls, 6))   # create pa variable

### Alcohol consumption 30
# Beer
data30$beer <- replace(data30$beer, data30$beer == 998, NA_real_) #No. of pints of beer drunk in last 7 days
data30$beer <- replace(data30$beer, data30$beer == 999, NA_real_)

# Spirits
data30$spirits<- replace(data30$spirits, data30$spirits == 998, NA_real_)
data30$spirits <- replace(data30$spirits, data30$spirits == 999, NA_real_)

# Wine
data30$wine <- replace(data30$wine, data30$wine == 998, NA_real_) #No. of glasses of wine drunk in last 7 days
data30$wine <- replace(data30$wine, data30$wine == 999, NA_real_)

# Sherry
data30$sherry <- replace(data30$sherry, data30$sherry == 998, NA_real_) #No. of glasses of sherry drunk in last 7 days
data30$sherry <- replace(data30$sherry, data30$sherry == 999, NA_real_)

# Alcopops
data30$pops <- replace(data30$pops, data30$pops == 998, NA_real_) #No. of bottles of alcopops drunk in last 7 days
data30$pops <- replace(data30$pops, data30$pops == 999, NA_real_)

# Other alcoholic drinks
data30$othdrink <- replace(data30$othdrink, data30$othdrink ==8, NA_real_)
data30$othdrink <- replace(data30$othdrink, data30$othdrink == 9, NA_real_)

# Replace missing data for alcohol categories with 0 if they don't drink often
data30 = data30 %>%
  mutate(beer = ifelse((data30$drinks == 5 | data30$drinks == 6 | data30$drinks == 7) & is.na(data30$beer),0,data30$beer)) %>%
  mutate(spirits = ifelse((data30$drinks == 5 | data30$drinks == 6 | data30$drinks == 7) & is.na(data30$spirits),0,data30$spirits)) %>%
  mutate(wine = ifelse((data30$drinks == 5 | data30$drinks == 6 | data30$drinks == 7) & is.na(data30$wine),0,data30$wine)) %>%
  mutate(sherry = ifelse((data30$drinks == 5 | data30$drinks == 6 | data30$drinks == 7) & is.na(data30$sherry),0,data30$sherry)) %>%
  mutate(pops = ifelse((data30$drinks == 5 | data30$drinks == 6 | data30$drinks == 7) & is.na(data30$pops),0,data30$pops)) %>%
  mutate(othdrink = ifelse((data30$drinks == 5 | data30$drinks == 6 | data30$drinks == 7) & is.na(data30$othdrink),0,data30$othdrink))

# Transform to units
data30 = data30 %>% 
  mutate(beer=2.5*beer) %>%
  mutate(spirits=1*spirits) %>%
  mutate(wine=2.1*wine) %>%
  mutate(sherry=1*sherry) %>%
  mutate(pops=1.5*wine) %>%
  mutate(othdrink=ifelse(data30$othdrink==1, 1*othdrink,0)) %>%
  mutate(alc30=beer+spirits+wine+sherry+pops+othdrink)

# Replace frequency with actual numbers for the number of weeks
# Most days = 50; 3–4 times a week = 50; once or twice a week = 50; 
# less often = 12; only on special occasions = 4; never = 0

data30 = data30 %>% 
  mutate(fre30=case_when(
    data30$drinks == 1 ~ 50,#on most days
    data30$drinks == 2 ~ 50,#2-3 days a week
    data30$drinks == 3 ~ 50,#once a week
    data30$drinks == 4 ~ 12,# 2 to three times a month 
    data30$drinks == 5 ~ 4,# les often 
    data30$drinks == 6 ~ 0,# never nowadays 
    data30$drinks == 7 ~ 0, # never had a alco drinks
    data30$drinks == 8 ~ NA_real_,
    data30$drinks == 9 ~ NA_real_)) %>%
  mutate(alcohol30=fre30*alc30)

### Smoking 30
data30 = data30 %>% 
  mutate(smoking30=case_when(
    data30$smoking == 1 ~ 1, #never smoked 
    data30$smoking == 2 ~ 2, #former smoker 
    data30$smoking == 3 ~ 3, ##smoke occasionally 
    data30$smoking == 4 ~ 4, # daily smoker 
    data30$smoking == 9 ~ NA_real_))

### Make categorical variables factors
data30$cmsex <-as.factor(data30$cmsex)
data30$anychd <- as.factor(data30$anychd)
data30$parentst30 <-as.factor(data30$parentst30)

### Remove columns 
data30=data30[,-which(names(data30) %in% c("numadch","ownchild","smoking","nofcigs","exsmoker","drinks","beer", "spirits","wine","sherry","pops","othdrink","exercise","breathls","sweat","alc30","fre30"))]


################## Sweep Age 34 ##############
colkeep2=c(1,19,22,110,114,115,1080,1362,2280,2323,2327,2328,2329,2330,2331,2332,2333,2338,2339,2340,2352) # columns to keep
data34=sweep34[,colkeep2] # keep only needed columns
data3034=merge(data30,data34,by="bcsid")
data3034=data3034[-2036,]

SES_excel <- read_excel("BCS_SES.xlsx") ## SES trajectory
colnames(SES_excel)[colnames(SES_excel)== "BCSID"] <- "bcsid" #change colname
data3034=merge(data3034,SES_excel,by="bcsid",all=TRUE)

### Children
data3034$bd7nchhh <- replace(data3034$bd7nchhh, data3034$bd7nchhh == -6, NA_real_)
data3034$bd7ochhh <- replace(data3034$bd7ochhh, data3034$bd7ochhh == -6, NA_real_)

data3034 = data3034 %>%
  mutate(children34=bd7nchhh+bd7ochhh) # combined biological and non-biological children into one variable
data3034 <- data3034[!is.na(data3034$children34),]

data3034=data3034 %>%
  mutate(sumparent3034= ifelse(data3034$children34 == 0,data3034$children34, 1))

### Marital status
data3034$bd7ms <- replace(data3034$bd7ms,data3034$bd7ms==-7, NA_real_)

### Ethnic
data3034 = data3034 %>% 
  mutate(ethnic = as.factor(ifelse(data3034$ethnic==1 , 1, 2))) 

### Physical activity 34
## Frequency of exercise
data3034 = data3034 %>%
  mutate(freq_exercise34 = case_when(data3034$b7breals == 1 ~ 1, #Everyday
                                     data3034$b7breals == 2 ~ 2,  #4-5 days a week
                                     data3034$b7breals == 3 ~ 3, # 2-3 days a week
                                     data3034$b7breals == 4 ~ 4,  # once a week
                                     data3034$b7breals == 5 ~ 5, # 2-3 a month
                                     data3034$b7breals == 6 ~ 5, #less often
                                     data3034$b7breals ==-7 ~ NA_real_, #other missing
                                     data3034$b7breals ==-8 ~ NA_real_,#don't know
                                     data3034$b7breals ==-9 ~ NA_real_, #refusal 
                                     data3034$b7breals ==-1 ~ NA_real_)) #not applicable

data3034 = data3034 %>% 
  mutate(pa34=ifelse(data3034$b7exerse==1,data3034$b7breals, 6)) # create physical activity variable

### High Blood Pressure
data3034 = data3034 %>% 
  mutate(hbp=ifelse(data3034$downhibp == 1 | data3034$bd7hpb11 ==1, 1, 2)) # create high blood pressure variable
data3034=data3034[-3592,]

### Ethnic Group + Cohabitation status
data3034 = data3034 %>% 
  mutate(ethnic = as.factor(ifelse(data3034$ethnic==1 , 1, 2))) %>%  # recode ethnic variable as british and non-british
  mutate(cohab = as.factor(case_when(data3034$bd7spphh == 1 ~ 1,
                                     data3034$bd7spphh == 0 ~ 2,
                                     data3034$bd7spphh == -6 ~ NA_real_))) # recode cohab variable

### Smoking 34
data3034 = data3034 %>% 
  mutate(smoking34 = as.factor(case_when(data3034$b7smokig == 1 ~ 1, #never smoked
                                         data3034$b7smokig == 2 ~ 2, #former smoker
                                         data3034$b7smokig == 3 ~ 3, # occassional
                                         data3034$b7smokig == 4  ~ 4, #daily
                                         data3034$b7smokig == -9 ~ NA_real_,
                                         data3034$b7smokig == -8 ~ NA_real_,
                                         data3034$b7smokig == -7 ~ NA_real_,
                                         data3034$b7smokig == -1 ~ NA_real_)))  # recode smoking variable 

### Alcohol consumption 34
data3034$b7beer <- replace(data3034$b7beer, data3034$b7beer == -9, NA_real_) #refusal
data3034$b7beer <- replace(data3034$b7beer, data3034$b7beer == -8, NA_real_) #don't know
data3034$b7beer <- replace(data3034$b7beer, data3034$b7beer == -7, NA_real_)# other missing
data3034$b7beer <- replace(data3034$b7beer, data3034$b7beer == -1, NA_real_) #not applicable

data3034$b7spiris <- replace(data3034$b7spiris, data3034$b7spiris == -9, NA_real_)
data3034$b7spiris <- replace(data3034$b7spiris, data3034$b7spiris == -8, NA_real_)
data3034$b7spiris <- replace(data3034$b7spiris, data3034$b7spiris == -7, NA_real_)
data3034$b7spiris <- replace(data3034$b7spiris, data3034$b7spiris == -1, NA_real_)

data3034$b7wine <- replace(data3034$b7wine, data3034$b7wine == -9, NA_real_)
data3034$b7wine <- replace(data3034$b7wine, data3034$b7wine == -8, NA_real_)
data3034$b7wine <- replace(data3034$b7wine, data3034$b7wine == -7, NA_real_)
data3034$b7wine <- replace(data3034$b7wine, data3034$b7wine == -1, NA_real_)

data3034$b7sherry <- replace(data3034$b7sherry, data3034$b7sherry == -9, NA_real_)
data3034$b7sherry <- replace(data3034$b7sherry, data3034$b7sherry == -8, NA_real_)
data3034$b7sherry <- replace(data3034$b7sherry, data3034$b7sherry == -7, NA_real_)
data3034$b7sherry <- replace(data3034$b7sherry, data3034$b7sherry == -1, NA_real_)

data3034$b7pops <- replace(data3034$b7pops, data3034$b7pops == -9, NA_real_)
data3034$b7pops <- replace(data3034$b7pops, data3034$b7pops == -8, NA_real_)
data3034$b7pops <- replace(data3034$b7pops, data3034$b7pops == -7, NA_real_)
data3034$b7pops <- replace(data3034$b7pops, data3034$b7pops == -1, NA_real_)

data3034$b7othdnk <- replace(data3034$b7othdnk, data3034$b7othdnk == -9, NA_real_)
data3034$b7othdnk <- replace(data3034$b7othdnk, data3034$b7othdnk == -8, NA_real_)
data3034$b7othdnk <- replace(data3034$b7othdnk, data3034$b7othdnk == -7, NA_real_)
data3034$b7othdnk <- replace(data3034$b7othdnk, data3034$b7othdnk == -1, NA_real_)

data3034 = data3034 %>%
  mutate(beer34 = ifelse((data3034$b7drinks == 5 | data3034$b7drinks == 6 | data3034$b7drinks == 7) & is.na(data3034$b7beer),0,data3034$b7beer)) %>%
  mutate(spirits34 = ifelse((data3034$b7drinks == 5 | data3034$b7drinks == 6 | data3034$b7drinks == 7) & is.na(data3034$b7spiris),0,data3034$b7spiris)) %>%
  mutate(wine34 = ifelse((data3034$b7drinks == 5 | data3034$b7drinks == 6 | data3034$b7drinks == 7) & is.na(data3034$b7wine),0,data3034$b7wine)) %>%
  mutate(sherry34 = ifelse((data3034$b7drinks == 5 | data3034$b7drinks == 6 | data3034$b7drinks == 7) & is.na(data3034$b7sherry),0,data3034$b7sherry)) %>%
  mutate(pops34 = ifelse((data3034$b7drinks == 5 | data3034$b7drinks == 6 |data3034$b7drinks == 7) & is.na(data3034$b7pops),0,data3034$b7pops)) %>%
  mutate(othdrink34 = ifelse((data3034$b7drinks == 5 | data3034$b7drinks == 6 | data3034$b7drinks == 7) & is.na(data3034$b7othdnk),0,data3034$b7othdnk))

# Recode drinks variable 
data3034 = data3034 %>%
  mutate(beer34=2.5*beer34) %>%
  mutate(spirits34=1*spirits34) %>%
  mutate(wine34=2.1*wine34) %>%
  mutate(sherry34=1*sherry34) %>%
  mutate(pops34=1.5*pops34) %>%
  mutate(othdrink34=ifelse(data3034$othdrink34==1, 1*data3034$othdrink34,0)) %>%
  mutate(alc34=beer34+spirits34+wine34+sherry34+pops34+othdrink34)

## most days = 50; 3–4 times a week = 50; once or twice a week = 50; less often = 12; only on special occasions = 4; never = 0

data3034 = data3034 %>% 
  mutate(fre34=case_when(
    data3034$b7drinks == 1 ~ 50,
    data3034$b7drinks == 2 ~ 50,
    data3034$b7drinks == 3 ~ 50,
    data3034$b7drinks == 4 ~ 12,
    data3034$b7drinks == 5 ~ 4,
    data3034$b7drinks == 6 ~ 0,
    data3034$b7drinks == 7 ~ 0,
    data3034$b7drinks == -9 ~ NA_real_,
    data3034$b7drinks == -8 ~ NA_real_,
    data3034$b7drinks == -7 ~ NA_real_,
    data3034$b7drinks == -1 ~ NA_real_)) %>%
  mutate(alcohol34=fre34*alc34)

## BMI
data3034$bd7bmi <- replace(data3034$bd7bmi,data3034$bd7bmi == -7, NA_real_)

## PA30 and PA34 
data3034 = data3034 %>% 
  mutate(pa30 = case_when(data3034$pa30 == 1 ~ 6,
                          data3034$pa30 == 2 ~ 5,
                          data3034$pa30 == 3 ~ 4,
                          data3034$pa30 == 4 ~ 3,
                          data3034$pa30 == 5 ~ 2,
                          data3034$pa30 == 6 ~ 2,
                          data3034$pa30 == 7 ~ 1)) %>% # recode pa30 variable 
  mutate(pa34 = case_when(data3034$pa34 == 1 ~ 6,
                          data3034$pa34 == 2 ~ 5,
                          data3034$pa34 == 3 ~ 4,
                          data3034$pa34 == 4 ~ 3,
                          data3034$pa34 == 5 ~ 2,
                          data3034$pa34 == 6 ~ 2,
                          data3034$pa34 == 7 ~ 1))  # recode pa34 variable 

# Change in PA, Alcohol and Smoking
data3034 = data3034 %>% 
  mutate(changepa = pa34-pa30) %>%
  mutate(changealc = alcohol34 - alcohol30) %>%
  mutate(changesmoking = as.numeric(smoking34) - as.numeric(smoking30))

## Parenthood
data3034 = data3034 %>%
  mutate(parent3034 = case_when(
    data3034$anychd == 1 ~ 1, # previous parent
    data3034$anychd == 2 & data3034$children34 == 0 ~ 2, # new parent
    data3034$anychd == 2 & data3034$children34 > 0 ~ 3)) # never parent

## Remove columns 
data3034=data3034[,-which(names(data3034) %in% c("anychd","downhibp","parentst30","freq_exercise30","pa30",
                                                 "annualcons30","smoking30","bd7ms","b7numelh","bd7spphh",
                                                 "bd7nchhh","bd7ochhh","b7sc","bd7hq13","bd7hpb11","b7smokig",
                                                 "b7drinks", "b7beer","b7spiris","b7wine","b7sherry","b7pops",
                                                 "b7othdnk","b7exerse","b7breals","b7sweat", "beer34",
                                                 "spirits34","wine34","sherry34","pops34","bd7hq5",
                                                 "othdrink34","sumparent3034","pa34","hbp","bd7bmi",
                                                 "smoking34","edu","alc34","fre34","freq_exercise34",
                                                 "alcohol30","alcohol34","children34"))]
## Change column names
colnames(data3034)=c("bcsid","ethnic","sex","class","cohab","change pa","change alcohol","change smoking","parent status")

## % Missing
gg_miss_var(data3034, show_pct = TRUE)

################## Demographic Table 1 #########

## ETHNICITY 
## Ethnicity by parent status for male
table(data3034$ethnic[data3034$sex=="1"],data3034$`parent status`[data3034$sex=="1"])
## Ethnicity by parent status for female
table(data3034$ethnic[data3034$sex=="2"],data3034$`parent status`[data3034$sex=="2"])

## CLASS 
## Class by parent status for male
table(data3034$class[data3034$sex=="1"],data3034$`parent status`[data3034$sex=="1"])
## Class by parent status for female
table(data3034$class[data3034$sex=="2"],data3034$`parent status`[data3034$sex=="2"])

## COHAB 
## Cohabitation status by parent status for male
table(data3034$cohab[data3034$sex=="1"],data3034$`parent status`[data3034$sex=="1"])
## Cohabitation status by parent status for female
table(data3034$cohab[data3034$sex=="2"],data3034$`parent status`[data3034$sex=="2"])

## CHANGE IN PA 
## Change in physical activity by parent status for male
mean(na.omit(data3034$`change pa`[which(data3034$sex=="1" & data3034$`parent status`=="1")]))
mean(na.omit(data3034$`change pa`[which(data3034$sex=="1" & data3034$`parent status`=="2")]))
mean(na.omit(data3034$`change pa`[which(data3034$sex=="1" & data3034$`parent status`=="3")]))

sd(na.omit(data3034$`change pa`[which(data3034$sex=="1" & data3034$`parent status`=="1")]))
sd(na.omit(data3034$`change pa`[which(data3034$sex=="1" & data3034$`parent status`=="2")]))
sd(na.omit(data3034$`change pa`[which(data3034$sex=="1" & data3034$`parent status`=="3")]))

## Change in physical activity by parent status for female
mean(na.omit(data3034$`change pa`[which(data3034$sex=="2" & data3034$`parent status`=="1")]))
mean(na.omit(data3034$`change pa`[which(data3034$sex=="2" & data3034$`parent status`=="2")]))
mean(na.omit(data3034$`change pa`[which(data3034$sex=="2" & data3034$`parent status`=="3")]))

sd(na.omit(data3034$`change pa`[which(data3034$sex=="2" & data3034$`parent status`=="1")]))
sd(na.omit(data3034$`change pa`[which(data3034$sex=="2" & data3034$`parent status`=="2")]))
sd(na.omit(data3034$`change pa`[which(data3034$sex=="2" & data3034$`parent status`=="3")]))

## CHANGE IN SMOKING 
## Change in smoking by parent status for male
mean(na.omit(data3034$`change smoking`[which(data3034$sex=="1" & data3034$`parent status`=="1")]))
mean(na.omit(data3034$`change smoking`[which(data3034$sex=="1" & data3034$`parent status`=="2")]))
mean(na.omit(data3034$`change smoking`[which(data3034$sex=="1" & data3034$`parent status`=="3")]))

sd(na.omit(data3034$`change smoking`[which(data3034$sex=="1" & data3034$`parent status`=="1")]))
sd(na.omit(data3034$`change smoking`[which(data3034$sex=="1" & data3034$`parent status`=="2")]))
sd(na.omit(data3034$`change smoking`[which(data3034$sex=="1" & data3034$`parent status`=="3")]))

## Change in smoking by parent status for female
mean(na.omit(data3034$`change smoking`[which(data3034$sex=="2" & data3034$`parent status`=="1")]))
mean(na.omit(data3034$`change smoking`[which(data3034$sex=="2" & data3034$`parent status`=="2")]))
mean(na.omit(data3034$`change smoking`[which(data3034$sex=="2" & data3034$`parent status`=="3")]))

sd(na.omit(data3034$`change smoking`[which(data3034$sex=="2" & data3034$`parent status`=="1")]))
sd(na.omit(data3034$`change smoking`[which(data3034$sex=="2" & data3034$`parent status`=="2")]))
sd(na.omit(data3034$`change smoking`[which(data3034$sex=="2" & data3034$`parent status`=="3")]))

## CHANGE IN ALCOHOL 
## Change in alcohol by parent status for male
mean(na.omit(data3034$`change alcohol`[which(data3034$sex=="1" & data3034$`parent status`=="1")]))
mean(na.omit(data3034$`change alcohol`[which(data3034$sex=="1" & data3034$`parent status`=="2")]))
mean(na.omit(data3034$`change alcohol`[which(data3034$sex=="1" & data3034$`parent status`=="3")]))

sd(na.omit(data3034$`change alcohol`[which(data3034$sex=="1" & data3034$`parent status`=="1")]))
sd(na.omit(data3034$`change alcohol`[which(data3034$sex=="1" & data3034$`parent status`=="2")]))
sd(na.omit(data3034$`change alcohol`[which(data3034$sex=="1" & data3034$`parent status`=="3")]))

## Change in alcohol by parent status for female
mean(na.omit(data3034$`change alcohol`[which(data3034$sex=="2" & data3034$`parent status`=="1")]))
mean(na.omit(data3034$`change alcohol`[which(data3034$sex=="2" & data3034$`parent status`=="2")]))
mean(na.omit(data3034$`change alcohol`[which(data3034$sex=="2" & data3034$`parent status`=="3")]))

sd(na.omit(data3034$`change alcohol`[which(data3034$sex=="2" & data3034$`parent status`=="1")]))
sd(na.omit(data3034$`change alcohol`[which(data3034$sex=="2" & data3034$`parent status`=="2")]))
sd(na.omit(data3034$`change alcohol`[which(data3034$sex=="2" & data3034$`parent status`=="3")]))


################## Multivariate Regression ################## 
## Multivariable - Change PA
fit1=lm(`change pa` ~ `parent status`+ethnic+sex+class,data=data3034)
summary(fit1)

# MULTIPLE R squared equals .004 meaning 0.4 % of variation in changes in physical activity can be explained by our model
# the residual standards gives an idea of how far observed changes in pa (y values ) are far from the predicted or 
# fitted lung capacity. gives us an idea of the typical sized residuals or error
# the intercept tells how much the y value is when all x are zeros

#-0.05 is the affect of parenthood on changes in 

## Multivariable - Change smoking
fit2=lm(`change smoking` ~  `parent status`+ethnic+sex+class+cohab,data=data3034)
summary(fit2)

## Multivariable - Change alcohol
fit3=lm(`change alcohol` ~  `parent status`+ethnic+sex+class+cohab,data=data3034)
summary(fit3)

################## Multivariate Regression (Sex Stratification) ################## 
male=subset(data3034,sex == 1)
female=subset(data3034,sex == 2)

## Multivariable - Change PA, Male
fit1_male=lm(`change pa` ~ `parent status`+ethnic+class,data=male)
summary(fit1_male)

## Multivariable - Change PA, Female
fit1_female=lm(`change pa` ~ `parent status`+ethnic+class,data=female)
summary(fit1_female)

## Multivariable - Change smoking, Male
fit2_male=lm(`change smoking` ~ `parent status`+ethnic+class,data=male)
summary(fit2_male)

## Multivariable - Change PA, Female
fit2_female=lm(`change smoking` ~ `parent status`+ethnic+class,data=female)
summary(fit2_female)

## Multivariable - Change alcohol, Male
fit3_male=lm(`change alcohol` ~ `parent status`+ethnic+class,data=male)
summary(fit3_male)

## Multivariable - Change alcohol, Female
fit3_female=lm(`change alcohol` ~ `parent status`+ethnic+class,data=female)
summary(fit3_female)


