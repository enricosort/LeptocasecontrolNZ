#Leptospirosis Case Control Study
#Enrico Seyssel Ortolani
#When using the code, I suggest to toggle between folded and unfolded versions by using Alt+o and Shift+Alt+o.
#The sections are divided in Levels according to the indentation of the pound signs
#18th September, 2021
#############PACKAGES#AND#REFERENCING####################
#To make the code run faster, I have put all the install package functions inactive. Please remove the pound sings if you want to install the packages
# install.packages("purrr")
# install.packages("scales")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("sqldf")
# install.packages("RODBC)
# install.packages("forcats")
# install.packages("readxl")
# install.packages("gmodels")
# install.packages("catspec")
# install.packages("descr")
# install.packages("tidyr")
# install.packages(("epiR"))
# install.packages("rwunderground")
# install.packages("forestplot")
# install.packages("data.table")
# install.packages ("bannerCommenter")
# install.packages("tibble")
# install.packages("rstudioapi")
# install.packages("lmtest")
# install.packages("MASS")
# install.packages("plyr")
# install.packages(("missMethods"))
# install.packages("lubridate")
# install.packages("gridExtra")

library(forcats)
library(RODBC)
library(sqldf)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(gmodels)
library(catspec)
library(descr)
library(tidyr)
library(scales)
library(epiR)
library(rwunderground)
library(forestplot)
library(readxl)
library(data.table)
library(bannerCommenter)
library(purrr)
library(tibble)
library(rstudioapi)
library(lmtest)
library(MASS)
library(missMethods)
library(lubridate)
library(gridExtra)

#citing the packages used:
#first I copied a code to create a .bib file so that can be exported to referencing softwares:
usedpackages<-c("RODBC","forcats","tidyverse",  "plyr",  "dplyr",  "ggplot2",  "gmodels",  "catspec",  "descr",  "tidyr",  "scales",  "epiR",  "rwunderground",  "forestplot",  "readxl",  "data.table",  "purrr",  "tibble",  "rstudioapi",  "lmtest",  "MASS",  "lubridate","gridExtra")

citeRpacks <- function(pkg_list, filename, RStudio = FALSE) {
  
  #ht to https://stackoverflow.com/questions/2470248/write-lines-of-text-to-a-file-in-r for sink()
  for (i in 1:length(pkg_list)) {
    sink(file = paste(filename, ".bib", sep = ""), append = T)
    writeLines(toBibtex(citation(package = pkg_list[i])))
    sink()
  }
  
  if(RStudio) {
    
    c <- RStudio.Version()$citation
    
    sink(file = paste(filename, ".bib", sep = ""), append = T)
    writeLines(paste("@Manual{,",
                     "\n   title = {", c$title, "},",
                     "\n   author = {{", c$author, "}},",
                     "\n   organization = {", c$organization, "},",
                     "\n   address = {", c$address, "},",
                     "\n   year = {", c$year, "},",
                     "\n   note = {", RStudio.Version()$version, "}",
                     "\n   url = {", c$url, "},",
                     "\n }",
                     sep = ""))
    sink()
    
  }
  
}
#the citation in text: 
for (i in 1:length(usedpackages)) { print(citation (package = usedpackages[i]))}
#this line creates the .bib file
citeRpacks(pkg_list = usedpackages,filename = "leptothesisreference", RStudio = T)

#####Beginning of the code -  This setion has been written by Shahista Nisa#####
myconn = odbcConnect("mepilabHRCLepto")

## Identify tables in database
sqlTables(myconn)

#If you are not browsing the SQL database, but the csv file, the code is the following (remove the pound signs) : 
# lepto1_mod<-read.csv("lepto1.csv")
# all_participants<-lepto1_mod
# all_participants[all_participants=="NA"]<-NA
# #put as.factor in all the ctabs factors
# all_participants<-all_participants[,-1]
# all_participants$Type<-as.factor((all_participants$Type))
# dat<-all_participants
head(dat[1:10])

## Fetch relevant dataframe from database
### Cases and Control questionnaire
all_participants = sqlFetch(myconn,"CaseQuestionnaire")

#remove the rows that are not controls or cases
all_participants<-all_participants[!is.na(all_participants$Type),]

##Substrings Of A Character Vector i.e. date

all_participants$Age = as.numeric(substr(all_participants$InterviewDate, 1,4)) - as.numeric(substr(all_participants$DOB, 1,4))


##Sort gender

all_participants$Sex = recode(all_participants$Gender,
                              
                              "M" = "Male",
                              
                              "Male" = "Male",
                              
                              "F" = "Female",
                              
                              "Female" = "Female")


##Subset cases and controls

cases = filter(all_participants, Type == "Case")

controls = filter(all_participants, Type == "Control")

##Filter sex

sex = all_participants %>%
  group_by(Type, Sex) %>% 
  dplyr::  summarise(cases = n())

sex
sex<-as.data.frame(sex)
#Long table
sex
#wide table
spread(sex,Type,cases)


##Age

total_age = all_participants %>%
  
  group_by(Type) %>%
  
  summarise(median=median(as.numeric(Age),na.rm=T),
            
            Q25=quantile(as.numeric(Age),1/4,na.rm=T),
            
            Q75=quantile(as.numeric(Age),3/4,na.rm=T),
            
            count=length(Age))


as.data.frame(total_age)

##Gender/Age plot

ggplot(all_participants) +
  
  geom_histogram(aes(x=as.numeric(Age), fill = Sex)) +
  
  facet_wrap(~Type) +
  
  theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) +
  
  labs( x="Age", y="Counts") +
  
  theme(panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(),
        
        axis.line = element_line(colour = "black"),
        
        panel.background = element_blank()) +
  
  theme(text = element_text(size=15))


ggplot(all_participants,aes(x=Type,y=as.numeric(Age), fill = Sex)) +
  
  geom_boxplot(varwidth=TRUE) +
  
  #scale_fill_brewer(palette="Paired") +
  
  labs( x="", y="Age (years)", fill = "Sex") +
  
  theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) +
  
  theme(axis.title=element_text(size=20)) +
  
  theme(text = element_text(size=20)) +
  
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_fill_brewer(palette="Paired")


##Dates between getting sick and seeking help

cases$days_diff = as.Date(substr(cases$DateFirstSoughtHelp, 1,10)) - as.Date(substr(cases$DateFirstLepto, 1,10))

ggplot(cases, aes(x = Age, y = as.numeric(days_diff))) +
  
  geom_point(aes(colour = factor(Sex)), size = 2) +  
  
  labs( x="Age", y="Number of days sick before seeking help", colour = "Sex") +
  
  theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


table(cases$HospitalisedDays)

summary(cases$HospitalisedDays)

table(cases$ICUAdmission)

#Adjustment for the 
#creating short name for using the table
dat<-all_participants
dat$Type<-factor(dat$Type)

#We also create a dichotomous variable for Cases and Controls. Cases will be 1 and controls 0. 
dat$dichotomoustype<-factor(ifelse(dat$Type=="Case", 1,0))

#for my Master's dissertation, I am removing the interviews after 30th June 2021. In this code I have hidden this line with pound sing, but if necessary to check the data I used for the dissertation, execute this line: 
#dat<-dat[as.Date(dat$InterviewDate)<as.Date("2021-07-01"),]

## Calculating the power of the study##################
#calculating the power of the study with varying frequency of exposures. From 5% to 95% increasing 5% for each line.
for (i in (1:19)/20) {
  samplesize<-epi.sscc(OR = 3.0, p0 = i, n = nrow(dat), power = NA, 
                       r = 2.23, rho.cc = 0.0, design = 1, sided.test = 2, conf.level = 0.95, 
                       method = "matched", fleiss = FALSE)
  print(samplesize$power)
}

powercomparison<-as.data.frame(matrix(nrow = 19, ncol=4))

#this time varying ORs (0.25,0.5, 0.75, 2,3 and 4) and frequency of exposures.
for (i in (1:19)/20) { for (j in c(0.25,0.5, 0.75,2,3,4))  {
  samplesize<-epi.sscc(OR = j, p0 = i, n = nrow(dat), power = NA, 
                       r = 2.23, rho.cc = 0.0, design = 1, sided.test = 2, conf.level = 0.95, 
                       method = "matched", fleiss = FALSE)
  powercomparison[i*20,ifelse(j<1,j*4,j+2)]<-samplesize$power
}}

powercomparison


########################TIME#ANALYSIS####
#Columns using Dates:
head(dat[grep("Date",names(dat))])

#creating a vector with Date column numbers
datescases<-grep("Date",names(dat))[c(1:3,5)]
str(dat[datescases])

# the dates are not recognized as dates by R. We will have to transform the type of variable:
for (i in 1:4) { dat[[datescases[i]]]<-as.Date(substr(x =dat[,datescases[i]],start = 1,stop = 10))}
str(dat[datescases])
dat[datescases]
summary(dat$DateFirstLepto)

#create the variable with number of days between being sick and being interviewed.
dat$PeriodSicktoInterview<-dat$InterviewDate-dat$DateFirstLepto
dat$PeriodSicktoInterview<-as.numeric(dat$PeriodSicktoInterview)
str(dat$PeriodSicktoInterview)

#some descriptive statistics and plots for this variable
boxplot(dat$PeriodSicktoInterview)
summary(dat$PeriodSicktoInterview)

#The ID from the participants was given in growing order as cases were enrolled. Checking if any trend exists on the period between sick and interview and the QIDs. 
plot(dat$QID, dat$PeriodSicktoInterview)

summary(lm(dat$PeriodSicktoInterview~ dat$QID)) #coefficient is positive but not signifficant. Maybe cases are being interviewed on longer periods after getting sick. The delay due to COVID could be an explanation. 

# Instead of using QID, Interview Date could be more appropriate.
#calculate correlation between illness-interview interval and interview date
casesinterviewdate<-dat%>% filter(Type=="Case")%>% dplyr:: select(InterviewDate)
casesPeriodSicktoInterview <-dat%>% filter(Type=="Case")%>% dplyr:: select (PeriodSicktoInterview)
#Is there correlation between interview date and PeriodSicktoInterview?
cor(as.numeric(casesPeriodSicktoInterview$PeriodSicktoInterview),as.numeric(casesinterviewdate$InterviewDate), use = "complete.obs")
#looks weak

plot(dat$InterviewDate, dat$PeriodSicktoInterview)
abline(lm(dat$PeriodSicktoInterview~ dat$InterviewDate))
summary(lm(dat$PeriodSicktoInterview~ dat$InterviewDate)) # coefficient is also positive, p-value a little smaller, however still not signifficant. 

summary(dat$InterviewDate)

#checking for the date in which cases became sick
dat$DateFirstLepto
#some observations were made. Viewing all the observations from the cases regarding when they first became ill
dat[!is.na(dat$DateFirstLeptoDetails),c(which(names(dat)=="DateFirstLepto"),which(names(dat)=="DateFirstLepto")+1)]
# Isolating only the cases which no date was put 
dat[!is.na(dat$DateFirstLeptoDetails)&is.na(dat$DateFirstLepto),c(which(names(dat)=="DateFirstLepto"),which(names(dat)=="DateFirstLepto")+1)]
dat$DateFirstLeptoDetails[is.na(dat$DateFirstLepto)&dat$Type=="Case"]

####CORRECTIONS
#So that these values do not become empty, I will transform the uncertainty into estimated values.
# For example: Early September. I will consider it beginning of September.7th September
dat$DateFirstLeptoDetails [dat$DateFirstLeptoDetails=="Early September"&!is.na(dat$DateFirstLeptoDetails)]
dat[dat$DateFirstLeptoDetails=="Early September"&!is.na(dat$DateFirstLeptoDetails), match(c("InterviewDate","DateFirstLepto", "DateFirstLeptoDetails"), names(dat))]
dat$DateFirstLepto[dat$DateFirstLeptoDetails=="Early September"&!is.na(dat$DateFirstLeptoDetails)]<-as.Date("2019-09-07")

#another one - September / October. - I will put the transition between September and October - 30th September.
dat[dat$DateFirstLeptoDetails=="September/October 2020"&!is.na(dat$DateFirstLeptoDetails), match(c("InterviewDate","DateFirstLepto", "DateFirstLeptoDetails"), names(dat))]
dat$DateFirstLepto[dat$DateFirstLeptoDetails=="September/October 2020"&!is.na(dat$DateFirstLeptoDetails)]<-as.Date("2020-09-30")


#now another correction: these dates have been incorrectly inserted as 2021. They were actually 2020. They will be corrected
dat$InvitationDate[dat$InvitationDate=="2021-11-30"&!is.na(dat$InvitationDate)&dat$InterviewDate=="2021-01-21"]<-"2020-11-30"
dat$InvitationDate[dat$InvitationDate=="2021-10-28"&!is.na(dat$InvitationDate)&dat$InterviewDate=="2021-02-15"]<-"2020-10-28"

#numer of weeks between the first and the last interview 
as.numeric((max(dat$InterviewDate) - min(dat$InterviewDate)))/7

#histograms for interview dates with fortnight breaks
hist(dat$InterviewDate[dat$Type=="Control"],as.numeric((max(dat$InterviewDate) - min(dat$InterviewDate)))/14)
hist(dat$InterviewDate[dat$Type=="Case"],48)
sort(dat$InterviewDate[dat$Type=="Control"])

#Creating a variable called interview or sick. This will be further used to compare cases and controls throughout the code. It will be very important because the period of 30 days before this variable is when the exposures happened.
dat$IntervieworSick<-as.Date.numeric(ifelse(dat$Type=="Case",dat$DateFirstLepto,ifelse(dat$Type=="Control",dat$InterviewDate,NA)))

#Comparing Interview Dates of cases and Controls

#Graph used for the dissertation (limited dates)
time1<-ggplot(data = dat, aes(x=InterviewDate, fill=Type))+geom_histogram(stat="bin", bins = 25, breaks=as.Date(ymd(20190701)%m+% months(0:24)))+ labs(x= "Interview date", y= "Count")+scale_x_date(breaks=c(ymd('2019-07-01') %m+% months(which(0:24%%3==0)-1)), date_labels = "%m-%Y", limits =c(ymd("2019-06-30"),ymd("2021-06-30")))+ labs(x= "Cases and controls interview date (month-year)", y= "Count", fill="Group")
time1

#General graph not limiting dates
ggplot(data = dat, aes(x=InterviewDate, fill=Type))+geom_histogram(stat="bin", bins = 22)+ labs(x= "Interview date", y= "Count")+scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y")

#Comparing Interview Dates of Controls with date person was first ill is more suitable, because the interview asks cases about exposures 30 days before the onset of symptoms: 

#GGplot not limiting dates 
ggplot(data = dat, aes(x=IntervieworSick, fill=Type))+geom_histogram(stat="bin", bins = 22)+scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y")+labs(x= "Cases onset of symptoms date and controls interview date", y= "Count", fill="Group")

#GGplot limiting dates for the dissertation
time2<-ggplot(data = dat, aes(x=IntervieworSick, fill=Type))+geom_histogram(stat="bin", bins = 25, breaks=as.Date(ymd(20190701)%m+% months(0:24)))+ scale_x_date(breaks=c(ymd('2019-07-01') %m+% months(which(0:24%%3==0)-1)), date_labels = "%m-%Y", limits =c(ymd("2019-06-01"),ymd("2021-07-01")))+ labs(x= "Cases onset of symptoms date and controls interview date (month-year)", y= "Count", fill="Group")

#Now what I will do is to create a variable with the month and day of the year, but removed the information for year. That is because we can draw plots to understand seasonality trends. The 2nd line is to use 1 to 365 as day of the year ( a numeric value)
dat$IntervieworSickMonthDay<-format(dat$IntervieworSick, format="%m-%d")
dat$IntervieworSickDayY<-yday(dat$IntervieworSick)


time3<-ggplot(data = dat, aes(x=IntervieworSickDayY, fill=Type))+stat_bin(bins = 12,breaks=c(0,match(as.Date(ymd(20210201)%m+% months(0:11)),as.Date(ymd(20210101):ymd(20220101)))) )+ labs(x= "Cases onset of symptoms date and controls interview date - Month of the year", y= "Count", fill="Group", labels= c(month.abb[1:12]) ) + scale_x_continuous(breaks =c(0,match(as.Date(ymd(20210201)%m+% months(0:10)),as.Date(ymd(20210101):ymd(20211231)))), labels = c(month.abb[1:12]))


grid.arrange(time1,time2,time3)

#Above was one idea of graphs. The other idea would be to separate cases and controls the following lines will be for plots ending .1 

#faceting the plots
timeforplot<-dat[match(c("QID","Type","InterviewDate","DateFirstLepto"),names(dat))]
narrowtimeforplot<-gather(timeforplot,"Variable","Date",3:4)
narrowtimeforplot$VarType<-ifelse(narrowtimeforplot$Type=="Case"&narrowtimeforplot$Variable=="InterviewDate","Case Interview",ifelse(narrowtimeforplot$Type=="Control"&narrowtimeforplot$Variable=="InterviewDate","Control Interview",ifelse(narrowtimeforplot$Type=="Case"&narrowtimeforplot$Variable=="DateFirstLepto","Onset of symptoms",NA)))
narrowtimeforplot<-narrowtimeforplot[!is.na(narrowtimeforplot$VarType),]
time2.1<-ggplot(data = narrowtimeforplot, aes(x=Date, fill=Type))+geom_histogram(stat="bin", bins = 25, breaks=as.Date(ymd(20190701)%m+% months(0:24)))+ scale_x_date(breaks=c(ymd('2019-07-01') %m+% months(which(0:24%%3==0)-1)), date_labels = "%m-%Y", limits =c(ymd("2019-06-01"),ymd("2021-07-01")))+ labs(x= "Date (month-year)", y= "Number of participants", fill="Group")+facet_grid(rows = vars(VarType))
time2.1

#Now using time of the year
dateforplot<-dat[match(c("QID","Type","IntervieworSickDayY"),names(dat))]
descriptionlabel<-c("Onset of symptoms", "Interview")
description_labeller <- function(variable,value){
  return(descriptionlabel[value])
}

time3.1<-ggplot(data = dateforplot, aes(x=IntervieworSickDayY, fill=Type))+stat_bin(bins = 12,breaks=c(0,match(as.Date(ymd(20210201)%m+% months(0:11)),as.Date(ymd(20210101):ymd(20220101)))) )+ labs(x= "Month of the year", y= "Number of participants", fill="Group", labels= c(month.abb[1:12]) ) + scale_x_continuous(breaks =c(0,match(as.Date(ymd(20210201)%m+% months(0:10)),as.Date(ymd(20210101):ymd(20211231)))), labels = c(month.abb[1:12]))+facet_grid(rows = vars(Type), labeller= description_labeller)
time3.1


#Transform the Interview or Sick variable into Season of the Year
dat$IntervieworSickSeason<-ifelse(dat$IntervieworSickMonthDay<"03-20","Summer",ifelse(dat$IntervieworSickMonthDay<"06-21","Autumn", ifelse(dat$IntervieworSickMonthDay<"09-23","Winter",ifelse(dat$IntervieworSickMonthDay<"12-21","Spring","Summer"))))
dat$IntervieworSickSeason
dat$IntervieworSickSeason<-factor(dat$IntervieworSickSeason)
summary(dat$IntervieworSickSeason)
#Testing if there is a significant difference in the aeason distribution of cases and controls
chisq.test(ctab(dat$IntervieworSickSeason, dat$Type)$table)
fisher.test(ctab(dat$IntervieworSickSeason, dat$Type)$table)

#Now, What I will do is to transform the Interview or Sick variable to number of days distant from the Solstice. that is a way to have similar numeric values for days with similar seasons. so winter would have low values, and summer would have high values. 
dat$IntervieworSickSolsticeDist<-as.numeric(abs(as.Date(ifelse(is.na(dat$IntervieworSick),NA,paste(year(dat$IntervieworSick),"-06-21",sep= "")))-dat$IntervieworSick))
dat$IntervieworSickSolsticeDist

ggplot(data = dat, aes(x=IntervieworSickSolsticeDist, fill=Type))+geom_histogram(stat="bin", bins = 22) + labs(x="Number of days of distance from winter solstice") + scale_fill_discrete(labels=c("Case first sick date", "Control interview date"))

#Checking if there is a statistical difference between distance from solstice of cases and controls.
wilcox.test(dat$IntervieworSickSolsticeDist[dat$Type=="Case"],dat$IntervieworSickSolsticeDist[dat$Type=="Control"])


#creating a vector with the column numbers of time variables
Timevariables<-which(grepl("Interview", names(dat))&!grepl("Name",names(dat)))
names(dat[Timevariables])


# Simple plot with dates of cases onset of symptoms and dates of interview of controls
ggplot(data = dat, aes(x=DateFirstLepto))+geom_histogram()
ggplot(data=dat[dat$Type=="Case",], aes(x=IntervieworSickDayY))+geom_histogram()

#Creating a vector with the month of illness
monthillness<-data.frame(table(month(dat$DateFirstLepto)))
names(monthillness)<-c("Month","Count")
monthillness$Month<-as.numeric(monthillness$Month)

#Drawing a plot with frequency of case onset of symptoms per month and the loess curve.
time4<- ggplot(data=monthillness, aes(Month, Count))+geom_col() +  geom_smooth() + scale_x_continuous(breaks= 1:12, labels = c(month.abb[1:12])) + labs(x= "Frequency of cases onset of symptoms per month and loess smooth curve with confidence intervals (shaded)", y= "Count" ) + scale_y_continuous(limit=c(0,NA),oob=squish)

$Creating a vector with the month and year of onset of Symptoms
datesfirstill<-data.frame(ymd(20190601)%m+% months(0:23),hist(dat$DateFirstLepto, breaks=c(as.Date(ymd(20190601)%m+% months(0:24))))$counts)  
  names(datesfirstill)<-c("Months", "Count")

  #The same plot as time4 but this time the whole timeline - year and month
time5<-ggplot(data = datesfirstill, aes(x=Months, Count ))+geom_col() + geom_smooth() + labs(x= "Frequency of cases onset of symptoms per month and loess smooth curve with confidence intervals (shaded)", y= "Count" ) + scale_y_continuous(limit=c(0,NA),oob=squish)

grid.arrange(time5, time4)

#My idea now is to compare cases from the study with total notified cases by the ESR+Public health published in the Public Health Surveillance website: https://surv.esr.cri.nz/

#Now creating a dataframe with number of people with onset of symptoms per month - please update the number of months you want to search in the sequence ( for example : months(0:30)). Doing the same for invitation date.
datesfirstill<-data.frame(ymd(20190601)%m+% months(0:23),hist(dat$DateFirstLepto, breaks=c(as.Date(ymd(20190601)%m+% months(0:24))))$counts) 

datesinvited<-data.frame(ymd(20190601)%m+% months(0:24),hist(dat$InvitationDate, breaks=c(as.Date(ymd(20190601)%m+% months(0:25))))$counts) 


names(datesfirstill)<-c("Months", "Count")
names(datesinvited)<-c("Months", "Count")


datesfirstill$Type<-rep("Enrolled cases",length(datesfirstill$Count))
datesinvited$Type<-rep("Enrolled cases",length(datesinvited$Count))

#The most appropriate information to compare with Notified cases is the invitation date because it is close to the notificaton date

#Now I will do the same for notified cases
datesnotified<-datesfirstill
#The line below has been taken from the Health Surveillance website.It was the most updated information. It is equivalent to the months June 2019 to May -2021. these lines might have to be updated for mor recent information. 
datesnotified$Count<-c(12,13,12,17,15,11,10,11,10,5,2,6,5,7,3,6,8,10,9,4,5,8,14,9)
datesnotified$Type<-rep("Notifications",length(datesnotified$Count))
#Finally I join invitation date and notified date into one single data frame and use it to draw the comparison plots.
datesallcases<-rbind(datesinvited,datesnotified)
datesallcases

#I will also create data frames to compare enrolled cases and notified cases by month (seasonality trends).
datesnotified$Month<-month(datesnotified$Months)
monthnotifications<-aggregate(datesnotified$Count,list(datesnotified$Month),function(x)(sum(x)))
names(monthnotifications)<-c("Month","Count")

monthnotifications$Type<-rep("Notifications",length(monthnotifications$Count))
monthillness$Type<-rep("Enrolled cases",length(monthillness$Count))
allmonthcases<-rbind(monthillness,monthnotifications)


#Plotting the comparison as a timeline
time6<-ggplot(data = datesallcases, aes(x=Months, Count, fill=Type, colour=Type))+geom_col(position = "dodge")  + scale_y_continuous(limit=c(0,NA),oob=squish)+scale_fill_brewer(palette = "Set2", direction = -1)  + labs(y= "Number of patients" )

#Plotting the comparison as in day of the year
time7<-ggplot(data = allmonthcases, aes(x=Month, Count, fill=Type, colour=Type))+geom_col(position = "dodge") + scale_y_continuous(limit=c(0,NA),oob=squish)+scale_fill_brewer(palette = "Set2", direction = -1)  + scale_x_continuous(breaks= 1:12, labels = c(month.abb[1:12])) + labs(y= "Number of patients" )
#If you want to add to these plots a loess curve, simply add to the time 6 and time 7 plots the line below without the pound sign.
#+ geom_smooth()
time6
time7


###########################DEMOGRAPHY######
##################################################rurality and Spatial data######
#Classification by Nearest Primary school
### the classification into regions and rurality by primary school was done manually, so there is no specific code in R for this classification. There is an excel file rural-spatial.xlsx for this work but I have included all the values on the following lines. All the values retrieved will be sent to the main dataframe
ruralspatialschool<-as.data.frame(matrix(nrow =218, ncol = 5))
names(ruralspatialschool)<-c("QID", "ParticipantID", "IDN", "RegionSchools", "RuralitySchools")
ruralspatialschool$QID<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,148,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,224,229,230,231)

ruralspatialschool$ParticipantID<-c("LA300","LA301","LA302","LA303","LA304","LA305","LA308","LA306","LA329","LA309","LA136","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","LA336","LA131","LA311","LA312","LA313","LA314","LA315","LA317","LA318","LA319","LA320","LA322","LA323","LA324","LA325","LA330","LA335","LA331","LA333","LA339","LA340","LA342","LA344","LA346","LA347","NA","LA350","LA337","LA351","LA124ii","LA354","LA348","LA356","LA355","LA352","LA353","LA359","LA360","LA362","LA364","LA361","LA365","LA369","LA371","LA367","LA373","LA374","LA368","LA370","LA372","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","LA375","LA366","LA377","LA144","LA382","LA386","LA387")

ruralspatialschool$IDN<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,119,251,274,78,256,278,198,87,60,20,188,75,174,229,271,3,130,263,171,152,232,54,43,74,189,55,284,187,95,168,238,61,153,39,12,156,111,200,28,177,67,63,62,46,1,108,5,227,22,225,194,231,400,446,486,273,475,487,328,552,570,349,389,363,468,424,520,453,346,361,320,106,564,327,451,498,527,395,493,332,528,362,323,309,560,512,448,326,430,460,303,322,568,509,485,473,504,537,488,524,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,652,624,640,834,783,677,691,622,614,778,719,654,790,669,786,602,813,707,668,646,625,688,575,854,712,661,616,634,626,591,736,800,576,742,686,598,828,820,596,788,796,746,577,643,818,701,667,619,852,812,NA,NA,NA,NA,NA,NA,NA)

ruralspatialschool$RegionSchools<- c("Southland","Hawke's Bay","Bay of Plenty","Waikato","Southland","Southland","Northland","Otago","Tasman","Taranaki","Northland","Southland","West Coast","Waikato","Northland","Waikato","Wellington","NA","Hawke's Bay","Waikato","Canterbury","Gisborne","Waikato","Waikato","Tasman","Gisborne","Canterbury","Northland","Canterbury","Bay of Plenty","Manawatu-Whanganui","Waikato","Southland","Waikato","Waikato","Hawke's Bay","Wellington","Northland","Otago","Otago","Bay of Plenty","Otago","Auckland","Bay of Plenty","Southland","Northland","NA","Northland","Wellington","Bay of Plenty","Manawatu-Whanganui","Southland","Tasman","Northland","Waikato","Southland","West Coast","Canterbury","NA","Waikato","Bay of Plenty","Auckland","Wellington","Canterbury","Taranaki","Canterbury","Auckland","Waikato","Tasman","Northland","Manawatu-Whanganui","Waikato","Hawke's Bay","Otago","Southland","Wellington","Waikato","Wellington","Auckland","Bay of Plenty","Waikato","Taranaki","Taranaki","Tasman","NA","Canterbury","Auckland","Auckland","Bay of Plenty","Auckland","Hawke's Bay","Canterbury","Canterbury","Wellington","Waikato","Bay of Plenty","NA","Otago","Southland","Northland","Otago","Northland","Southland","Auckland","Hawke's Bay","Wellington","NA","Wellington","Auckland","Bay of Plenty","Auckland","Hawke's Bay","Northland","Hawke's Bay","Taranaki","Manawatu-Whanganui","Manawatu-Whanganui","Waikato","Waikato","Manawatu-Whanganui","Waikato","Waikato","Tasman","Waikato","Waikato","Manawatu-Whanganui","Waikato","Waikato","Waikato","Manawatu-Whanganui","Southland","Southland","Bay of Plenty","Manawatu-Whanganui","Waikato","Northland","NA","Waikato","Waikato","Canterbury","Northland","Waikato","Otago","Manawatu-Whanganui","Northland","Hawke's Bay","Manawatu-Whanganui","Auckland","Waikato"," Waikato","Manawatu-Whanganui","Waikato","Manawatu-Whanganui","Manawatu-Whanganui","Waikato","Taranaki","Manawatu-Whanganui","Hawke's Bay","Taranaki","Waikato","Waikato","NA","Otago","Auckland","Bay of Plenty","Canterbury","Hawke's Bay","Auckland","Waikato","Otago","Northland","Otago","Bay of Plenty","Wellington","Canterbury","Auckland","Waikato","Wellington","Auckland","Wellington","Hawke's Bay","Waikato","Southland","Hawke's Bay","NA","Tasman","Northland","Tasman","Northland","West Coast","Canterbury","Bay of Plenty","NA","Otago","Southland","Southland","NA","Auckland","Hawke's Bay","Wellington","Auckland","NA","Waikato","Northland","Southland","Canterbury","West Coast","Waikato","Bay of Plenty","NA","Northland","Waikato","Auckland","Northland","Northland","Waikato","Manawatu-Whanganui","Manawatu-Whanganui")


ruralspatialschool$RuralitySchools<-c("Rural","NA","Rural","Main Urban","Rural","Main Urban","Main Urban","Rural","Minor Urban","Rural","Minor Urban","Rural","Minor Urban","Minor Urban","Rural","Rural","Main Urban","NA","Rural","Minor Urban","Minor Urban","Main Urban","Rural","Main Urban","Main Urban","Main Urban","Secondary Urban","Rural","Rural","Main Urban","Minor Urban","Main Urban","Main Urban","Minor Urban","Rural","Main Urban","Main Urban","Rural","Main Urban","Rural","Main Urban","Main Urban","Rural","Minor Urban","Rural","Rural","NA","Main Urban","Main Urban","Rural","Main Urban","Rural","Rural","Rural","Minor Urban","Rural","Rural","Rural","NA","Minor Urban","Main Urban","Main Urban","Main Urban","Rural","Main Urban","Secondary Urban","Main Urban","Secondary Urban","Main Urban","Rural","Rural","Secondary Urban","Rural","Rural","Rural","Main Urban","Rural","Main Urban","Main Urban","Secondary Urban","Rural","Rural","Main Urban","Minor Urban","NA","Rural","Main Urban","Main Urban","Main Urban","Main Urban","Main Urban","Main Urban","Rural","Main Urban","Rural","Secondary Urban","NA","Minor Urban","Rural","Rural","Secondary Urban","Rural","Minor Urban","Main Urban","Main Urban","Main Urban","NA","Minor Urban","Main Urban","Main Urban","Main Urban","Rural","Rural","Rural","Rural","Rural","Rural","Main Urban","Rural","Secondary Urban","Rural","Rural","Rural","Rural","Rural","Minor Urban","Rural","Main Urban","Rural","Rural","Rural","Rural","Minor Urban","Main Urban","Main Urban","Rural","NA","Rural","Main Urban","Rural","Rural","Minor Urban","Rural","Main Urban","Rural","Main Urban","Secondary Urban","Main Urban","Rural","Rural","Minor Urban","Rural","Rural","Main Urban","Rural","Rural","Rural","Rural","Minor Urban","Secondary Urban","Main Urban","NA","Minor Urban","Main Urban","Main Urban","Rural","Minor Urban","Main Urban","Minor Urban","Main Urban","Main Urban","Minor Urban","Main Urban","Main Urban","Rural","Main Urban","Minor Urban","Rural","Main Urban","Minor Urban","Rural","Rural","Rural","Rural","NA","Rural","Rural","Rural","Minor Urban","Rural","Secondary Urban","Rural","NA","Rural","Rural","Rural","NA","Main Urban","Main Urban","Main Urban","Main Urban","NA","Rural","Rural","Rural","Main Urban","Minor Urban","Rural","Main Urban","NA","Main Urban","Main Urban","Rural","Main Urban","Rural","Minor Urban","Secondary Urban","Rural")

#this is the final table for rurality and regions
dim(ruralspatialschool)
ruralspatialschool

#first converting text Nas to real NAs
ruralspatialschool$RegionSchools[ruralspatialschool$RegionSchools=="NA"]<-NA
ruralspatialschool$RuralitySchools[ruralspatialschool$RuralitySchools=="NA"]<-NA
ruralspatialschool$IDN[ruralspatialschool$IDN=="NA"]<-NA
#removing the rows with no results (whose participants were also eliminated in the main dataframe )
dim(ruralspatialschool)
ruralspatialschool<-ruralspatialschool[!(is.na(ruralspatialschool$IDN)&is.na(ruralspatialschool$RegionSchools)&is.na(ruralspatialschool$RuralitySchools)),]
dim(ruralspatialschool)
#the columns ParticipantID and IDN are a reference for the cases and controls but they can be summarized by the QID column, so they will be removed to merge with the main dataframe
ruralspatialschool<- ruralspatialschool[-c(2:3)]
head(ruralspatialschool)

#inserting the region and rurality columns to the main data frame
dim(dat)
dat<- base::merge(dat,ruralspatialschool, by="QID", all.x=T, all.y=F)
dim(dat)
#This is the table of regions of nearest primary schools:
regionsofschools<-ctab(as.factor(dat$RegionSchools), as.factor(dat$Type))$table
regionsofschools

#the table of rurality of nearest primary school
ruralityschools<-ctab(as.factor(dat$RuralitySchools), as.factor(dat$Type), addmargins = T, percentages = T)

ruralityschools
dat$RuralitySchools

#Merging categories to only Urban and Rural
dat$Gen.RuralitySchools<-factor(ifelse(grepl("Rural", dat$RuralitySchools, ignore.case = T), "Rural", ifelse(grepl("Urban", dat$RuralitySchools,ignore.case = T), "Urban", NA)))
Gen.ruralityschools<-ctab(dat$Gen.RuralitySchools,dat$Type)
Gen.ruralityschools

## Rurality according to respondents' address. This is the most used variable for rurality in the study

#QID
QID<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,148,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,224,229,230,231)

#IDN
IDN<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,119,251,274,78,256,278,198,87,60,20,188,75,174,229,271,3,130,263,171,152,232,54,43,74,189,55,284,187,95,168,238,61,153,39,12,156,111,200,28,177,67,63,62,46,1,108,5,227,22,225,194,231,400,446,486,273,475,487,328,552,570,349,389,363,468,424,520,453,346,361,320,106,564,327,451,498,527,395,493,332,528,362,323,309,560,512,448,326,430,460,303,322,568,509,485,473,504,537,488,524,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,652,624,640,834,783,677,691,622,614,778,719,654,790,669,786,602,813,707,668,646,625,688,575,854,712,661,616,634,626,591,736,800,576,742,686,598,828,820,596,788,796,746,577,643,818,701,667,619,852,812,NA,NA,NA,NA,NA,NA,NA)

# ParticipantID
ParticipantID<- c("LA300","LA301","LA302","LA303","LA304","LA305","LA308","LA306","LA329","LA309","LA136","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","LA336","LA131","LA311","LA312","LA313","LA314","LA315","LA317","LA318","LA319","LA320","LA322","LA323","LA324","LA325","LA330","LA335","LA331","LA333","LA339","LA340","LA342","LA344","LA346","LA347","NA","LA350","LA337","LA351","LA124ii","LA354","LA348","LA356","LA355","LA352","LA353","LA359","LA360","LA362","LA364","LA361","LA365","LA369","LA371","LA367","LA373","LA374","LA368","LA370","LA372","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","LA375","LA366","LA377","LA144","LA382","LA386","LA387")
# Rurality according to address
RuralityHomeaddr<- c("Rural other","Large urban area","Medium urban area","Small urban area","Rural other","Large urban area","Rural other","Rural settlement","Rural other","Rural other","Rural other","Rural settlement","Small urban area","Small urban area","Rural settlement","Rural other","Major urban area","Major urban area","Rural settlement","Small urban area","Rural other","Large urban area","Rural other","Major urban area","Small urban area","Large urban area","Rural other","Rural other","Rural settlement","Major urban area","Rural other","Medium urban area","Rural other","Rural other","Rural other","Large urban area","Rural other","Rural other","Major urban area","Rural other","Major urban area","Major urban area","Rural other","Rural settlement","Rural other","Rural other","Rural other","Rural other","Rural other","Rural settlement","Large urban area","Rural settlement","Rural settlement","Rural other","Rural other","Rural other","Rural other","Rural other","Rural other","Rural settlement","Major urban area","Major urban area","Major urban area","Rural other","Rural other","Medium urban area","Major urban area","Large urban area","Medium urban area","Small urban area","Rural other","Medium urban area","Rural other","Rural other","Rural other","Major urban area","Rural other","Major urban area","Rural other","Rural other","Rural settlement","Rural other","Rural other","Small urban area","Rural settlement","Rural settlement","Major urban area","Major urban area","Rural other","Major urban area","Rural other","Major urban area","Rural other","Rural other","Rural other","Medium urban area","Medium urban area","Small urban area","Rural other","Rural other","Medium urban area","Rural other","Rural other","Major urban area","Large urban area","Rural other","Large urban area","Small urban area","Major urban area","Major urban area","Major urban area","Rural other","Rural other","Rural other","Rural other","Rural other","Rural other","Rural other","Rural other","Rural other","Rural settlement","Rural settlement","Rural other","Rural other","Rural other","Rural other","Rural other","Major urban area","Rural other","Rural other","Rural other","Rural other","Small urban area","Large urban area","Major urban area","Rural other","NA","Rural other","Rural other","Rural other","Rural other","Rural other","Rural other","Large urban area","Rural other","Large urban area","Medium urban area","Rural other","Rural other","Rural settlement","Rural other","Rural other","Rural settlement","Rural other","Rural other","Rural other","Rural other","Rural settlement","Rural other","Rural other","Major urban area","Rural other","Small urban area","Rural settlement","Major urban area","Major urban area","Rural other","Small urban area","Small urban area","Rural other","Large urban area","Rural other","Rural settlement","Major urban area","Rural settlement","Major urban area","Rural other","Major urban area","Rural other","Rural other","Rural settlement","Rural other","Rural other","Rural other","Large urban area","Rural other","Small urban area","Rural other","Rural other","Rural settlement","Medium urban area","Rural other","Major urban area","Rural other","Rural other","Rural other","Major urban area","Major urban area","Large urban area","Major urban area","Major urban area","Major urban area","Rural other","Rural other","Rural other","Major urban area","Rural other","Rural settlement","Small urban area","Major urban area","Large urban area","Rural other","Rural other","Large urban area","Rural other","Rural other","Medium urban area","Rural other")
#Region by home address
RegionHomeaddr<-c("Southland","Hawke's Bay","Bay of Plenty","Waikato","Southland","Southland","Northland","Otago","Tasman","Taranaki","Northland","Southland","West Coast","Waikato","Northland","Waikato","Wellington","Waikato","Hawke's Bay","Waikato","Canterbury","Gisborne","Waikato","Waikato","Tasman","Gisborne","Canterbury","Northland","Canterbury","Bay of Plenty","Manawatu-Whanganui","Waikato","Southland","Waikato","Waikato","Hawke's Bay","Wellington","Northland","Otago","Southland","Bay of Plenty","Otago","Auckland","Bay of Plenty","Southland","Northland","Waikato","Northland","Wellington","Bay of Plenty","Manawatu-Whanganui","Southland","Tasman","Northland","Waikato","Southland","West Coast","Canterbury","Hawke's Bay","Bay of Plenty","Bay of Plenty","Wellington","Wellington","Canterbury","Taranaki","Canterbury","Auckland","Waikato","Tasman","Northland","Manawatu-Whanganui","Waikato","Hawke's Bay","Otago","Southland","Wellington","Waikato","Wellington","Auckland","Bay of Plenty","Waikato","Taranaki","Taranaki","Tasman","Manawatu-Whanganui","Canterbury","Auckland","Auckland","Bay of Plenty","Auckland","Hawke's Bay","Canterbury","Canterbury","Wellington","Waikato","Bay of Plenty","Otago","Otago","Southland","Northland","Otago","Northland","Southland","Auckland","Hawke's Bay","Wellington","Hawke's Bay","Wellington","Auckland","Bay of Plenty","Auckland","Hawke's Bay","Northland","Hawke's Bay","Taranaki","Manawatu-Whanganui","Manawatu-Whanganui","Waikato","Waikato","Manawatu-Whanganui","Waikato","Waikato","Tasman","Waikato","Waikato","Manawatu-Whanganui","Waikato","Waikato","Waikato","Hawke's Bay","Southland","Southland","Bay of Plenty","Manawatu-Whanganui","Waikato","Northland","NA","Waikato","Waikato","Canterbury","Northland","Waikato","Otago","Manawatu-Whanganui","Northland","Hawke's Bay","Manawatu-Whanganui","Auckland","Waikato","Waikato","Manawatu-Whanganui","Waikato","Manawatu-Whanganui","Manawatu-Whanganui","Waikato","Taranaki","Manawatu-Whanganui","Hawke's Bay","Taranaki","Waikato","Waikato","Taranaki","Otago","Auckland","Bay of Plenty","Wellington","Hawke's Bay","Auckland","Waikato","Otago","Northland","Otago","Bay of Plenty","Wellington","Canterbury","Auckland","Waikato","Wellington","Waikato","Wellington","Hawke's Bay","Waikato","Southland","Hawke's Bay","Hawke's Bay","Tasman","Northland","Tasman","Northland","West Coast","Marlborough","Bay of Plenty","Wellington","Otago","Southland","Southland","Canterbury","Auckland","Hawke's Bay","Wellington","Auckland","Wellington","Waikato","Northland","Southland","Canterbury","West Coast","Waikato","Otago","Bay of Plenty","Northland","Waikato","Auckland","Northland","Waikato","Waikato","Manawatu-Whanganui","Manawatu-Whanganui")

ruralspatialaddress<-data.frame(QID, ParticipantID, IDN,RuralityHomeaddr, RegionHomeaddr)

write.csv(ruralspatialaddress,file = "ruralspatialaddress.csv")
ruralspatialaddress
# removing participant ID and IDN
head(ruralspatialaddress)
ruralspatialaddress<-ruralspatialaddress[-(2:3)]
dim(ruralspatialaddress)
dim(dat)
length(match(ruralspatialaddress$QID,dat$QID))

dat<-left_join(dat,ruralspatialaddress)
dim(dat)

ruralitytable<-ctab(as.factor(dat$RuralityHomeaddr), dat$Type); ruralitytable
ruralitytable<-as.data.frame(ruralitytable$table)
ruralitytable

## analysing results for rurality - difference between cases and controls
wideruralitytable<-spread(ruralitytable ,key = Var2, value =Freq); wideruralitytable
wideruralitytable$Var1<-factor(wideruralitytable$Var1, levels = c("Major urban area","Large urban area", "Medium urban area", "Small urban area", "Rural settlement", "Rural other"))
wideruralitytable<-wideruralitytable[order(wideruralitytable$Var1),]
names(wideruralitytable)[1]<-"Urban/Rural"
wideruralitytable

#determining the level order for rurality from Major urban area (most urban) to rural other (most rural).
dat$RuralityHomeaddr<-factor(dat$RuralityHomeaddr, levels = c("Major urban area","Large urban area", "Medium urban area", "Small urban area", "Rural settlement", "Rural other"), ordered = T)
dat$RuralityHomeaddr
#testing the difference of rurality as in ordinal values from "Major urban area" to "rural other" - if the null hypothesis is rejected, then there is difference of rurality between cases and controls
wilcox.test(as.numeric(filter(dat,dat$Type=="Case")$RuralityHomeaddr), as.numeric(filter(dat,dat$Type=="Control")$RuralityHomeaddr))
# there was difference between groups p<0.01

#Merging the different urban areas to one. same for rural areas. Creating the variable of urbanxrural - Gen.RuralityHomeaddr. This variable will be further used to adjust the logistic regression
summary.rurality<-data.frame(matrix(data=c(sum(wideruralitytable[1:4,2]),sum(wideruralitytable[1:4,3]),sum(wideruralitytable[5:6,2]),sum(wideruralitytable[5:6,3])), nrow = 2, byrow = T),row.names = c("Urban", "Rural"))
names(summary.rurality)<-c("Cases", "Controls")
summary.rurality
fisher.test(summary.rurality) # same result as the Mann-whitney / wilcoxon rank sum test - the odds ratio under 1 for rurality was signifficant.

dat$RuralityHomeaddr

# Merging the rural x urban categories to 2: Urban and Rural
dat$Gen.RuralityHomeaddr<-factor(ifelse(grepl("Rural", dat$RuralityHomeaddr, ignore.case = T), "Rural", ifelse(grepl("Urban", dat$RuralityHomeaddr,ignore.case = T), "Urban", NA)))
dat$Gen.RuralityHomeaddr
generalruralitybyaddress<-ctab(as.factor(dat$Gen.RuralityHomeaddr),dat$Type)
generalruralitybyaddress

###Analysing Home type which has also to do with Rurality.
dat$HomeType
str(dat$HomeType) #it is integer. First we make calculations as quantitative variable then we change it to factor to do tables. The test we will use is Wilcoxon rank sum test (also known as Mann Whitney`s test`) because the variable may be considered ordinal (from Town to Farm or vice versa. Lifestyleblock is intermediate )

#First we change 999 values to NA
dat[dat$HomeType==999,which(names(dat)=="HomeType")]<-NA

#now check the values subsetting to cases and controls
dat[dat[which(names(dat)=="Type")]=="Case",which(names(dat)=="HomeType")]
dat[dat[which(names(dat)=="Type")]=="Control",which(names(dat)=="HomeType")]
#now executing the test. 
wilcox.test(dat[dat[which(names(dat)=="Type")]=="Case",which(names(dat)=="HomeType")],dat[dat[which(names(dat)=="Type")]=="Control",which(names(dat)=="HomeType")])
##Median is signifficantly different. that means, that cases are signifficantly different from control regarding Home Types. More cases living in farms

#now we change it ot factors and change the names to facilitate the reading of the table
dat$HomeType<-factor(ifelse(dat$HomeType==1,"Town/City",ifelse(dat$HomeType==2,"Lifestyle Block", ifelse(dat$HomeType==3, "Farm", NA))))

dat$HomeType
hometypetable<-ctab(dat$HomeType, dat$Type)
hometypetable
chisq.test(ctab(dat$HomeType, dat$Type)$table)

#This is the table of regions according to home address
regionshomeaddresstable<-ctab(as.factor(dat$RegionHomeaddr), dat$Type)
regionshomeaddresstable


#Now I will create dichotomous variables with the regions. FOr example: the variable for Waikato will have -1 if someone's home address is from Waikato and 0 if not.
#regions
regionames<-unique(dat$RegionHomeaddr)
#In this loop I am assigning names of variables from each region
for (i in 1:length(regionames)) { 
  assign(paste("Region",regionames[i],sep = ""),rep(0,nrow(dat))) }
#then creating a dataframe with the region names
regionnamesdf<-data.frame(matrix(nrow=nrow(dat),ncol=length(regionames)))
names(regionnamesdf)<-regionames
head(regionnamesdf)
regionnamesdf[is.na(regionnamesdf)]<-0
head(regionnamesdf)

#Now I am inserting the values from each participant. 
for (i in 1:nrow(dat)) { 
  regionnamesdf[i,match(dat$RegionHomeaddr[i],names(regionnamesdf))]<- -1 }
regionnamesdf
#And finally joining to the main database.
dat<-cbind(dat,regionnamesdf)
#Now I am creating a vector with the assigned number of each region column at the main dataframe. I will work a lot with this type of vector in this file. 
regioncolumns<-(match(regionames,names(dat)))
regioncolumns
#Making the names easier to work with. Removing the spaces and special characters from the region names
adjregionames<-sapply(regionames,function(x)gsub(" ","",x))
adjregionames<-unlist(lapply(adjregionames,function(x)gsub("'","",x)))
adjregionames<-unlist(lapply(adjregionames,function(x)gsub("-","",x)))
adjregionames
names(dat)[regioncolumns]<-adjregionames
names(dat)[regioncolumns]<-paste("Region",names(dat)[regioncolumns],sep="")
names(dat)[regioncolumns]
#Now all the region names have the prefix Region.

#Comparing home types, rurality of homes and schools. Understanding how the variavles work with each other
summary(dat$HomeType)
summary(dat$HomeType[dat$Gen.RuralityHomeaddr=="Rural"])
summary(dat$HomeType[dat$Gen.RuralityHomeaddr=="Urban"])
#Cases from Towns/Cities
summary(dat$Gen.RuralityHomeaddr[dat$HomeType=="Town/City"&dat$Type=="Case"])
#Controls from Town/ Cities 
summary(dat$Gen.RuralityHomeaddr[dat$HomeType=="Town/City"&dat$Type=="Control"])
#The difference between variables does not seem to be diferential.

#doing the same for lifestyle block and Farms
summary(dat$Gen.RuralityHomeaddr[dat$HomeType=="Lifestyle Block"&dat$Type=="Case"])
summary(dat$Gen.RuralityHomeaddr[dat$HomeType=="Lifestyle Block"&dat$Type=="Control"])
summary(dat$Gen.RuralityHomeaddr[dat$HomeType=="Farm"&dat$Type=="Case"])
summary(dat$Gen.RuralityHomeaddr[dat$HomeType=="Farm"&dat$Type=="Control"])

#Now doing the same for rurality of nearest primary schools x home address
summary(dat$Gen.RuralityHomeaddr[dat$Gen.RuralitySchools=="Rural"&dat$Type=="Case"])
summary(dat$Gen.RuralityHomeaddr[dat$Gen.RuralitySchools=="Rural"&dat$Type=="Control"])
summary(dat$Gen.RuralityHomeaddr[dat$Gen.RuralitySchools=="Urban"&dat$Type=="Case"])
#A lot of controls who had nearest primary school urban lived in rural areas. It looks a diferential deviation. Would controls that live in rural areas live in "not that rural" areas, compared to cases?
summary(dat$Gen.RuralityHomeaddr[dat$Gen.RuralitySchools=="Urban"&dat$Type=="Control"])

#Old rurality classification x new rurality classification
summary(dat$Gen.RuralityHomeaddr[dat$mrkt=="Non-rural"&dat$Type=="Control"])
#Many controls that were classified as rural were further classified as urban in the new measurement.
summary(dat$Gen.RuralityHomeaddr[dat$mrkt=="Rural"&dat$Type=="Control"])

##############################################Knowledge about Leptospirosis#####
head(dat[grepl("GetLepto",names(dat))])
getleptocolumns<-which(grepl("GetLepto",names(dat))&!grepl("Other",names(dat)))
getleptocolumns
for (i in 1:length(getleptocolumns)) {
  dat[,getleptocolumns[i]]<-factor(dat[,getleptocolumns[i]])
  print(names(dat[getleptocolumns[i]]))
  print(ctab(dat[,getleptocolumns[i]],dat$Type)) }
#It seems this variable is very related to Cases. Have Cases learned more about Lepto since they got ill? How could that contribute to the case control study? It would be interesting to know about their knoldege before they were ill. No further analyses will be done because of this possibility of bias. 


###########################################################Other Demography#########################

#############Age - categorizing age

dat$Agegroup<-factor(ifelse(dat$Age<30,"16-30",ifelse(dat$Age<40,"30-39",
                                                      ifelse(dat$Age<50,"40-49",ifelse(dat$Age<60,"50-59",ifelse(dat$Age<70,"60-69","70+"))))))

summary(dat$Agegroup)


############################# Contingency tables for Age  groups - narrow and wide tables

agegroups<-as.data.frame<-ctab(as.factor(dat$Agegroup),as.factor(dat$Type))
agegroups<-as.data.frame(agegroups$table)
agegroups
agegroups<-spread(agegroups,Var2,Freq)
agegroups

#Difference from the median age
median(dat$Age)
dat$MedianAge<-dat$Age-median(dat$Age)
dat$MedianAge

#Age over 16. That variable will be used for logistic regression.
dat$AgeOver16<-dat$Age-16
dat$AgeOver16

#A violin plot for casexcontrol age
ggplot(data=dat, aes(x=Type, y=Age, fill=Sex))+geom_violin()

#Other plot for age
agegroupcomparison<-data.frame(dat$Agegroup,dat$Type,dat$Sex)
agegroupcomparison

#Just cases
ggplot(data=dat[dat$Type=="Case",], aes(x=Agegroup, fill=Sex))+geom_bar(aes(y=(..count..)/sum(..count..)))+
  scale_fill_brewer(palette="Paired")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank())
#Just controls
ggplot(data=dat[dat$Type=="Control",], aes(x=Agegroup, fill=Sex))+geom_bar(aes(y=(..count..)/sum(..count..)))+
  scale_fill_brewer(palette="Paired")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank())

#Preparing a data frame for another plot. This time for cases and controls
agetypesexplot<-ddply(data.frame(table(dat[sort(match(c("Sex","Agegroup", "Type"),names(dat)))])), .(Type), mutate, pct = round(Freq/sum(Freq) * 100, 1))

ggplot(data=agetypesexplot, aes(x=Type,y=pct, fill=Sex))+ geom_bar(stat = "identity")+
  scale_fill_brewer(palette="Paired")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank())+ facet_grid(cols = vars(Agegroup)) + labs(x="",y= "Percentage (%)") +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  )

#Statistical tests for Age of cases ad controls

dat$Agegroup[dat$Type=="Case"]
median(dat$Age[dat$Type=="Case"])
median(dat$Age[dat$Type=="Control"])
mean(dat$Age[dat$Type=="Case"])
mean(dat$Age[dat$Type=="Control"])

#Age looks a normal distribution variable
hist(dat$Age)
# A t test is appropriate for normal distributed variables
t.test(dat$Age[dat$Type=="Case"],dat$Age[dat$Type=="Control"])
# I will also do a non-parametrical test:
wilcox.test(dat$Age[dat$Type=="Case"],dat$Age[dat$Type=="Control"])
#significant difference for both them.

### analysing ethnicity
dat$ethnicity #there are some ethnicity values as NAs, although looking at the database, they shouldn`t be NAs.`
#substituting Nas for 0s
dat[grepl("Ethnicity",names(dat))&(!names(dat)%in% "Ethnicity")][is.na(dat[grepl("Ethnicity",names(dat))&(!names(dat)%in% "Ethnicity")])]<-0

ethnicitynoNA<-dat[grepl("Ethnicity",names(dat))&(!names(dat)%in% "Ethnicity")]

ethnicitynoNA[is.na(ethnicitynoNA)]<-0       #removing Nas from these columns
#reapplying the code for ethnicity - the last line was changed because the other ethnicity column is categorical, not dichotomous. i am also applying the Nz group priority as of Ministry of Health 
ethnicitynoNA$ethnicity = ifelse(ethnicitynoNA$EthnicityNZMaori == -1, "Maori",ifelse(ethnicitynoNA$EthnicityNiuean == -1, "Pacific Peoples",ifelse(ethnicitynoNA$EthnicityTongan == -1, "Pacific Peoples",ifelse(ethnicitynoNA$EthnicityCookIsMaori == -1,"Pacific Peoples",ifelse(ethnicitynoNA$EthnicitySamoan == -1, "Pacific Peoples",ifelse(ethnicitynoNA$EthnicityIndian == -1,"Asian",ifelse(ethnicitynoNA$EthnicityChinese == -1,"Asian",ifelse(ethnicitynoNA$EthnicityOthers == -1, "Other Ethnicity",ifelse(ethnicitynoNA$EthnicityNZEuropean == -1,"European",ifelse(ethnicitynoNA$EthnicityOthers == 0,"Not declared", "Other Ethnicity"))))))))))

dat$ethnicity<-ethnicitynoNA$ethnicity
ethnicity<-ctab(as.factor(dat$ethnicity), as.factor(dat$Type))
table(dat$ethnicity,dat$Type)
ethnicity$table

ethnic.<- as.data.frame(ethnicity$table)
ethnic. # will spread in  2 columns: Case and control
ethnicity<-spread(ethnic.,Var2,Freq)
ethnicity
total.ethnic.<-as.data.frame(ethnicity$Case+ethnicity$Control) 
names(total.ethnic.)[1]<-"Total"
total.ethnic.

ethnicity$Case<-as.numeric(ethnicity$Case)
ethnicity$Control<-as.numeric(ethnicity$Control )
ethnicity
ethnicitytable<-cbind(ethnicity, total.ethnic.)
ethnicitytable

# Simple statistics to check if the table is significantly different from cases and controls within ethnicity
chisq.test(ethnicity[2:3])
fisher.test(ethnicity[2:3])


####Gender/Sex
sexwide<-spread(sex,Type,cases)

sexwide  

# the initial matching was done according to the previous cases of Leptospirosis. the frequency matching was planned to be 90% male, 10% female

######Education  
dat$Education[dat$Education=="Primary"]<-2;dat$Education[dat$Education=="Secondary"]<-3;dat$Education[dat$Education=="Tertiary education"]<-4 
dat$Education<-factor(dat$Education, levels=c(1,2,3,4))
dat$Education

education<-ctab(as.factor(dat$Education), as.factor(dat$Type))$table
education
educationperc<-data.frame("Cases"=(education[,1]/sum(education[,1]))*100,"Controls"=(education[,2]/sum(education[,2]))*100, row.names = c("No Education", "Primary", "Secondary", "Tertiary") )
educationperc<-rownames_to_column(educationperc,"EducationLevel"); educationperc

narroweducation<-gather(data = educationperc,"Group","Percentage",2:3);narroweducation
#changing 0s to 0.1s so that the bars can appear at ggplot
narroweducation[narroweducation$Percentage==0,3]<-0.1
narroweducation

ggplot(data=narroweducation, aes(x=EducationLevel, y=Percentage, fill=Group))+geom_col(position = position_dodge(preserve = "single"))+
  labs(x="Highest Education Level", y="percent", title = 
         "Study level per groups")+ coord_cartesian(ylim = c(0, 60))

#test if there is signifficant difference between groups
wilcox.test(as.numeric(dat$Education[dat$Type=="Case"]),as.numeric(dat$Education[dat$Type=="Control"]))

#The variable below may be useful to test correlations further.
dat$NumericEducation<-as.numeric(dat$Education)


####Income\

#change 99s to Nas
dat[which(dat$PersonalIncome==99),which(names(dat)=="PersonalIncome")]<-NA
dat$PersonalIncome<-factor(as.character(dat$PersonalIncome))

# at the moment, new values have been inserted as text, not as factors 1-5 like old values were. change all the values to the same base.
newincomepattern<-c("Zero income", "\\$0 to \\$14,000" , "\\$14,001 to \\$48,000", "\\$48,001 to \\$70,000", "\\$70,001 and over")
newincome<-c("Zero income", "$0 to $14,000" , "$14,001 to $48,000", "$48,001 to $70,000", "$70,001 and over")
oldincome<-c(1:5)
#changing the patterns
dat[grep(paste(newincomepattern, collapse = "|"), dat[,which(names(dat)=="PersonalIncome")], ignore.case = T),111]<- oldincome[na.omit(match(dat$PersonalIncome, newincome))]
dat$PersonalIncome
#changing refuse to answetr to NA
dat$PersonalIncome[dat$PersonalIncome=="Refuse to answer"]<-NA
dat$PersonalIncome<-factor(dat$PersonalIncome, levels=sort(unique(dat$PersonalIncome)))
dat$PersonalIncome

#tables for incomes
#narrow table for income frequencies
income<-ctab(as.factor(dat$PersonalIncome), as.factor(dat$Type))
income<-as.data.frame(income$table)
names(income)[2]<-"Group"
names(income)[1]<-"Income Level"

income
#wide table
wideincome<-spread(income,key = Group, value = Freq)
wideincome

#wide table for percentages
Incomeperc<-data.frame("Cases"=(wideincome[,2]/sum(wideincome[,2]))*100,"Controls"=(wideincome[,3]/sum(wideincome[,3]))*100,row.names = c("No income", "<NZ$14,000", "NZ$14,001 to 48,000", "NZ$48,0001 to 70,000", ">NZ$70,000") )
Incomeperc

#changing 0s to 0.1 to appear bars in ggplot
Incomeperc[Incomeperc==0]<-0.1
Incomeperc

Incomeperc<-rownames_to_column(Incomeperc,"IncomeLevel"); Incomeperc

#changing back to wide for percentages
wideIncomeperc<-gather(Incomeperc,key = "Group", value = "Percentage",2:3)
wideIncomeperc


wideIncomeperc$IncomeLevel<-factor(wideIncomeperc$IncomeLevel,ordered = T, levels=c("No income","<NZ$14,000","NZ$14,001 to 48,000","NZ$48,0001 to 70,000", ">NZ$70,000"))

ggplot(data=wideIncomeperc, aes(x=IncomeLevel, y=Percentage, fill=Group))+geom_col(position = "dodge")+
  labs(x="Income level", y="percent", title = 
         "Income level per groups")

orderedincome <- ordered(as.factor(dat$PersonalIncome))

#Now that I created an ordered factor, I will calculate medians of the factor variable.
summary(orderedincome)
median(orderedincome, ordered_low=T, na.rm = T)
median(ordered(as.factor(dat$PersonalIncome[dat$Type=="Case"]  )),ordered_low=T, na.rm = T)
median(ordered(as.factor(dat$PersonalIncome[dat$Type=="Control"]  )),ordered_low=T, na.rm = T)

quantile(ordered(as.factor(dat$PersonalIncome[dat$Type=="Control"]  )),type = 1, na.rm = T)
quantile(ordered(as.factor(dat$PersonalIncome[dat$Type=="Case"]  )),type = 1, na.rm = T)

wilcox.test( as.numeric(dat$PersonalIncome[dat$Type=="Case"]), as.numeric(dat$PersonalIncome[dat$Type=="Control"]),alternative = "greater")

dat$NumericIncome<-as.numeric(dat$PersonalIncome)



################################################################Occupations#################

# the following diagram is how the occupations were categorized

##--
##                      Agriculture Workers                     
##--
#                             | 
#                             ???
##---
##      
##  1 Horticulture workers
##  2 Mixed ( Livestock and Horticulture Workers)     
##  3 Unidentified Agricultural Workers (no clues about type of work in agriculture) 
##  4                Livestock Workers
##--
#                             ???
##--
##                      Livestock Workers
##                        ???
##  1 Dairy Workers
##  2 mixed Dairy / Dry Stock Workers
##  3 Dry Stock workers
##  4 Unidentified livestock workers (no clues about type of livestock farming)
##---
##                         ??? 
##---
##                      Dry Stock Workers           
##                         ???                              
##                      Beef Cattle workers
##                      Sheep workers 
##                      Other Dry Stock
##--
##---
##                      Other professions labelled:    
##                          Meat Workers
##                          Building Industry Workers
##                          Health Workers
##                          Transportation
##                          Engineering
##                          Other occupations
##                          Retired
##                          Jobless (jobless, unemployed, no work, no information available - NAs)                  
##---
#building the search patterns for each occupation - each occupation will have a searching pattern vector and one pattern vector that excludes each profession:

#agriculture occupations
# livestock occupation
dairypatterns<-c("dairy", "diary", "milk" )
sheeppatterns<-c("sheep", "wool")
beefcattlepatterns<-c("beef", "calving")
drystockpatterns<-c (sheeppatterns, beefcattlepatterns, "dry stock", "Stock farm")
livestockpatterns<-c(dairypatterns,drystockpatterns, "animals")
horticulturepatterns<-c("horticulture", "orchard", "gardner", "orchid", "forestry", "forrestry")
agriculturepatterns<-c(livestockpatterns,horticulturepatterns, "agricultur", "farm")
# Other professions
meatpatterns<-c("abbat", "meat", "butch", "slaughter", "offal")
buildingpatterns<-c("build", "construction")
engineerpatterns<-c("engineer")
transportpatterns<-c("driver", "truck", "pilot")
healthpatterns<-c("nurse", "dentist", "health" )
retiredpatterns<-c("retired")
joblesspatterns<-c("unemployed","jobless","not working", "Other")
allpatterns<-list(dairypatterns, sheeppatterns,beefcattlepatterns ,drystockpatterns, livestockpatterns, horticulturepatterns, agriculturepatterns,meatpatterns,buildingpatterns,engineerpatterns,transportpatterns,healthpatterns,retiredpatterns,joblesspatterns)

names(allpatterns)<-c("dairypatterns","sheeppatterns","beefcattlepatterns","drystockpatterns","livestockpatterns","horticulturepatterns","agriculturepatterns","meatpatterns","buildingpatterns","engineerpatterns","transportpatterns","healthpatterns","retiredpatterns","joblesspatterns")

##### patterns that eliminate the job description from each occupation
nondairypatterns<-c("engineering", "10 years back")
nonsheeppatterns<-c("pelting")
nonbeefpatterns<-c("driver")
nondrystockpatterns<-c(nonbeefpatterns,nonsheeppatterns)
nonlivestockpatterns<-c(nondairypatterns, nondrystockpatterns)
nonhorticulturepatterns<-c("zzz")
nonagriculturepatterns<-c(nonlivestockpatterns,nondrystockpatterns) 
nonmeatpatterns<-("zzz")
nonbuildingpatterns<-c("boots")
nonengineerpatterns<-c("part time farmer")
nontransportpatterns<-("zzz")
nonhealthpatterns<-("zzz")
nonretiredpatterns<-("zzz")
nonjoblesspatterns<-("zzz")
allnonpatterns<-list(nondairypatterns, nonsheeppatterns,nonbeefpatterns, nondrystockpatterns, nonlivestockpatterns, nonhorticulturepatterns, nonagriculturepatterns,nonmeatpatterns,nonbuildingpatterns,nonengineerpatterns,nontransportpatterns,nonhealthpatterns,nonretiredpatterns,nonjoblesspatterns)
names(allnonpatterns)<-c("nondairypatterns","nonsheeppatterns","nonbeefpatterns","nondrystockpatterns","nonlivestockpatterns","nonhorticulturepatterns","nonagriculturepatterns","nonmeatpatterns","nonbuildingpatterns","nonengineerpatterns","nontransportpatterns","nonhealthpatterns","nonretiredpatterns","nonjoblesspatterns")

#Now I am going to create a dataframe for the occupations described above. the "non" patterns will eliminate the participant from the described occupation if a description of their job contains the strings in the vectors. When no strings are added to the "non-pattern", then I placed zzz just so that the pattern exists and may be manipulated in the future.

alloccupations<-as.data.frame(matrix(nrow = nrow(dat), ncol = length(allpatterns)))

namesfromoccupations<-as.vector(unlist(strsplit(paste(c(substring(names(allpatterns),1,nchar(names(allpatterns))-8)),collapse=c("workers,",",")),",")))
names(alloccupations)<- namesfromoccupations
head(alloccupations)

#using this loop to search for all the patterns and excluding the non patterns, then inserting the logical results to the matrix alloccupations. This matrix will be inserted to the main data frame as -1s and 0s and one column for each profession
for(i in 1:13){
  alloccupations[,i]<-(grepl(pattern= paste(allpatterns[[i]], collapse="|"), x=dat[,which(names(dat)=="JobBeforeLepto1")], ignore.case=T)&!grepl(pattern = paste(allnonpatterns[[i]], collapse = "|"),x=dat[,which(names(dat)=="JobBeforeLepto1")],  ignore.case = T))|(grepl(pattern= paste(allpatterns[[i]], collapse="|"), x=dat[,which(names(dat)=="JobBeforeLepto2")], ignore.case=T))&!grepl(pattern = paste(allnonpatterns[[i]], collapse = "|"),x=dat[,which(names(dat)=="JobBeforeLepto2")],  ignore.case = T)|(grepl(pattern= paste(allpatterns[[i]], collapse="|"), x=dat[,which(names(dat)=="JobBeforeLepto3")], ignore.case=T)&!grepl(pattern = paste(allnonpatterns[[i]], collapse = "|"),x=dat[,which(names(dat)=="JobBeforeLepto3")],  ignore.case = T))
}

#using a separate formula for jobless to include the NAs
alloccupations[,14]<-(grepl(pattern= paste(allpatterns[[14]], collapse="|"), x=dat[,which(names(dat)=="JobBeforeLepto1")], ignore.case=T)&!grepl(pattern = paste(allnonpatterns[[14]], collapse = "|"),x=dat[,which(names(dat)=="JobBeforeLepto1")],  ignore.case = T))|is.na(dat[,which(names(dat)=="JobBeforeLepto1")])



#inserting to the main data frame
for (i in 1:14){
  dat[dim(dat)[2]+1]<-ifelse(unlist(alloccupations[i]),-1,0)
  names(dat)[dim(dat)[2]]<-namesfromoccupations[i]
}

#creating "other occupations" collumn
dat$OtherOccupations<-rep(0,nrow(dat))

for (i in 1:nrow(dat)) {
  dat$OtherOccupations[i]<-ifelse(!(any(alloccupations[i,]==T)),-1,0)
}


#Refine occupations list by looking at the other columns
# search for other tips of the work in agriculture, look other job columns
dat[alloccupations$agricultureworkers,112:114]
# analysing the specific livestock farm occupations
dat[alloccupations$agricultureworkers&alloccupations$dairyworkers,112:114]
dat[alloccupations$agricultureworkers&alloccupations$beefcattleworkers,112:114] 
dat[alloccupations$agricultureworkers&alloccupations$sheepworkers,112:114]
dat[alloccupations$agricultureworkers&alloccupations$drystockworkers,112:114]
dat[alloccupations$agricultureworkers&alloccupations$horticultureworkers, 112:114]


# Seing what rows are left after eliminating the specific categories for agriculture. the general rows will be later recategorized, until there are no clues left about the specific category for agriculture worker.
dat[alloccupations$agricultureworkers&!(alloccupations$dairyworkers|alloccupations$drystockworkers|alloccupations$beefcattleworkers| alloccupations$sheepworkers|alloccupations$horticultureworkers),112:114]

dat[dat$livestockworkers==0&dat$horticultureworkers==0&dat$agricultureworkers==-1,112:114]


#Quantifying Occupations

head(dat[,which(names(dat)=="dairyworkers"):which(names(dat)=="OtherOccupations")]) #these were the added columns for professions
# naming the occupations for the next line of code

occupations<-c(names(alloccupations))
occupations

# doing a narrow table with all the occupations, QID and case/control group

narrowoccupations<-gather(dat[,c(1,7,which(names(dat)=="dairyworkers"):which(names(dat)=="OtherOccupations"))],key= "Occupation", value = "Total", 3:17)

#removing 0 values, not important for the table
narrowoccupations<-narrowoccupations[narrowoccupations$Total==-1,]


#mixed Dairy and Dry stock - to check on people that work on mixed livestock farming - dairy and dry stock
dat$DairyDryStock<-ifelse(alloccupations$dairyworkers&alloccupations$drystockworkers,-1,-0)

# sheep/beef workers - create a variable for people that work with both sheep and cattle dry stock 
dat$MixedDryStockWorkers<-ifelse(alloccupations$beefcattleworkers&alloccupations$sheepworkers,-1,0)
dat$MixedDryStockWorkers
ctab(as.factor(dat$MixedDryStockWorkers), as.factor(dat$Type))

#check people that are undefined dry stock workers
dat[dat$drystockworkers==-1&dat$beefcattleworkers==0&dat$sheepworkers==0,c(120+3*(0:8),c(149+4*(0:19),233+8*(0:4)),112:114)]

#change the status of these undefined drystock workers to mixed dry stock workers. There is evidence they work with beef and sheep.
dat[dat$drystockworkers==-1&dat$beefcattleworkers==0&dat$sheepworkers==0&dat$JobBeforeLepto2=="Dry stock farmer"&!is.na(dat$JobBeforeLepto2),match("MixedDryStockWorkers",names(dat))]<- -1

dat[dat$drystockworkers==-1&dat$beefcattleworkers==0&dat$JobBeforeLepto2=="Dry stock farmer"&!is.na(dat$JobBeforeLepto2),match(c("beefcattleworkers"),names(dat))]<- -1


dat[dat$drystockworkers==-1&dat$beefcattleworkers==0&dat$sheepworkers==0&dat$JobBeforeLepto2=="Dry stock farmer"&!is.na(dat$JobBeforeLepto2),match(c("sheepworkers"),names(dat))]<- -1

occupations<-append(occupations, c("OtherOccupations","DairyDryStock", "MixedDryStockWorkers"), after = 15)
occupations

##  2X 2 contingency tables 
comparison<-(tibble(1:17))
for (i in 1:17){
  comparison<-ctab(as.factor(dat[,which(names(dat)==occupations[i])]),as.factor(dat$Type))
  print(occupations[i])
  print(comparison)
}

#table with occupations for cases against controls
ctab(as.factor(narrowoccupations$Occupation), as.factor(narrowoccupations$Type))

#Vector with the occupation variables, used further in the M.V. models: 
occupationvector<-grep("workers|jobless|OtherOccupations|DairyDryStock",names(dat),ignore.case = T)

############################EXPOSURES#######

#Throughout this study I am going to do many loops and other functions and these will be done individually for each group of exposure variables. I will create vectors to identify the groups of exposure variables in this study, Athough you can see that they are obtained by different methods below, they have the same essence, i,e., a vector with the identification of the column from the database.

contactcolumns<-c(120+3*(0:8))
contactdayscolumns<-contactcolumns+1
vacccolumns<-c(152+4*(0:19),236+8*(0:4))
activitiescolumns<-c(149+4*(0:19),233+8*(0:4))
livestockPPEcolumns<-match("PPEGloves",names(dat)):match("PPEOthersSpecify",names(dat))
wildevidencecolumns<-which(grepl("Evidence",names(dat))&!grepl("None|Specify|WildAnimals",names(dat)))
petscolumns<-c((407+3*(0:6)),429+3*(0:6))
waterandtreatmentcolumns<-grep("Bore|Creek|WaterOther|Rain|Tanker|WaterTown", names(dat))
treatmentcolumns<-grep("Treated", names(dat))
worktreatmentcolumns<-names(dat[492+2*(0:6)])
hometreatmentcolumns<-names(dat[477+2*(0:6)])
worksourcecolumns<-which(grepl("WorkWater", names(dat))&!grepl("Other|Unsure|Comments|Treated", names(dat)))
homesourcecolumns<-which(grepl("Water", names(dat))&!grepl("Other|Unsure|Comments|Treated|Work|Activity|Flood|Effluent|Mud|Drainag|Wetland|Landsc|Lepto|Contact|Access|Situation|Troughs", names(dat)))
waterexposurecolumns<-c(520+3*(0:6))
recreationalcolumns<-c(511+2*(0:3),match(c("WaterContactOcean","WaterContactRiver","WaterContactOthers"),names(dat)))



#####################ANIMAL EXPOSURES#####
#################################LIVESTOCK#EXPOSURES###########
#####################################################Contact with Livestock#############

#Having a look at the open-ended question - other livestock animal in contact
dat$ContactOthersDetails[dat$ContactOthers!=0]

#many answers for poultry - let's create a variable ContactPoultry

dat$ContactPoultry<-ifelse(grepl("chicken|poultry",dat$ContactOthersDetails),-1,0)
dat$ContactPoultryDays<-ifelse(grepl("chicken|poultry",dat$ContactOthersDetails),dat$ContactOthersDays,0)
dat$ContactPoultryType<-factor(ifelse(grepl("chicken|poultry",dat$ContactOthersDetails),as.character(dat$ContactOthersType), NA))
#now let's remove all the respondents for contact poultry from variable contactothers
dat$ContactOthers[dat$ContactPoultry==-1]<-0

#I have checked that many people answered contact with poultry animals in the pet section. I am going to consider that as livestock, not pets. 
dat[grepl("chicken",dat$PetOthersSpecify, ignore.case = T),c(7,petscolumns,contactcolumns,which(names(dat)=="PetOthersSpecify"))]
dat[grepl("chicken",dat$PetOthersSpecify, ignore.case = T),match("ContactPoultry",names(dat))]<--1
dat[grepl("chicken",dat$PetOthersSpecify, ignore.case = T),match("ContactPoultryType",names(dat))]<-"O"
dat$ContactPoultryType
dat[grepl("chicken",dat$PetOthersSpecify, ignore.case = T),match("ContactPoultryType",names(dat))]
dat[dat$PetOthersSpecify %in%  c("Chickens","chickens","chicken") ,match("ContactPoultryDays",names(dat))]<- dat[dat$PetOthersSpecify %in%  c("Chickens","chickens","chicken") ,match("PetOthersDays",names(dat))]



#same for pigs. change them to livestock
dat[grepl("pig",dat$PetOthersSpecify, ignore.case = T),c(7,petscolumns,contactcolumns,which(names(dat)=="PetOthersSpecify"))]
dat[grepl("pig",dat$PetOthersSpecify, ignore.case = T),match("ContactPigs",names(dat))]<- -1
dat[grepl("pig",dat$PetOthersSpecify, ignore.case = T),match("ContactPigsType",names(dat))]<-"O"
dat[grepl("pig",dat$PetOthersSpecify, ignore.case = T),match("ContactPigsDays",names(dat))]<-dat[grepl("pig",dat$PetOthersSpecify, ignore.case = T),match("PetOthersDays",names(dat))]

#check sheep also in pets section
dat[grepl("lamb",dat$PetOthersSpecify, ignore.case = T),c(7,petscolumns,contactcolumns,which(names(dat)=="PetOthersSpecify"))]
dat[grepl("lamb|sheep",dat$PetOthersSpecify, ignore.case = T),match("ContactSheep",names(dat))]
dat[grepl("lamb|Sheep",dat$PetOthersSpecify, ignore.case = T),match("ContactSheepType",names(dat))]
dat[grepl("lamb",dat$PetOthersSpecify, ignore.case = T),match("ContactSheepType",names(dat))]<-"B"


#Creating a variable for contact with any cattle

dat$ContactCattle<-ifelse(dat$ContactDairy==-1|dat$ContactBeef==-1,-1,0)
#Creating Variable contact Cattle Days
dat$ContactCattleDays<-ifelse(dat$ContactDairyDays<999&!is.na(dat$ContactDairyDays),dat$ContactDairyDays,0)+ifelse(dat$ContactBeefDays<999&!is.na(dat$ContactBeefDays),dat$ContactBeefDays,0)


###the contact with other animals variables includes one observation for contact with exporting Heifers. That would not be considered other animals. that obervation was removed. Cat shouldn't be here either. there is one variable for pet cats and one variable for wild cats. then I will eliminate the number of days for these activities.

dat[which(dat$ContactOthersDetails=="Export heifers to China"),144]<-0
dat[which(dat$ContactOthersDetails=="cat"),144]<-0
dat[144]
dat[dat$ContactOthersDays>0&dat$ContactOthers==0&!is.na(dat$ContactOthersDays)&!dat$ContactOthersDays==999,which(names(dat)=="ContactOthersDays")]<-0

#The vector for contact with livestock has to be redefined with the new variables poultry and cattle
names(dat[120+3*(0:8)])
contactcolumns<-c(120+3*(0:8),which(names(dat)=="ContactPoultry"),which(names(dat)=="ContactCattle"))


#Now we have to change all the Nas, 99s and 999s to 0s
for (i in (contactcolumns)){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0}

##creating variable for contact with any farm animal
dat$contactwithfarm<-ifelse(dat$ContactDairy==-1|dat$ContactBeef==-1|dat$ContactSheep==-1|dat$ContactPigs==-1|
                              dat$ContactGoats==-1|dat$ContactDeer==-1|dat$ContactAlpacas==-1|dat$ContactHorses==-1|
                              dat$ContactOthers==-1|dat$ContactPoultry==-1,-1,0)
#Number of participants in contact with any livestock
sum(dat$contactwithfarm)

ctab(as.factor(dat$contactwithfarm), as.factor(dat$Type))
test.fisher<-fisher.test(as.factor(dat$contactwithfarm), as.factor(dat$Type))
test.fisher


#contingency tables
#creating comparison tables for animal contact especies and group
comparison<-tibble(1,2,3,4,5,6,7,8)
for (i in (contactcolumns)){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}



#Starting to prepare a univariable O.R. for each species
anygroup<-(c("Anycontact",as.numeric(test.fisher$estimate[1]),as.numeric(test.fisher$conf.int[1]),as.numeric(test.fisher$conf.int[2])))

anygroup

comparisonanimals<-as.data.frame(matrix(ncol=4,nrow=length(contactcolumns)))
names(comparisonanimals)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonanimals
comparisonanimals$AnimalGroup<-names(dat[contactcolumns])
comparisonanimals<-rbind(comparisonanimals,anygroup)
comparisonanimals
#fisher tests with the confidence intervals
for (i in 1:length(contactcolumns)){
  comparisonanimals[i,1]<-names(dat[contactcolumns[i]])
  comparisonanimals[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,contactcolumns[i]]),dat$Type)$table)$estimate)
  comparisonanimals[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,contactcolumns[i]]),dat$Type)$table)$conf.int[1])
  comparisonanimals[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,contactcolumns[i]]),dat$Type)$table)$conf.int[2])
}

#The crude O.R. is done
comparisonanimals

#To draw a forestplot the values have to be numerical:
str( comparisonanimals)
for (i in 2:4){ comparisonanimals[,i]<-as.numeric(comparisonanimals[,i])}


#########drawing a forest plot with the crude ORs and confidence intervals
forestplot(labeltext = comparisonanimals$AnimalGroup, mean=comparisonanimals$fisherOR,lower=comparisonanimals$Min.conf.int, upper=comparisonanimals$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           is.summary = c(rep(F,11),T), 
           boxsize = 1/exp(abs(log(comparisonanimals$Max.conf.int)-(log(comparisonanimals$Min.conf.int)))),
           clip = c(0,30), title="Livestock contact risk for leptospirosis notifications")

####################################Contact with livestock at work/Outside ############
###EXploring information about contact at work or outside
workoroutsidecolumns<-(contactcolumns+2)[-match("ContactCattle",names(dat[contactcolumns]) )]

uniqueanswercontact<-list(0)

#List of participants who had contact at work or outside
for (i in 1:length(workoroutsidecolumns)) { 
  print(summary(dat[workoroutsidecolumns[i]]))
  
}
#contingency tables for cases and controls
for (i in 1:length(workoroutsidecolumns)) { 
  print(names(dat[workoroutsidecolumns[i]]))
  print(ctab(factor(dat[[workoroutsidecolumns[i]]]),dat$Type)) }

#Descriptive statistics for these variables
uniqueanswercontact<-(factor(c(sapply(uniqueanswercontact,function(x)paste(x,collapse = "")))))
summary(uniqueanswercontact)
uniqueanswercontact[grepl("OW|WO|WBO|BO",uniqueanswercontact)]<-"B"
uniqueanswercontact[uniqueanswercontact==""]<-NA
uniqueanswercontact
dat$UniqueAnswerWorkContact<-factor(uniqueanswercontact)
ctab(dat$UniqueAnswerWorkContact,dat$Type)

fisher.test(ctab(dat$UniqueAnswerWorkContact,dat$Type)$table)

#Now considering those that had any contact with livestock at work ( at work or both at work and home) one category
workcontingency<-ctab(dat$UniqueAnswerWorkContact,dat$Type)$table
workcontingency<-spread(data.frame(workcontingency),key = Var2,value = Freq)
rownames(workcontingency)<-workcontingency[[1]]
workcontingency<-workcontingency[-1]
workcontingency
workcontingency<-rbind(c(workcontingency[1,]+workcontingency[3,]),c(workcontingency[2,]))
rownames(workcontingency)<-c("Work","Not at work")
workcontingency<-data.frame(workcontingency)
fisher.test(apply(workcontingency,1,function(x)as.numeric(x)))
workcontingency
















###############################################Contact with livestock score####
#### Creating a score for the contact with livestock - first, the number of days in contact with livestock is used when the contact happened. Then, the results are summed to create one final value of exposure.

#transforming all Nas to 0s. With contact score, Nas can be replaced by 0s.
head(dat[contactdayscolumns])

for (i in contactdayscolumns){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}

#creating a data frame to analyse the number of days of contact and a variable with summing all the days in contact with livestock
score.livestock.contact<-as.data.frame(matrix(ncol=length(contactdayscolumns),nrow=nrow(dat)))
names(score.livestock.contact)<-names(dat[contactdayscolumns])
head(score.livestock.contact)


for (i in 1:length(contactdayscolumns)) { for (j in 1:nrow(dat)){
  score.livestock.contact[j,i]<-as.numeric(dat[j,(contactdayscolumns[i]-1)])*-as.numeric(dat[j,contactdayscolumns[i]])
}}
score.livestock.contact

score.livestock.contact$Totalscore<-apply(score.livestock.contact,1,sum, na.rm=T)
dat$LivestockContactScore<-score.livestock.contact$Totalscore
hist(dat$LivestockContactScore)
# the distribution does not seem to behave as normal distribution. I will try to apply a non-parametric test. In this case, Wilcoxon signed rank sum test
dat$LivestockContactScore[dat$Type=="Case"]
dat$LivestockContactScore[dat$Type=="Control"]
# values with no zeros: 
dat$LivestockContactScore[dat$Type=="Control"&dat$LivestockContactScore!=0]
dat$LivestockContactScore[dat$Type=="Case"&dat$LivestockContactScore!=0]
#means and medians for all values
mean(dat$LivestockContactScore[dat$Type=="Case"]) ; median(dat$LivestockContactScore[dat$Type=="Case"]) 
mean(dat$LivestockContactScore[dat$Type=="Control"]); median(dat$LivestockContactScore[dat$Type=="Control"])
#means and medians excluding the non=exposures (sum of exposure days=0)
mean(dat$LivestockContactScore[dat$Type=="Case"&dat$LivestockContactScore!=0]); median(dat$LivestockContactScore[dat$Type=="Case"&dat$LivestockContactScore!=0])
mean(dat$LivestockContactScore[dat$Type=="Control"&dat$LivestockContactScore!=0]);median(dat$LivestockContactScore[dat$Type=="Control"&dat$LivestockContactScore!=0])

#the mean and median were quite different from Case to control
#wilcoxon test: 
wilcox.test(dat$LivestockContactScore[dat$Type=="Case"], dat$LivestockContactScore[dat$Type=="Control"])
# the wilcoxon signed rank test ( Mann-Whitney) showed signifficance for difference in means.
wilcox.test(dat$LivestockContactScore[dat$Type=="Case"&dat$LivestockContactScore!=0], dat$LivestockContactScore[dat$Type=="Control"&dat$LivestockContactScore!=0])
#even eliminating the people that did not have any contact with livestock from the test (only comparing people that had contact with livestock), the test was signifficant.

#boxplot including 0s
boxplot(dat$LivestockContactScore[dat$Type=="Case"], dat$LivestockContactScore[dat$Type=="Control"])
#boxplot excluding 0s
boxplot(dat$LivestockContactScore[dat$Type=="Case"&dat$LivestockContactScore!=0], dat$LivestockContactScore[dat$Type=="Control"&dat$LivestockContactScore!=0])


ggplot(dat,aes(x=Type,y=LivestockContactScore)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Livestock contact score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=20)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#transforming the table to a narrow table
score.livestock.contact<-cbind(dat[7],score.livestock.contact)
narrow.score.livestock.contact<-gather(score.livestock.contact,"Group","Score",2:length(score.livestock.contact))
head(narrow.score.livestock.contact)
dim(narrow.score.livestock.contact)

#Transforming all NAs to 0s. If error returns, it means that no corrections have to be made.
narrow.score.livestock.contact[is.na(narrow.score.livestock.contact$Score),]$Score<-0
#narrow.score.livestock.contact<-narrow.score.livestock.contact[!is.na(narrow.score.livestock.contact[3]),]
dim(narrow.score.livestock.contact)
head(narrow.score.livestock.contact)

#This plot considers all cases and controls
ggplot(narrow.score.livestock.contact,aes(x=Group,y=Score, fill=Type)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Livestock contact score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=8)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#This plot considers only cases and controls that had some contact with livestock
ggplot(narrow.score.livestock.contact[!narrow.score.livestock.contact$Score==0,],aes(x=Group,y=Score, fill=Type)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Livestock contact score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=8)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

head(score.livestock.contact)

#Now I am going to test means and medians
testsforlivestock<-as.data.frame(matrix(ncol=5,nrow=ncol(score.livestock.contact)-1))

names(testsforlivestock)<-c("Meancases","Mediancases","MeanControl","MedianControl","Wilcoxon.p-value")
rownames(testsforlivestock)<-names(score.livestock.contact)[2:ncol(score.livestock.contact)]
testsforlivestock
for (i in 1:(ncol(score.livestock.contact)-1)) {
  testsforlivestock[i,5]<- ifelse(any(score.livestock.contact[dat$Type=="Case",][,i+1]>0)&any(score.livestock.contact[dat$Type=="Control",][,i+1]>0),wilcox.test(score.livestock.contact[dat$Type=="Case",][,i+1], score.livestock.contact[dat$Type=="Control",][,i+1] )$p.value, NA)
  testsforlivestock[i,1]<-mean(score.livestock.contact[dat$Type=="Case",][,i+1],na.rm = T)
  testsforlivestock[i,2]<-median(score.livestock.contact[dat$Type=="Case",][,i+1],na.rm = T)
  testsforlivestock[i,3]<-mean(score.livestock.contact[dat$Type=="Control",][,i+1],na.rm = T)
  testsforlivestock[i,4]<-median(score.livestock.contact[dat$Type=="Control",][,i+1],na.rm = T) 
}


testsforlivestock



#same test excluding 0s. 
testsforlivestock.2<-as.data.frame(matrix(ncol=7,nrow=ncol(score.livestock.contact)-1))
names(testsforlivestock.2)<-c("Meancases","Mediancases","MeanControl","MedianControl","Wilcoxon.p-value","MeanTotal", "MedianTotal")
rownames(testsforlivestock.2)<-names(score.livestock.contact)[2:ncol(score.livestock.contact)]
testsforlivestock.2
for (i in 1:(ncol(score.livestock.contact)-1)) {
  testsforlivestock.2[i,5]<- ifelse(any(score.livestock.contact[dat$Type=="Case"&score.livestock.contact[i+1]>0,][,i+1]>0)&any(score.livestock.contact[dat$Type=="Control"&score.livestock.contact[i+1]>0,][,i+1]>0),wilcox.test(score.livestock.contact[dat$Type=="Case"&score.livestock.contact[i+1]>0,][,i+1], score.livestock.contact[dat$Type=="Control"&score.livestock.contact[i+1]>0,][,i+1] )$p.value, NA)
  testsforlivestock.2[i,1]<-mean(score.livestock.contact[dat$Type=="Case"&score.livestock.contact[i+1]>0,][,i+1],na.rm = T)
  testsforlivestock.2[i,2]<-median(score.livestock.contact[dat$Type=="Case"&score.livestock.contact[i+1]>0,][,i+1],na.rm = T)
  testsforlivestock.2[i,3]<-mean(score.livestock.contact[dat$Type=="Control"&score.livestock.contact[i+1]>0,][,i+1],na.rm = T)
  testsforlivestock.2[i,4]<-median(score.livestock.contact[dat$Type=="Control"&score.livestock.contact[i+1]>0,][,i+1],na.rm = T) 
  testsforlivestock.2[i,6]<-mean(score.livestock.contact[score.livestock.contact[i+1]>0,][,i+1],na.rm = T) 
  testsforlivestock.2[i,7]<-median(score.livestock.contact[score.livestock.contact[i+1]>0,][,i+1],na.rm = T) 
}

testsforlivestock.2

#median and mean number of days in contact by species - participant - excluding those that did not have contact.
#All
mean(dat[,contactdayscolumns][!dat[,contactdayscolumns]==0])
median(dat[,contactdayscolumns][!dat[,contactdayscolumns]==0])
#cases
mean(dat[dat$Type=="Case",contactdayscolumns][!dat[dat$Type=="Case",contactdayscolumns]==0])
median(dat[dat$Type=="Case",contactdayscolumns][!dat[dat$Type=="Case",contactdayscolumns]==0])
#controls
mean(dat[dat$Type=="Control",contactdayscolumns][!dat[dat$Type=="Control",contactdayscolumns]==0])
median(dat[dat$Type=="Control",contactdayscolumns][!dat[dat$Type=="Control",contactdayscolumns]==0])

wilcox.test((dat[dat$Type=="Case",contactdayscolumns][!dat[dat$Type=="Case",contactdayscolumns]==0]),(dat[dat$Type=="Control",contactdayscolumns][!dat[dat$Type=="Control",contactdayscolumns]==0]))


###########################################Vaccination of livestock animals##########################
#This section analyses vaccination but it also creates a dataframe that will be used for the livestock activities. I categorized all the open-ended answers according to animal species and type of activity.

#which variables are related to vacination?
names(dat[c(152+4*(0:19),236+8*(0:4))]) #names of the vaccination variables
vacccolumns<-c(152+4*(0:19),236+8*(0:4))
names(dat[activitiescolumns])

#removing NAs, 99s and 999s from vaccination coloumns
for (i in vacccolumns){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}

#############################################################some corrections of the database: 
#_____________________________________________________________________________________________________________________
#changing cattle to dairy (the respondent previously specified only contact with dairy cattle)

levels(dat$ActivityAnyOtherSpecify)<- c(levels(dat$ActivityAnyOtherSpecify),"Picking up plastics and garbages between urine and faeces, vaccinate dairy")

dat[dat$ActivityAnyOtherSpecify=="Picking up plastics and garbages between urine and feaces, vaccinate cattle"& !is.na(dat$ActivityAnyOtherSpecify),which(names(dat)=="ActivityAnyOtherSpecify")]<-"Picking up plastics and garbages between urine and faeces, vaccinate dairy"


#Activityanyotherspecify column is written : "Dairy cattle trimming tail dags". It would fit better in ActivityDairyCattleDock.
dat[!is.na(dat$ActivityAnyOtherSpecify),unique(c(1,contactcolumns,activitiescolumns,c(which(names(dat)=="ActivityAnyClean"):which(names(dat)=="ActivityAnyOtherSpecify")),which(names(dat)=="JobBeforeLepto1"):which(names(dat)=="JobPartTimeSpecify")))][3,]
dat[dat$ActivityAnyOtherSpecify=="Dairy cattle trimming tail dags"&!is.na(dat$ActivityAnyOtherSpecify),grepl("ActivityDairyCattleDock",names(dat))]<-dat[dat$ActivityAnyOtherSpecify=="Dairy cattle trimming tail dags"&!is.na(dat$ActivityAnyOtherSpecify),grepl("ActivityAnyOther",names(dat))&!grepl("Specify",names(dat))]
dat$ActivityAnyOther[dat$ActivityAnyOtherSpecify=="Dairy cattle trimming tail dags"&!is.na(dat$ActivityAnyOtherSpecify)]<-0

#This activity is probably cattle - not possible to identify between beef or dairy
dat[!is.na(dat$ActivityAnyOtherSpecify),unique(c(1,contactcolumns,activitiescolumns,c(which(names(dat)=="ActivityAnyClean"):which(names(dat)=="ActivityAnyOtherSpecify")),which(names(dat)=="JobBeforeLepto1"):which(names(dat)=="JobPartTimeSpecify")))][4,]

levels(dat$ActivityAnyOtherSpecify)<- c(levels(dat$ActivityAnyOtherSpecify),"artificial insemination cattle")
dat[grep("artificial insemination",dat$ActivityAnyOtherSpecify),names(dat)=="ActivityAnyOtherSpecify"]<-"artificial insemination cattle"

#Respondent did not have contact with dairy cattle so it probable would be beef cattle.
dat[!is.na(dat$ActivityAnyOtherSpecify),unique(c(1,contactcolumns,activitiescolumns,c(which(names(dat)=="ActivityAnyClean"):which(names(dat)=="ActivityAnyOtherSpecify")),which(names(dat)=="JobBeforeLepto1"):which(names(dat)=="JobPartTimeSpecify")))][5,]
levels(dat$ActivityAnyOtherSpecify)<- c(levels(dat$ActivityAnyOtherSpecify),"offloading beef cattle from stock transport in my cattle yard office manager, farmer")
dat[grep("offloading cattle from stock transport in my cattle yard office manager, farmer",dat$ActivityAnyOtherSpecify),names(dat)=="ActivityAnyOtherSpecify"]<-"offloading beef cattle from stock transport in my cattle yard office manager, farmer"

# Dip x drench? should we consider same activity?
dat[!is.na(dat$ActivityAnyOtherSpecify),unique(c(1,contactcolumns,activitiescolumns,c(which(names(dat)=="ActivityAnyClean"):which(names(dat)=="ActivityAnyOtherSpecify")),which(names(dat)=="JobBeforeLepto1"):which(names(dat)=="JobPartTimeSpecify")))][9,]

# Pelt Sheep x Dress sheep - almost same activity. putting 0 in ActivityAnyOther
dat[!is.na(dat$ActivityAnyOtherSpecify),unique(c(1,contactcolumns,activitiescolumns,c(which(names(dat)=="ActivityAnyClean"):which(names(dat)=="ActivityAnyOtherSpecify")),which(names(dat)=="JobBeforeLepto1"):which(names(dat)=="JobPartTimeSpecify")))][21,]

dat[grep("Pelting sheep",dat$ActivityAnyOtherSpecify),c(which(names(dat)=="ActivityAnyOther"):which(names(dat)=="ActivityAnyOtherVacc"))]<-c(0,NA,NA,NA)

# this respondent already answered yes to many slaughtering options. There is no need to include it as an additional activity.
dat[!is.na(dat$ActivityAnyOtherSpecify),unique(c(1,contactcolumns,activitiescolumns,c(which(names(dat)=="ActivityAnyClean"):which(names(dat)=="ActivityAnyOtherSpecify")),which(names(dat)=="JobBeforeLepto1"):which(names(dat)=="JobPartTimeSpecify")))][22,]

dat[grep("Homekill slaughterman -shoots, skins, guts, butchers",dat$ActivityAnyOtherSpecify),c(which(names(dat)=="ActivityAnyOther"):which(names(dat)=="ActivityAnyOtherVacc"))]<-c(0,NA,NA,NA)

#this respondent only had contact with beef cattle, therefore I will add beef cattle to the description
dat[!is.na(dat$ActivityAnyOtherSpecify),unique(c(1,contactcolumns,activitiescolumns,c(which(names(dat)=="ActivityAnyClean"):which(names(dat)=="ActivityAnyOtherSpecify")),which(names(dat)=="JobBeforeLepto1"):which(names(dat)=="JobPartTimeSpecify")))][25,]

levels(dat$ActivityAnyOtherSpecify)<- c(levels(dat$ActivityAnyOtherSpecify),"Moving beef cattle stock")
dat[grep("Moving stock",dat$ActivityAnyOtherSpecify),names(dat)=="ActivityAnyOtherSpecify"]<-"Moving beef cattle stock"

#This respondent answered having no contact with beef or dairy cattle but then reported shifting cattle.
dat[!is.na(dat$ActivityAnyOtherSpecify),unique(c(1,contactcolumns,activitiescolumns,c(which(names(dat)=="ActivityAnyClean"):which(names(dat)=="ActivityAnyOtherSpecify")),which(names(dat)=="JobBeforeLepto1"):which(names(dat)=="JobPartTimeSpecify")))][32,]

dat[grep("Shifting cattle",dat$ActivityAnyOtherSpecify),names(dat)=="ContactCattle"]<- -1

#These are the other answers for livestock activities
dat$ActivityAnyOtherSpecify[!is.na(dat$ActivityAnyOtherSpecify)&dat$ActivityAnyOther==-1]

#____________________________________________________________________________________________________________________#

###checking for errors at the vaccination columns
vaccerrorcolumns<-data.frame(matrix(ncol = 25, nrow=nrow(dat)))
names(vaccerrorcolumns)<-names(dat[vacccolumns])
summ.error.vacc<-data.frame(matrix(ncol = 1, nrow=nrow(dat)))
names(summ.error.vacc)<-"Summ.error.vacc"
vaccerrorcolumns


#### in this for loop I am looking for questionnaires that were answered as none activity but the vaccination status was answered.
for (i in 1:nrow(dat)){ vaccerrorcolumns[i,]<-(dat[i,activitiescolumns]==0|dat[i,activitiescolumns]==999|is.na(dat[i,activitiescolumns]))& dat[i,vacccolumns]!=0}
for (i in 1:nrow(dat)){summ.error.vacc[i,]<-ifelse(any(vaccerrorcolumns[i,]==T),-1,0)}
summ.error.vacc
which(summ.error.vacc==-1)# these are the observations in which vaccination was marked but activity was not. is that expected?
dat[c(1,3,10,35,120),sort(c(vacccolumns,activitiescolumns,1,2,3))]
#

#creating a variable to summarize all the vaccinations in each animal group. that means just the respondents who had farm activity and answered about vaccination will be used on the analysis. 
vaccinatedlivestock<-data.frame(matrix(ncol = 25, nrow=nrow(dat)))
names(vaccinatedlivestock)<-names(dat[vacccolumns])
head(vaccinatedlivestock)
#The dummy data frame is ready, now going to run the loop with th
vaccinatedlivestock[is.na(vaccinatedlivestock)]<-0
head(vaccinatedlivestock)

#reducing the size of names
names(vaccinatedlivestock)<-gsub("Activity","",names(vaccinatedlivestock))
names(vaccinatedlivestock)<-gsub("Vacc","",names(vaccinatedlivestock))
head(vaccinatedlivestock)

for (i in 1:length(vacccolumns)) {
  vaccinatedlivestock[,i]<-ifelse(dat[[vacccolumns[i]-3]]==-1, (dat[[vacccolumns[i]]]),as.factor(NA))
}

head(vaccinatedlivestock)
str(vaccinatedlivestock) ##most variables are numeric or integers. I want to change to factors
for (i in 1:ncol(vaccinatedlivestock)){ vaccinatedlivestock[,i]<-as.factor(vaccinatedlivestock[,i])}

#Tables with the vaccination status for each species
tablesvaccination<-summary(vaccinatedlivestock)
tablesvaccination

#now I will add the case/control column and the participant number column to the vaccination dataframe - QID
QID<-dat$QID
types<-dat$Type
vaccinatedlivestock<-cbind(QID,types,vaccinatedlivestock)

head(vaccinatedlivestock)

#Now I am going to do a narrow dataframe with the vaccination
narrowvaccinatedlivestock<- gather(vaccinatedlivestock,"ActivitySpecies", "VaccinationStatus", 3:27, na.rm = T)
head(narrowvaccinatedlivestock)
narrowvaccinatedlivestock[narrowvaccinatedlivestock$VaccinationStatus==0,] # these values represent those activities that happened although vaccination status was not filled in

sort(narrowvaccinatedlivestock[narrowvaccinatedlivestock$VaccinationStatus==0,][,1]) # these are QID numbers
#remove those values
narrowvaccinatedlivestock<-narrowvaccinatedlivestock[-which(narrowvaccinatedlivestock$VaccinationStatus==0),]
#removing the same values for the wide table
vaccinatedlivestock[vaccinatedlivestock==0]<-NA

#I am going to remove all the rows with "Any Activity" because these values will be further categorized by species" and then merged to the final table.

narrowvaccinatedlivestock<-narrowvaccinatedlivestock[-which(grepl(pattern = "Any",x = narrowvaccinatedlivestock$ActivitySpecies)),]

#creating a column with activity name
for (i in 1:nrow(narrowvaccinatedlivestock)) {
  narrowvaccinatedlivestock$Activity[i]<-strsplit(narrowvaccinatedlivestock$ActivitySpecies,"Cattle|Sheep")[[i]][[2]]}
head(narrowvaccinatedlivestock)

#now going to create a column just with the species (just bear in mind that when I say species, not always species. Because cattle will be differentiated between Dairy and Beef).
for (i in 1:nrow(narrowvaccinatedlivestock)) {
  narrowvaccinatedlivestock$Species[i]<-strsplit(gsub('([[:upper:]])', ' \\1', narrowvaccinatedlivestock$ActivitySpecies),split = " ")[[i]][[2]]}
head(narrowvaccinatedlivestock)
#changing the order of columns
narrowvaccinatedlivestock<-narrowvaccinatedlivestock[,c(1,2,5,6,3,4)]
narrowvaccinatedlivestock$Activity<-gsub("Calve|Lamb","Calve/Lamb",narrowvaccinatedlivestock$Activity)
head(narrowvaccinatedlivestock)

###Categorizing other activities
#trying to include all the species for "other activities"
head(dat[228+8*(0:5)])
#six acitivities unrelated to species. Let`s see how the answers are organized.
head(dat[c(sort((228+(1:4)+sort(rep(8*(0:4),4)))),269)])
#listing all the other species columns
otheractiv.sp<-c(sort((228+(1:4)+sort(rep(8*(0:4),4)))),269)
str(dat[otheractiv.sp])

#Which participants had at least some activity? 
otheract<-c(0)
for (i in 1:nrow(dat)) {
  otheract[i]<-!all(is.na(dat[i,otheractiv.sp]))
} 
otheract
which(otheract==1)

dat[which(otheract==1),otheractiv.sp] # here we can see all the species on the open form 

#Now we are going to categorize these values according to their strings. "Cattle" will include activities when the differentiation between beef or dairy could not be done
vaccinatedanimalgroups<-c("Cattle","Beef", "Dairy", "Sheep","Pigs","Horses","Deer","Goats","Poultry")
animalgroupspatterns<-list(c("cattle", "calves", "calf", "cow"),c("beef"), c("dairy"), c("sheep", "lamb", "ewe"),c("pig"),c("horse"), c("deer"),c("goat"), c("chicken","chook"))
animalgroupnonpatterns<-list(c("beef cattle","dairy cattle"), c("zzz"), c("zzz"), c("zzz"), c("zzz"),c("zzz"), c("zzz"), c("zzz"), c("zzz"), c("zzz") )

#Creating a dummy dataframe
speciesvaccination<-as.data.frame(matrix(nrow=nrow(dat),ncol=ncol(dat[otheractiv.sp])))
names(speciesvaccination)<-names(dat[otheractiv.sp])
head(speciesvaccination)
dat[otheractiv.sp]


#Now we will classify each species according to the list of activities with open-ended answer for species and put it in the dummy variable
for (i in 1:nrow(dat)) { for (j in 1:ncol(dat[otheractiv.sp])){ for (k in 1:length(animalgroupspatterns) ){
  speciesvaccination[i,j]<- ifelse(is.na(speciesvaccination[i,j]), ifelse(grepl(paste(animalgroupspatterns[[k]],collapse = "|"), dat[[i,otheractiv.sp[j]]], ignore.case = T)& !grepl(paste(animalgroupnonpatterns[[k]], collapse = "|"),dat[[i,otheractiv.sp[j]]],ignore.case = T),vaccinatedanimalgroups[k],next),next)}}}

head(speciesvaccination)


otherspeciesvacc<-as.data.frame(matrix(nrow=nrow(dat),ncol=ncol(dat[otheractiv.sp])))
names(otherspeciesvacc)<-names(dat[otheractiv.sp])
head(otherspeciesvacc)


for (i in 1:nrow(dat)){for (j in 1:ncol(speciesvaccination)) {
  otherspeciesvacc[i,j]<- is.na(speciesvaccination[i,j])&!is.na(dat[i,otheractiv.sp[j]])
}  }


#checking the values that were left after the classification (what was categorized as others?).
for (i in 1:ncol(speciesvaccination)) {
  print (dat[which(otherspeciesvacc[i]==T),otheractiv.sp[i]])}


head(narrowvaccinatedlivestock)
head(speciesvaccination)
speciesvaccination<-cbind(QID,types,speciesvaccination)
narrowspeciesvaccination<-gather(speciesvaccination,"Activity", "Species", 3:ncol(speciesvaccination))
head(narrowspeciesvaccination)

#creating a vector for the column numbers from other activities vaccination
otheractiv.sp.vacc<-otheractiv.sp-(ifelse(otheractiv.sp%%4==0,4,otheractiv.sp%%4))
otheractiv.sp.vacc

#creating a dataframe for the other activities vaccination
speciesvaccination.status<-as.data.frame(matrix(nrow=nrow(dat),ncol=ncol(dat[otheractiv.sp.vacc])))
names(speciesvaccination.status)<-names(dat[otheractiv.sp.vacc])
head(speciesvaccination.status)

for ( i in 1:nrow(dat)) { for (j in 1:ncol(speciesvaccination.status)){
  speciesvaccination.status[i,j]<-dat[i,otheractiv.sp.vacc[j]]
}}

head(speciesvaccination.status)
speciesvaccination.status<-cbind(QID,types,speciesvaccination.status)
narrowspeciesvaccination.status<- gather(speciesvaccination.status,"Activity", "vaccinationStatus",3:ncol(speciesvaccination.status))
dim(narrowspeciesvaccination.status)
dim(narrowspeciesvaccination)
#These two dataframes should have the same dimensions that will allow to bind the two dataframes in one.
head(narrowspeciesvaccination)
narrowspeciesvaccination<-cbind(narrowspeciesvaccination,narrowspeciesvaccination.status$vaccinationStatus)
names(narrowspeciesvaccination)[5]<-"VaccinationStatus"
head(narrowspeciesvaccination) #although there are rows with vaccination status filled in and species as na, that is an error. there shouldn`t be vaccination status. that will be corrected when we remove the rows with NA species.
#remove rows with NA for species
narrowspeciesvaccination<-narrowspeciesvaccination[-which(is.na(narrowspeciesvaccination$Species)),]
dim(narrowspeciesvaccination)
narrowspeciesvaccination


#Make one column that blends especies and activity and remove some unnecessary characters among the strings from Activities.

for (i in 1:nrow(narrowspeciesvaccination)){
  narrowspeciesvaccination$ActivitySpecies[i]<-paste(gsub(pattern = "Specify|Activity|Sp|[0-9]", replacement = "", x=narrowspeciesvaccination$Activity[i]),narrowspeciesvaccination$Species[i],sep="")
  narrowspeciesvaccination$Activity[i]<-gsub(pattern = "Specify|Activity|Sp|[0-9]", replacement = "", x=narrowspeciesvaccination$Activity[i])
}

#eliminating the rows with vaccination status==0 
narrowspeciesvaccination[narrowspeciesvaccination$VaccinationStatus==0,]
narrowspeciesvaccination<-narrowspeciesvaccination[-which(narrowspeciesvaccination$VaccinationStatus==0),]
head(narrowspeciesvaccination)

#adjusting the order of the columns
narrowspeciesvaccination<-narrowspeciesvaccination[,c(1,2,3,4,6,5)]
head(narrowspeciesvaccination)
head(narrowvaccinatedlivestock)

#now they are ready to be merged
narrowvaccination<-rbind(narrowvaccinatedlivestock,narrowspeciesvaccination)

#the final narrow table
narrowvaccination

#There are still some NAs in the vaccination columns. Let's change them to 0s.
dat[vacccolumns][is.na(dat[vacccolumns])]<-0

# Analysing the % of respondents of vaccination that were certain or unsure about vaccination status.
summary(as.vector(apply(dat[!apply(dat[vacccolumns],1,function(x)(all(x==0))),vacccolumns],1,function(x)(all(x!=4)))))

surevaccine<-as.vector(apply(dat[!apply(dat[vacccolumns],1,function(x)(all(x==0))),vacccolumns],1,function(x)(all(x!=4))))
summary(surevaccine)
#Above, True is for those who are certain for all answers and False is for those who aren't sure for at least one of the answers. 

almostunsurevaccine<-as.vector(apply(dat[!apply(dat[vacccolumns],1,function(x)(all(x==0))),vacccolumns],1,function(x)(any(x==4)&!all(x==4|x==0))))
almostunsurevaccinetotal<-apply(dat[vacccolumns],1,function(x)(any(x==4)&any(x!=0)))
summary(almostunsurevaccine)
#???Above, True is for those who are uncertain for at least one of the answers and False is for  those that are certain for at least one answer. 

unsurevaccine<-as.vector(apply(dat[!apply(dat[vacccolumns],1,function(x)(all(x==0))),vacccolumns],1,function(x)(all(x==4|x==0))))
unsurevaccinetotal<-apply(dat[vacccolumns],1,function(x)(all(x==4|x==0)&any(x!=0)))
summary(unsurevaccine)
#???Above, True is for those who are uncertain for all the answers and False is for  those that are certain for at least one answer. 

#These fisher tests are going to test if the uncertainty or certainty are diferential between cases and controls
fisher.test(ctab(as.factor(surevaccine),dat[!apply(dat[vacccolumns],1,function(x)(all(x==0))),match("Type",names(dat))])$table)
fisher.test(ctab(as.factor(unsurevaccine),dat[!apply(dat[vacccolumns],1,function(x)(all(x==0))),match("Type",names(dat))])$table)
fisher.test(ctab(as.factor(almostunsurevaccine),dat[!apply(dat[vacccolumns],1,function(x)(all(x==0))),match("Type",names(dat))])$table)
#The tests show that there is not significant difference between cases and controls.

#vaccination Analysis
vaccxtype<-ctab(as.factor(narrowvaccination$VaccinationStatus), as.factor(narrowvaccination$types))$table

###This is the table comparing all activities in livestock and vaccination status. 
vaccxtype
### the chi square and fisher tests found no signifficant difference on the table,however, later they will be analysed through the Wilcoxon rank sum test that is a more indicated non-parametric statistical test for ordinal values (from totally vaccinated herd to not vaccinnated herd).
chisq.test(vaccxtype)
fisher.test(vaccxtype)
vaccinatedcases<-filter(narrowvaccination,types=="Case")
vaccinatedcontrols<-filter(narrowvaccination,types=="Control")
vaccinatedcontrols<-vaccinatedcontrols[-which(vaccinatedcontrols$VaccinationStatus==4),]
vaccinatedcases<-vaccinatedcases[-which(vaccinatedcases$VaccinationStatus==4),]
#The variables above were creaed to test the difference of vaccination status between cases and controls.

wilcox1<-wilcox.test(as.numeric(vaccinatedcases$VaccinationStatus),as.numeric(vaccinatedcontrols$VaccinationStatus))
wilcox1 # no signifficant difference of vaccination between cases and controls


# Table of vaccination x type of activity
vaccxactivity<-ctab(as.factor(narrowvaccination$ActivitySpecies), as.factor(narrowvaccination$VaccinationStatus))$table
vaccxactivity
#the table above considers vaccination status by species


str(vaccinatedlivestock)
for (i in 1:25) {
  vaccinatedlivestock[i]<-as.factor(vaccinatedlivestock[,i])
}
summary(vaccinatedlivestock)
#The table above considers general vaccination status not diferentiated by species


#making a table removing individual observations with all NAs in vaccinated livestock
noNAsvaccinatedlivestockc<-c(0)
for(i in 1:nrow(vaccinatedlivestock)){
  noNAsvaccinatedlivestockc[i]<-ifelse(all(is.na(vaccinatedlivestock[i,3:25])),0,-1)}
noNAsvaccinatedlivestock<-vaccinatedlivestock[noNAsvaccinatedlivestockc==-1,]
noNAsvaccinatedlivestock

# separating by animal groups
vaccinatedanimalgroups

vaccinationofanimalgroups<-list(1)
for (i in 1:9){
  vaccinationofanimalgroups[[i]]<- as.data.frame(ctab(as.factor(narrowvaccination[grepl(vaccinatedanimalgroups[i],narrowvaccination$Species, ignore.case = T),][,6]),as.factor(narrowvaccination[grepl(vaccinatedanimalgroups[i],narrowvaccination$Species, ignore.case = T),][,2]))$table)
  names(vaccinationofanimalgroups[[i]])<-c(paste(vaccinatedanimalgroups[i],"VaccinationStatus"),"Group","Count") }

vaccinationofanimalgroups




cattlenodifferentiation<-as.data.frame(ctab(as.factor(narrowvaccination[grepl(vaccinatedanimalgroups[1],narrowvaccination$Species, ignore.case = T)&!(grepl(vaccinatedanimalgroups[2],narrowvaccination$Species, ignore.case = T)|grepl(vaccinatedanimalgroups[3],narrowvaccination$Species, ignore.case = T)),][,6]),as.factor(narrowvaccination[grepl(vaccinatedanimalgroups[1],narrowvaccination$Species, ignore.case = T)&!(grepl(vaccinatedanimalgroups[2],narrowvaccination$Species, ignore.case = T)|grepl(vaccinatedanimalgroups[3],narrowvaccination$Species, ignore.case = T)),][,2]))$table)

#Here we can see the vaccination tables by species in a long format, because we will need a long table to plot.
vaccinationofanimalgroups

#this is the table in which no differentiation was made between dairy and dairy cattle activities at the moment of the questionnaire. That is, some people answered the questionnaire without identifying if cattle was beef or dairy.
cattlenodifferentiation

vaccinationofanimalgroups<-list(1)
for (i in 1:8){
  vaccinationofanimalgroups[[i]]<- spread(as.data.frame(ctab(as.factor(narrowvaccination[grepl(vaccinatedanimalgroups[i],narrowvaccination$Species, ignore.case = T),][,6]),as.factor(narrowvaccination[grepl(vaccinatedanimalgroups[i],narrowvaccination$Species, ignore.case = T),][,2]))$table),Var2,Freq) 
  names(vaccinationofanimalgroups[[i]])<-c(paste(vaccinatedanimalgroups[i],"VaccinationStatus"),"Case","Control")}
# Here we see in a wide format
vaccinationofanimalgroups

#this is the non differentiated cattle table in a wide format 
spread(cattlenodifferentiation,Var2,Freq)

# the Mann-Whittney Wilcoxon ranking test is appropriate to test the Vaccination status because they can be ranked according to an ordinal method. From 1 to 3, where 1 is fully Vaccinated and 3 is not vaccinated at all. We will test if there is a signifficant difference between cases and controls for each groupo of species.

for (i in 1:9) {
  wilcox.vacc<-wilcox.test(as.numeric(narrowvaccination[grepl(vaccinatedanimalgroups[i],narrowvaccination$Species, ignore.case = T)& narrowvaccination[,2]=="Case"& narrowvaccination[,6]!=4,6]),as.numeric(narrowvaccination[grepl(vaccinatedanimalgroups[i],narrowvaccination$Species, ignore.case = T)& narrowvaccination[,2]=="Control"& narrowvaccination[,6]!=4,6]))
  print(vaccinatedanimalgroups[i])
  print(wilcox.vacc) }

#apparently none of the Animal Groups were found to have difference between Cases and Controls for each of the species group


#### removing the dependence from the responses. Many respondents answered more than once about vaccination, so that creates dependence between the values.

#understanding how the values are distributed.
#data frame with all the respondents that answered more than once about vaccination
#these values below have been duplicated. 
repeatedobservations<-narrowvaccination[(duplicated(narrowvaccination[c(1,5)])|duplicated(narrowvaccination[c(1,5)], fromLast = T)),]
dat[c(unique(repeatedobservations$QID)),otheractiv.sp] #that is because they answered different names for the same pecies groups ("ewes and lambs" and "calf and cow"). we will remove those values. 
narrowvaccination<-narrowvaccination[!(duplicated(narrowvaccination[c(1,5)])|duplicated(narrowvaccination[c(1,5)], fromLast = T)),]
narrowvaccination

#checking for respondents that answered more than once about vaccination
morethanoneanswervaccine<-spread(narrowvaccination[c(1,2,5,6)],ActivitySpecies,VaccinationStatus)
head(morethanoneanswervaccine)
ncol(morethanoneanswervaccine)
nrow(morethanoneanswervaccine) #number of respondents who answered more than once for vaccination
morethanoneanswervaccine

repeatedvaccinations<-list(1)
for (i in 1:nrow(morethanoneanswervaccine)) {
  repeatedvaccinations[i]<-mean(as.numeric(c(t(morethanoneanswervaccine[i,3:ncol(morethanoneanswervaccine)]))),na.rm=T  )}
repeatedvaccinations

#number of answers per respondent
n.vacc.answers<-c(0)
for (i in 1:nrow(vaccinatedlivestock)) {
  n.vacc.answers[i]<-sum(!is.na(as.numeric(c(t(vaccinatedlivestock[i,3:ncol(vaccinatedlivestock)]))))&(as.numeric(c(t(vaccinatedlivestock[i,3:ncol(vaccinatedlivestock)]))))>0)}

n.vacc.answers

#table with respondents that answered different values for vaccination (when they answered more than once)
different.answers.vacc<-c(0)
for (i in 1:nrow(vaccinatedlivestock)) {
  different.answers.vacc[i]<- length(unique(c(t(vaccinatedlivestock[,3:ncol(vaccinatedlivestock)][i,])))[!is.na(unique(c(t(vaccinatedlivestock[,3:ncol(vaccinatedlivestock)][i,]))))]) }

different.answers.vacc
#respondents with different answers
vaccinatedlivestock[different.answers.vacc>1,]


#we will remove the responses with vaccination status 4 (unsure). before, we analyse these values
#number of unsure answers:
nrow(narrowvaccination[narrowvaccination$VaccinationStatus==4,]) 
#number of respondents that answered unsure
length(unique((narrowvaccination[narrowvaccination$VaccinationStatus==4,])$QID))
#distribution of respondens that answered unsure among cases and controls
table(dat[match(unique(narrowvaccination[narrowvaccination$VaccinationStatus==4,]$QID),dat[[1]]),7])
#removing 4s
narrowvaccination<-narrowvaccination[!narrowvaccination$VaccinationStatus==4,]

Widevaccinationbyspecies<-narrowvaccination[c(1,2,4,5,6)]
Widevaccinationbyspecies[order(Widevaccinationbyspecies$QID),]
Widevaccinationbyspecies<-Widevaccinationbyspecies[!(duplicated(Widevaccinationbyspecies[c(1,3,5)])&duplicated(Widevaccinationbyspecies[c(1,3,5)], fromLast = T)&duplicated(Widevaccinationbyspecies[c(1,3,5)], fromLast = T)),]
Widevaccinationbyspecies[(duplicated(Widevaccinationbyspecies[c(1,3,4)])&duplicated(Widevaccinationbyspecies[c(1,3,4)], fromLast = T)&duplicated(Widevaccinationbyspecies[c(1,3,4)], fromLast = T)),]
Widevaccinationbyspecies<-spread(Widevaccinationbyspecies, ActivitySpecies,VaccinationStatus)
for (i in 4:ncol(Widevaccinationbyspecies)) { Widevaccinationbyspecies[,i]<-as.numeric(Widevaccinationbyspecies[,i])
}
str(Widevaccinationbyspecies)
Widevaccinationbyspecies$Species<-factor(Widevaccinationbyspecies$Species, levels = c("Cattle", "Dairy","Beef","Sheep","Pigs","Goats"))
Widevaccinationbyspecies$Mean<-apply(Widevaccinationbyspecies[4:ncol(Widevaccinationbyspecies)],MARGIN = 1,FUN =function(x) mean(x,na.rm=T))

Widevaccinationbyspecies
sapply(Widevaccinationbyspecies,function(x)mean(x,na.rm = T))

ggplot(data = Widevaccinationbyspecies[!(is.na(Widevaccinationbyspecies$Mean)|is.na(Widevaccinationbyspecies$Species)),], aes(x = Species, y =Mean, fill=types))+geom_jitter(alpha=0.9,position=position_jitterdodge(),aes(colour = types))+scale_y_continuous(trans = "reverse", breaks=c(3,2,1), labels=c("Not Vaccinated","Partially Vaccinated", "Fully Vaccinated" ))+ theme(panel.grid.minor = element_blank())+labs(caption = "Leptospirosis vaccination of animals in contact with respondents within previous 30 days.\nVaccination status was recorded according to respondents'awareness.\nThe variable \"Cattle\" is shown for those participants who did not differentiate dairy and beef cattle in their answers.") + ylab("") + theme( plot.caption=element_text(hjust = c(0)))


#optional arguments to enter the plot.
# title = "Leptospirosis vaccination according to respondents"    plot.title = element_text(hjust = 0.5),
#+geom_boxplot(outlier.shape=NA,varwidth = TRUE, alpha=0.2)
####

vaccinationtable<-ctab(as.factor(narrowvaccination$Species),as.factor(narrowvaccination$VaccinationStatus))$table
vaccinationtable

#Frquency of responses of vaccination per species
vaccinationdf<-spread(as.data.frame(vaccinationtable),key = Var1,value = Freq)
vaccinationdf<-vaccinationdf[-1]
vaccinationdf

#Percentage of answers
sapply(vaccinationdf,function(x)x/(sum(x)))


##################################################Activities with livestock############################################
#removing NAs or 99s for all acitivity with farm animals binary data
names(dat[c(149+4*(0:19),233+8*(0:4))])
#I already created a vector with the column numbers. I will use sometimes this vector, sometimes the formula for the vector. (c(149+4*(0:19),233+8*(0:4)))
activitiescolumns

for (i in activitiescolumns){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}


#creating variable for " any activity with farm animals" 
AnyActivityFarm<-data.frame(matrix(nrow = nrow(dat), ncol = 1))
names(AnyActivityFarm)<-"AnyActivityFarm"
for (i in 1:nrow(dat)){
  AnyActivityFarm[i,]<- ifelse(any(dat[i,activitiescolumns]==-1),-1,0)
} 
summary(as.factor(AnyActivityFarm[[1]]))
dat$AnyActivityFarm<-AnyActivityFarm[[1]]

# contingency tables, chi-squared and fisher test . preparing the result for the forest plot
ctab(as.factor(dat$AnyActivityFarm), as.factor(dat$Type))
chisq.test(as.factor(dat$AnyActivityFarm), as.factor(dat$Type))
test.fisher2<-fisher.test(as.factor(dat$AnyActivityFarm), as.factor(dat$Type))
AnyActivity<-(c("Anyactivity",as.numeric(test.fisher2$estimate[1]),as.numeric(test.fisher2$conf.int[1]),as.numeric(test.fisher2$conf.int[2])))
AnyActivity



#creating contingency tables for animal activities and group
comparison<-tibble(0)
for (i in c(activitiescolumns)){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}


#calculating fisher test odds ratios and confidence intervals for each of the activities - inserting at the forest plot data frame

comparisonactivities<-as.data.frame(matrix(ncol=4,nrow=length(activitiescolumns)))
names(comparisonactivities)<-c("ActivityGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonactivities
comparisonactivities$ActivityGroup<-names(dat[activitiescolumns])
comparisonactivities<-rbind(comparisonactivities,AnyActivity)
comparisonactivities

#fisher tests with the confidence intervals - This loop estimates the fisher's O.R.s and confidence intervals.
for (i in 1:length(activitiescolumns)){
  tryCatch({comparisonactivities[ i,1]<-names(dat[activitiescolumns[i]])
  comparisonactivities[ i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,activitiescolumns[i]]),dat$Type)$table)$estimate)
  comparisonactivities[ i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,activitiescolumns[i]]),dat$Type)$table)$conf.int[1])
  comparisonactivities[ i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,activitiescolumns[i]]),dat$Type)$table)$conf.int[2])
}, error=function(e){}) }

#Now, change all the Ors and C.I.s to numeric values
for (i in 2:4){ comparisonactivities[,i]<-as.numeric(comparisonactivities[,i])}
comparisonactivities
#########drawing a forest plot with the ORs and confidence intervals
forestplot(labeltext = comparisonactivities$ActivityGroup, mean=comparisonactivities$fisherOR ,lower=comparisonactivities$Min.conf.int, 
           upper=comparisonactivities$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           is.summary = c(rep(F,25),T), 
           boxsize = 1/exp(abs(log(comparisonactivities$Max.conf.int)-(log(comparisonactivities$Min.conf.int)))),
           clip = c(0,65), title="Livestock activities risk of leptospirosis cases")

#the forest plot drawn has to many variables with long confidence intervals. will try to summarize by group of animals


#checking all the respondents who answered about Cattle unspecifically (no differentiation between dairy and beef catlle). 
idsfromunspecificresponses<-c(0)
for (i in 1:nrow(dat)) {
  idsfromunspecificresponses[i]<-any(dat[i,149+4*c(0:3)]==-1,na.rm = T)
  ifelse(any(dat[i,149+4*c(0:3)]==-1),print(dat[i,c(1,7,149+4*c(0:3))]),next)
  
}
dat[idsfromunspecificresponses==1,c(1,149+4*(0:19),233+8*(0:4))]
#these are the IDs who answered about cattle unspecifically.=
which(idsfromunspecificresponses==1)

##summary of activities

#activities related to dairy cattle
names(dat[149+4*c(11:14)]) 
AnyActivityDairy<-data.frame(matrix(nrow = nrow(dat), ncol = 1))
names(AnyActivityDairy)<-"AnyActivityDairy"
for (i in 1:nrow(dat)){
  AnyActivityDairy[i,]<- ifelse(any(dat[i,149+4*c(11:14)]==-1),-1,0)
} 
#Participants who had dairy cattle activity
sum(AnyActivityDairy[[1]])
dat$AnyActivityDairy<-AnyActivityDairy[[1]]

ctab(as.factor(dat$AnyActivityDairy), as.factor(dat$Type))
chisq.test(as.factor(dat$AnyActivityDairy), as.factor(dat$Type))
test.fisher11<-fisher.test(as.factor(dat$AnyActivityDairy), as.factor(dat$Type))
#Fisher's OR for any dairy activities
test.fisher11
AnyActivityDairy<-(c("AnyActivityDairy",as.numeric(test.fisher11$estimate[1]),as.numeric(test.fisher11$conf.int[1]),as.numeric(test.fisher11$conf.int[2])))
AnyActivityDairy

#activities related to dairy cattle
names(dat[149+4*c(15:18)]) 
AnyActivityBeef<-data.frame(matrix(nrow = nrow(dat), ncol = 1))
names(AnyActivityBeef)<-"AnyActivityBeef"
for (i in 1:nrow(dat)){
  AnyActivityBeef[i,]<- ifelse(any(dat[i,149+4*c(15:18)]==-1),-1,0)
} 
sum(AnyActivityBeef[[1]])
dat$AnyActivityBeef<-AnyActivityBeef[[1]]

ctab(as.factor(dat$AnyActivityBeef), as.factor(dat$Type))
chisq.test(as.factor(dat$AnyActivityBeef), as.factor(dat$Type))
test.fisher12<-fisher.test(as.factor(dat$AnyActivityBeef), as.factor(dat$Type))
test.fisher12
AnyActivityBeef<-(c("AnyActivityBeef",as.numeric(test.fisher3$estimate[1]),as.numeric(test.fisher3$conf.int[1]),as.numeric(test.fisher3$conf.int[2])))
AnyActivityBeef

#activities related to cattle - Any cattle - dairy, beef or unspecified
names(dat[149+4*c(0:3,11:18)]) 
AnyActivityCattle<-data.frame(matrix(nrow = nrow(dat), ncol = 1))
names(AnyActivityCattle)<-"AnyActivityCattle"
for (i in 1:nrow(dat)){
  AnyActivityCattle[i,]<- ifelse(any(dat[i,149+4*c(0:3,11:18)]==-1),-1,0)
} 
sum(AnyActivityCattle[[1]])
dat$AnyActivityCattle<-AnyActivityCattle[[1]]

#It is important that Any activity cattle is a reliable summary for activities with cattle, and not just the activities in which cattle was undefined. So respondents that answered for beef or dairy also have to be included in the variable

dat$AnyActivityCattle<-factor(ifelse(dat$AnyActivityCattle==-1|dat$AnyActivityBeef==-1|dat$AnyActivityDairy==-1,-1,0),levels=c(-1,0))


ctab(as.factor(dat$AnyActivityCattle), as.factor(dat$Type))
chisq.test(as.factor(dat$AnyActivityCattle), as.factor(dat$Type))
test.fisher3<-fisher.test(as.factor(dat$AnyActivityCattle), as.factor(dat$Type))
test.fisher3
AnyActivityCattle<-(c("AnyActivityCattle",as.numeric(test.fisher3$estimate[1]),as.numeric(test.fisher3$conf.int[1]),as.numeric(test.fisher3$conf.int[2])))
AnyActivityCattle




#activities related to sheep
names(dat[149+4*(4:10)]) 
AnyActivitySheep<-data.frame(matrix(nrow = nrow(dat), ncol = 1))
names(AnyActivitySheep)<-"AnyActivitySheep"
for (i in 1:nrow(dat)){
  AnyActivitySheep[i,]<- ifelse(any(dat[i,149+4*(4:10)]==-1),-1,0)
} 
sum(AnyActivitySheep[[1]])
dat$AnyActivitySheep<-AnyActivitySheep[[1]]


ctab(as.factor(dat$AnyActivitySheep), as.factor(dat$Type))
chisq.test(as.factor(dat$AnyActivitySheep), as.factor(dat$Type))
test.fisher4<-fisher.test(as.factor(dat$AnyActivitySheep), as.factor(dat$Type))
test.fisher4
AnyActivitySheep<-(c("AnyActivitySheep",as.numeric(test.fisher4$estimate[1]),as.numeric(test.fisher4$conf.int[1]),as.numeric(test.fisher4$conf.int[2])))
AnyActivitySheep 

#now I will categorize the other activities according to species. I have already made a table with the categorization of species according to vaccination ("speciesvaccination"). I will use the same table. 
head(speciesvaccination)

names(dat[activitiescolumns])
otheractivitiescolumns<-sort(rep(activitiescolumns[20:25],4))
otheractivitiescolumns<-otheractivitiescolumns[1:(length(otheractivitiescolumns)-3)]
otheractivitiescolumns
head(dat[otheractivitiescolumns])

otheractivitiesspecies<-data.frame(matrix(nrow = nrow(dat), ncol=(ncol(speciesvaccination)-2)))
names(otheractivitiesspecies)<-names(speciesvaccination)[-c(1,2)]
names(otheractivitiesspecies)

for (i in 1:nrow(dat)){ for (j in 1:length(otheractivitiescolumns)) {
  otheractivitiesspecies[i,j]<-ifelse(dat[otheractivitiescolumns][i,j]==-1,speciesvaccination[i,j+2],0) }}
head(otheractivitiesspecies)

#the table is done. Now what we will do is to create variables for any activities with each species.

vaccinatedanimalgroups
#we will use the same vector as we used with livestock vaccination

AnyActivitywithspecies<-data.frame(matrix(ncol=length(vaccinatedanimalgroups), nrow = nrow(dat)))
names(AnyActivitywithspecies)<-vaccinatedanimalgroups
head(AnyActivitywithspecies)
for(i  in 1:nrow(dat)){ for (j in 1:length(vaccinatedanimalgroups)){
  AnyActivitywithspecies[i,j]<-ifelse(any(otheractivitiesspecies[i,]==vaccinatedanimalgroups[j]),-1,0) }}
head(AnyActivitywithspecies)
AnyActivitywithspecies[is.na(AnyActivitywithspecies)]<-0
AnyActivitywithspecies
#these 4columns: "dat$AnyActivityCattle" and "dat$AnyActivitySheep" , "dat$AnyActivityBeef" , "dat$AnyActivityDairy" have already been created from the previous activities. What we will do now is to join these columns to the table we have just created and use the "or" command to do that. that way we can capture any activity from each livestock group.
dat$AnyActivityCattle<-ifelse(dat$AnyActivityCattle==-1|AnyActivitywithspecies$Cattle==-1,-1,0)
dat$AnyActivityDairy<-ifelse(dat$AnyActivityDairy==-1|AnyActivitywithspecies$Dairy==-1,-1,0)
dat$AnyActivityBeef<-ifelse(dat$AnyActivityBeef==-1|AnyActivitywithspecies$Beef==-1,-1,0)
dat$AnyActivityCattle<-ifelse(dat$AnyActivityCattle==-1|AnyActivitywithspecies$Cattle==-1,-1,0)

#now, add the other species to the main database
AnyActivitywithspeciesnames<-c(0)
for (i in 5:ncol(AnyActivitywithspecies)) { AnyActivitywithspeciesnames[i-4]<-paste0("AnyActivity",names(AnyActivitywithspecies)[i],sep ="") }
AnyActivitywithspeciesnames

names(AnyActivitywithspecies)[5:ncol(AnyActivitywithspecies)]<-AnyActivitywithspeciesnames
head(AnyActivitywithspecies)
dat<-cbind(dat,AnyActivitywithspecies[5:ncol(AnyActivitywithspecies)])

#creating a data frame for comparing crude ORs and confidence intervals between animal gorups. 
summ.activities<-as.data.frame(matrix(ncol=4,nrow=length(vaccinatedanimalgroups)+1))
names(summ.activities)<-c("ActivityGroup","fisherOR","Min.conf.int.","Max.conf.int")
summ.activities

#Now we create a final variable with the Fisher OR and CIs
f.plotactivitynames<-c(vaccinatedanimalgroups, "Farm")
for (i in 1:length(f.plotactivitynames)) { 
  summ.activities[i,2]<- fisher.test(ctab(as.factor(dat[,which(names(dat)==paste("AnyActivity",f.plotactivitynames, sep = "")[i])]),dat$Type)$table)$estimate
  summ.activities[i,3]<- fisher.test(ctab(as.factor(dat[,which(names(dat)==paste("AnyActivity",f.plotactivitynames, sep = "")[i])]),dat$Type)$table)$conf.int[1]
  summ.activities[i,4]<- fisher.test(ctab(as.factor(dat[,which(names(dat)==paste("AnyActivity",f.plotactivitynames, sep = "")[i])]),dat$Type)$table)$conf.int[2]
  summ.activities[i,1]<-paste("AnyActivity",f.plotactivitynames, sep = "")[i]
}
summ.activities

forestplot(labeltext = summ.activities$ActivityGroup, mean=summ.activities$fisherOR ,lower=summ.activities$Min.conf.int, 
           upper=summ.activities$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           is.summary = c(rep(F,9),T), 
           boxsize = 1/exp(abs(log(summ.activities$Max.conf.int)-(log(summ.activities$Min.conf.int)))),
           clip = c(0,65), title="Livestock activities risk of leptospirosis cases")


###Merging the activities from different species into one variable. For example: drenching dairy cattle and drenching sheep becomes Anydrenching
names(dat[activitiescolumns])

#first I create a list with patterns for each activity. The last vector is for the undesired patterns in the name selection. 
activitynames<-list(c("AnyAssistBirth", "Calve","Lamb"), c("AnyMilk", "Milk") , c("AnyCrutch.Dag", "Crutch"), c("AnyCastrate.Dock", "Dock"), c("AnyShear", "Shear"), c("AnyCleanUrineFaeces", "AnyClean"), c("AnySlaughterActivity", "Slaughter" ,"Homekill", "Welfare", "Dress"),c("AnyDrench", "Drench"), c("AnyOtherActivity", "ActivityAnyOther"),c("Days", "Sp","Vacc", "Type", "Raw", "Health")) 
for (i in 1:(length(activitynames)-1) ) {
  print(names(dat[grepl(paste(activitynames[[i]][2:length(activitynames[[i]])],collapse = "|"),names(dat))&!grepl(paste(activitynames[[length(activitynames)]],collapse = "|"),names(dat))]))}

for (i in 1:(length(activitynames)-1) ) {
  print(activitynames[[i]][1])
  print(which(grepl(paste(activitynames[[i]][2:length(activitynames[[i]])],collapse = "|"),names(dat))&!grepl(paste(activitynames[[length(activitynames)]],collapse = "|"),names(dat))))}

activitygrouplist<-list(0)
for (i in 1:(length(activitynames)-1) ) {
  
  activitygrouplist[[i]]<-c(which(grepl(paste(activitynames[[i]][2:length(activitynames[[i]])],collapse = "|"),names(dat))&!grepl(paste(activitynames[[length(activitynames)]],collapse = "|"),names(dat))))
  names(activitygrouplist)[[i]]<-activitynames[[i]][1]
}
#Column number identification from each type of activity:
activitygrouplist 

#Now I m going to create a data frame with the new activity summary variables (for example: any drench, any assist birth, etc) 
activitygrouptable<-data.frame(matrix(nrow = nrow(dat),ncol = length(activitygrouplist)))
names(activitygrouptable)<-names(activitygrouplist)
for( i in 1:nrow(dat)) { for (j in 1:length(activitygrouplist)) {
  activitygrouptable[i,j]<-ifelse(any(dat[i,activitygrouplist[[j]]]==-1),-1,0) }}
head(activitygrouptable)
names(activitygrouptable)
dat<-cbind(dat,activitygrouptable)
activitygroupcolumns<-grep(paste(names(activitygrouptable), collapse = "|"), names(dat))
activitygroupcolumns

#I will integrate skinning animals to the slaughter activities.
dat$AnySlaughterActivity<-factor(ifelse(dat$SkinAnimals==-1|dat$AnySlaughterActivity==-1,-1,-0))
dat$AnySlaughterActivity
#One NA ( Because it should be a NA for skinning animals) 
dat$AnySlaughterActivity[is.na(dat$AnySlaughterActivity)]<-0

#Now doing a Fisher O.R. table with the activity summary variables
comparisonactivitygroup<-as.data.frame(matrix(ncol=4,nrow=length(activitygroupcolumns)))
names(comparisonactivitygroup)<-c("ActivityGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonactivitygroup
comparisonactivitygroup$ActivityGroup<-names(dat[activitygroupcolumns])
comparisonactivitygroup<-rbind(comparisonactivitygroup,AnyActivity)
comparisonactivitygroup


for (i  in 1:length(activitygroupcolumns)) { 
  comparisonactivitygroup[i,1]<-names(dat[activitygroupcolumns[i]])
  comparisonactivitygroup[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,activitygroupcolumns[i]]),dat$Type)$table)$estimate)
  comparisonactivitygroup[ i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,activitygroupcolumns[i]]),dat$Type)$table)$conf.int[1])
  comparisonactivitygroup[ i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,activitygroupcolumns[i]]),dat$Type)$table)$conf.int[2])
}
comparisonactivitygroup
for (i in 2:4){ comparisonactivitygroup[,i]<-as.numeric(comparisonactivitygroup[,i])}
str(comparisonactivitygroup)

#########drawing a forest plot with the ORs and confidence intervals
forestplot(labeltext = comparisonactivitygroup$ActivityGroup, mean=comparisonactivitygroup$fisherOR ,lower=comparisonactivitygroup$Min.conf.int, 
           upper=comparisonactivitygroup$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           is.summary = c(rep(F,nrow(comparisonactivitygroup)-1),T), 
           boxsize = 1/exp(abs(log(comparisonactivitygroup$Max.conf.int)-(log(comparisonactivitygroup$Min.conf.int)))),
           clip = c(0,65), title="Livestock activities risk of leptospirosis cases")

#This vector below will be used as group of variables for the MV logistic regression further.
glmactivitycolumns<-which(grepl("AnyActivity",names(dat))&!grepl("AnyActivitytraps|AnyActivityPets",names(dat)))

#This is also an experimental variable for MV logistic regression
dat$BeeforDeerActivities<-dat$AnyActivityBeef==-1|dat$AnyActivityDeer==-1

##################################################Livestock Activity scores#####
head(dat[activitiescolumns])
head(dat[activitiescolumns+1])

#Changing Nas, 99s and 999s to 0s
for (i in c(activitiescolumns,activitiescolumns+1)){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}

# Creating a dataframe to summarize the farm activity
score.FarmActivity<-as.data.frame(matrix(ncol=length(activitiescolumns),nrow=nrow(dat)))
names(score.FarmActivity)<-names(dat[activitiescolumns])
head(score.FarmActivity)

#Copying the number of days of activities in the new dataframe. If the dichotomous variable for the activity was 0, then the number of days was removed from the new dataframe 
for (i in 1:length(activitiescolumns)) { for (j in 1:nrow(dat)){
  score.FarmActivity[j,i]<-as.numeric(dat[j,activitiescolumns[i]])*-as.numeric(dat[j,activitiescolumns[i]+1])
}}
head(score.FarmActivity)

#The total score for farm activity is the sum of days in all activities with livestock: 
score.FarmActivity$Totalscore<-apply(score.FarmActivity,1,sum, na.rm=T)
dat$FarmActivityScore<-score.FarmActivity$Totalscore

#transforming the table to a narrow table
score.FarmActivity<-cbind(QID, types ,score.FarmActivity)
head(score.FarmActivity)
narrow.score.FarmActivity<-gather(score.FarmActivity,"Group","Score",3:ncol(score.FarmActivity))
head(narrow.score.FarmActivity)
dim(narrow.score.FarmActivity)

## the table which was created in the vaccination status will be used again. that table was used to apply the species to each of the extra activities.We will use it to categorize the number of days of "other activities ( usually started by "ActivityAny") into species. so we will start by using the "speciesvaccination" dataframe which is ready. 

head(speciesvaccination) # all species are ready. Now we need to do another table with the scores from each activity




#creating a specific narrow table for the merge.
narrowspeciesdays<-gather(speciesvaccination,key = "Activity",value = "Species", 3:ncol(speciesvaccination))
head(narrowspeciesdays)

#creating a vector for the column numbers from other activities days
names(dat[otheractiv.sp])
otheractiv.sp.days<-otheractiv.sp-(ifelse(otheractiv.sp%%4==0,6,otheractiv.sp%%4+2))
names(dat[otheractiv.sp.days]) # the same column names from [otheractiv.sp], except that now we are dealing with number of days and not species.

#creating a dataframe for the other activities days. we will use the otheractiv.sp.days vector to recall the column numbers.
speciesdays.status<-as.data.frame(matrix(nrow=nrow(dat),ncol=ncol(dat[otheractiv.sp.days])))
names(speciesdays.status)<-names(dat[otheractiv.sp.days])
head(speciesdays.status)
#dummy dataframe is ready. Now we insert the desired information.

for ( i in 1:nrow(dat)) { for (j in 1:ncol(speciesdays.status)){
  speciesdays.status[i,j]<-dat[i,otheractiv.sp.days[j]]
}}

head(speciesdays.status) # information inserted. Next step is to add the group type and QID number columns to identify each row. 
speciesdays.status<-cbind(QID,types,speciesdays.status) #done. Now we transform it into a narrow table.

narrowspeciesdays.status<- gather(speciesdays.status,"Activity", "Days",3:ncol(speciesdays.status))
dim(narrowspeciesdays.status) 
head(narrowspeciesdays.status)

#taking the dataframe narrowspeciesdays we created previously, so that we can insert the species column.
dim(narrowspeciesdays)
head(narrowspeciesdays)
head(narrowspeciesdays.status)
tail(narrowspeciesdays)
tail(narrowspeciesdays.status)
#the dataframes need to match so that we can bind them on number of columns and rows, as well as column names.

narrowspeciesdays<-cbind(narrowspeciesdays,narrowspeciesdays.status$Days)
names(narrowspeciesdays)[5]<-"Days"
head(narrowspeciesdays)
# the dataframe is done

#checking for answers of number of days, however with no species information.
narrowspeciesdays[!is.na(narrowspeciesdays$Days)&is.na(narrowspeciesdays$Species),]


#we can see that there are many lines in which species was not discriminated. 
narrowspeciesdays[is.na(narrowspeciesdays$Days)&is.na(narrowspeciesdays$Species),] # these are the lines with NAs in both columns; species and days. Probably, no activity was done.
narrowspeciesdays[!is.na(narrowspeciesdays$Days)&!is.na(narrowspeciesdays$Species),] #in this line we see the rows with information for both species and days.

#remove all the rows with NA values for the 4th, 3rd and 2nd answer for species. We won't need to keep them. as for the rows with SP1, it is important to keep them to register respondents that did not do such activity.
dim(narrowspeciesdays)
narrowspeciesdays<-narrowspeciesdays[!(grepl("Sp2|Sp3|Sp4",narrowspeciesdays$Activity) & is.na(narrowspeciesdays$Species)),]
dim(narrowspeciesdays)
head(narrowspeciesdays)

#however, there are still answers without species.
narrowspeciesdays[!is.na(narrowspeciesdays$Days)&is.na(narrowspeciesdays$Species),]
narrowspeciesdays[!is.na(narrowspeciesdays$Days)&is.na(narrowspeciesdays$Species)&narrowspeciesdays$Days!=0,]
#these answers will be assigned to other species
#narrowspeciesdays[!is.na(narrowspeciesdays$Days)&is.na(narrowspeciesdays$Species),]$Species<-"Unknown"
narrowspeciesdays[!is.na(narrowspeciesdays$Days)&is.na(narrowspeciesdays$Species)&narrowspeciesdays$Days!=0,]$Species<-"Unknown"

narrowspeciesdays


#reduce the names in the narrowspeciesdays$Activity column
unique(narrowspeciesdays$Activity)
#first remove "ActivityAny" from the names
narrowspeciesdays$Activity<-substring(narrowspeciesdays$Activity, first = 12)
#now, remove everything after "Sp"
narrowspeciesdays$Activity<-sapply(strsplit(narrowspeciesdays$Activity,split = "Sp"), "[[", 1)
unique(narrowspeciesdays$Activity) #done.


#build one column for species and activity in the same name. 
for ( i in 1:nrow(narrowspeciesdays)) {
  narrowspeciesdays$ActivitySpecies[i]<-paste (narrowspeciesdays$Activity[i], narrowspeciesdays$Species[i])
}
narrowspeciesdays$ActivitySpecies
head(narrowspeciesdays)
#remove NA in the end of the string.
narrowspeciesdays$ActivitySpecies<-gsub(pattern = " NA", replacement = "", narrowspeciesdays$ActivitySpecies)


# now we take the dataframe that was previously built for the standardized activities by species (cattle, dairy, beef and sheep activities for milking, drenching, assisting labour, etc.)
head(narrow.score.FarmActivity)
unlist(narrow.score.FarmActivity$Group)
# We will remove all the rows with "Any" in the activity name,because we have already used it to classify by species in the narrowspeciesdays dataframe which will be merged along.  
narrow.score.FarmActivity<- narrow.score.FarmActivity[!grepl(pattern = "Any",x = narrow.score.FarmActivity$Group),]
head(narrow.score.FarmActivity)

# Remove the word activity from the activity names
narrow.score.FarmActivity$Group<- gsub("Activity","",x=narrow.score.FarmActivity$Group)
head(narrow.score.FarmActivity)
# Create the columns Species and ActivitySpecies in the narrow.score.FarmActivity dataframe
narrow.score.FarmActivity$Group<-gsub('([[:upper:]])', ' \\1', narrow.score.FarmActivity$Group)
for (i in 1:nrow(narrow.score.FarmActivity)) { 
  narrow.score.FarmActivity$Activity[i]<- strsplit(narrow.score.FarmActivity$Group[i],split = " ")[[1]][[length(strsplit(narrow.score.FarmActivity$Group[i],split = " ")[[1]])]] 
}


for (i in 1:nrow(narrow.score.FarmActivity)) { 
  narrow.score.FarmActivity$Species[i]<- strsplit(narrow.score.FarmActivity$Group[i],split = " ")[[1]][[2]] 
}
narrow.score.FarmActivity$Species


narrow.score.FarmActivity$ActivitySpecies<- paste(narrow.score.FarmActivity$Activity, narrow.score.FarmActivity$Species)

# totalscore is written twice in narrow.score.farmactivity$ActivitySpecies column.
narrow.score.FarmActivity$ActivitySpecies<-gsub("Totalscore Totalscore", "Totalscore", narrow.score.FarmActivity$ActivitySpecies)


#In the Activity column I will put lamb and calve in the same category

narrow.score.FarmActivity$Activity<-gsub(pattern = "Calve|Lamb", replacement = "AssistBirth",x = narrow.score.FarmActivity$Activity)


# final adjustments for the rbind of the 2 dataframes (narrowspeciesdays and narrow.score.FarmActivity)
head(narrowspeciesdays)
head(narrow.score.FarmActivity)
narrow.score.FarmActivity<-narrow.score.FarmActivity[-which(names(narrow.score.FarmActivity)=="Group")]
narrow.score.FarmActivity<-narrow.score.FarmActivity[,c(1,2,4,5,6,3)]
narrowspeciesdays<-narrowspeciesdays[,c(1,2,3,4,6,5)]
names(narrowspeciesdays)[which(names(narrowspeciesdays)=="Days")]<-"Score"
head(narrowspeciesdays)
head(narrow.score.FarmActivity)

#dataframes are ready for the rbind.
FarmActivitiescompleteScores<-rbind(narrow.score.FarmActivity, narrowspeciesdays)
dim(FarmActivitiescompleteScores)
# now, all the NA values in score will be changed to 0s. that means that who did not do the activity earns 0 points for it. 
FarmActivitiescompleteScores[is.na(FarmActivitiescompleteScores$Score),which(names(FarmActivitiescompleteScores)=="Score")]<-0




filter(FarmActivitiescompleteScores,FarmActivitiescompleteScores$Species=="Unknown")

#remove repeated values for same activity and species (sometimes someone responded "cleaned faeces from sheep and ewes, the algorithim created 2 rows for that)


#these are the lines of repeated values
for (i in unique(dat$QID)) {
  ifelse(any(duplicated(FarmActivitiescompleteScores[FarmActivitiescompleteScores$QID %in% i,]$ActivitySpecies)), print(FarmActivitiescompleteScores[FarmActivitiescompleteScores$QID==i,][duplicated(FarmActivitiescompleteScores[FarmActivitiescompleteScores$QID==i,]$ActivitySpecies),]),next)}


#these are the QIDs that have repeated rows ( because the respondent answered two names that were categorized twice for the same species within an activitiy (e.g. clean urine from lambs and sheep ))


#we will remove these lines from the narrow dataa frame. So that we can transform to  wide dataframe, it is necessary that no repeated values exist for each QID number. Besides, the number of days would count twice, and we do not want that.
repeatedspecies<-c(0)
for (i in unique(dat$QID)) {
  repeatedspecies[length(repeatedspecies)]<-unlist(ifelse(any(duplicated(FarmActivitiescompleteScores[FarmActivitiescompleteScores$QID %in% i,]$ActivitySpecies)), FarmActivitiescompleteScores[FarmActivitiescompleteScores$QID==i,][duplicated(FarmActivitiescompleteScores[FarmActivitiescompleteScores$QID==i,]$ActivitySpecies),],next))
  repeatedspecies[length(repeatedspecies)+1]=c(0)
}

repeatedspecies 
unique(repeatedspecies[1:length(repeatedspecies)-1])
for (i in  unique(repeatedspecies[1:length(repeatedspecies)-1])) { 
  FarmActivitiescompleteScores<- FarmActivitiescompleteScores[-which(rownames(FarmActivitiescompleteScores)==rownames(FarmActivitiescompleteScores[FarmActivitiescompleteScores$QID==i,][which(duplicated(FarmActivitiescompleteScores[FarmActivitiescompleteScores$QID==i,5])),])),]    }

# Rows removed 


# Now adjust the structures of the data frame columns. What must be considered factor, becomes factor. And we will set the levels of each factor. 

head(FarmActivitiescompleteScores)
str((FarmActivitiescompleteScores))
for (i in 3:5) { FarmActivitiescompleteScores[,i]<- as.factor(FarmActivitiescompleteScores[,i])}
str((FarmActivitiescompleteScores))
levels(FarmActivitiescompleteScores$Species)
FarmActivitiescompleteScores$Species<-factor(FarmActivitiescompleteScores$Species, levels = c("Cattle","Beef","Dairy","Sheep","Pigs","Deer","Goats","Horses","Unknown","Totalscore"))
paste(levels(FarmActivitiescompleteScores$Activity),collapse = dQuote(","))
FarmActivitiescompleteScores$Activity<-factor(FarmActivitiescompleteScores$Activity, levels = c("AssistBirth" , "Crutch"  , "Milk"    ,  "Dock" ,      "Drench" , "Palpate"  ,  "Shear"      ,      "Clean", "Slaughter" ,"Homekill" , "Welfare" , "Dress" , "Other" , "Totalscore"))
for (i in 1:nrow(FarmActivitiescompleteScores)){
  
  #creating a column with animal group - basically, unidentified, beef or dairy cattle becomes any cattle 
  FarmActivitiescompleteScores$Group[i]<-ifelse(grepl(pattern = "Cattle|Beef|Dairy" , FarmActivitiescompleteScores$Species[i], ignore.case = T), "AnyCattle", as.character(FarmActivitiescompleteScores$Species[i])) } 

FarmActivitiescompleteScores$Group<-factor(as.factor(FarmActivitiescompleteScores$Group), levels= c("AnyCattle","Sheep","Pigs","Deer","Goats","Horses","Unknown","Totalscore"))


#assigning 0 values for all the open field species that were included 

farmactivityscoresbyActivitySpecies<-spread(FarmActivitiescompleteScores[c(1,2,5,6)],key=ActivitySpecies,value = Score)

totalscorenumber<-which(names(farmactivityscoresbyActivitySpecies)=="Totalscore")

farmactivityscoresbyActivitySpecies.narrow<-gather(farmactivityscoresbyActivitySpecies,key = ActivitySpecies, value = "Score",c(3:(totalscorenumber-1),(totalscorenumber+1):ncol(farmactivityscoresbyActivitySpecies)))
farmactivityscoresbyActivitySpecies.narrow<-farmactivityscoresbyActivitySpecies.narrow[-3]
dim(farmactivityscoresbyActivitySpecies.narrow)


farmactivityscoresbyActivitySpecies.narrow$Activity<-sapply(strsplit(farmactivityscoresbyActivitySpecies.narrow$ActivitySpecies," "),head,1)
farmactivityscoresbyActivitySpecies.narrow$Species<-sapply(strsplit(farmactivityscoresbyActivitySpecies.narrow$ActivitySpecies," "),tail,1)

head(farmactivityscoresbyActivitySpecies.narrow)
farmactivityscoresbyActivitySpecies.narrow$Species<-ifelse(grepl("Clean|Dress|Homekill|Other|Slaughter|Welfare",farmactivityscoresbyActivitySpecies.narrow$Species),NA,farmactivityscoresbyActivitySpecies.narrow$Species)
unique(farmactivityscoresbyActivitySpecies.narrow$Species)

#In the Activity column I will put lamb and calve in the same category

farmactivityscoresbyActivitySpecies.narrow$Activity<-gsub(pattern = "Calve|Lamb", replacement = "AssistBirth",x = farmactivityscoresbyActivitySpecies.narrow$Activity)


for (i in 1:nrow(farmactivityscoresbyActivitySpecies.narrow)){
  
  #creating a column with animal group - basically, unidentified, beef or dairy cattle becomes any cattle 
  farmactivityscoresbyActivitySpecies.narrow$Group[i]<-ifelse(grepl(pattern = "Cattle|Beef|Dairy" , farmactivityscoresbyActivitySpecies.narrow$Species[i], ignore.case = T), "AnyCattle", as.character(farmactivityscoresbyActivitySpecies.narrow$Species[i])) } 

head(farmactivityscoresbyActivitySpecies.narrow)
head (FarmActivitiescompleteScores)

farmactivityscoresbyActivitySpecies.narrow<-farmactivityscoresbyActivitySpecies.narrow[c(1,2,5,6,3,4,7)]
unique(farmactivityscoresbyActivitySpecies.narrow$Species)
unique(FarmActivitiescompleteScores$Species)
farmactivityscoresbyActivitySpecies.narrow<-rbind(farmactivityscoresbyActivitySpecies.narrow,FarmActivitiescompleteScores[FarmActivitiescompleteScores$Species=="Totalscore"&!is.na(FarmActivitiescompleteScores$Species),])

unique(farmactivityscoresbyActivitySpecies.narrow$Species)

head(farmactivityscoresbyActivitySpecies.narrow)
str((farmactivityscoresbyActivitySpecies.narrow))
for (i in c(3:5,7)) { farmactivityscoresbyActivitySpecies.narrow[,i]<- as.factor(farmactivityscoresbyActivitySpecies.narrow[,i])}
str((farmactivityscoresbyActivitySpecies.narrow))
levels(farmactivityscoresbyActivitySpecies.narrow$Species)
farmactivityscoresbyActivitySpecies.narrow$Species<-factor(farmactivityscoresbyActivitySpecies.narrow$Species, levels = c("Cattle","Beef","Dairy","Sheep","Pigs","Deer","Goats","Horses","Poultry","Unknown","Totalscore"))
paste(levels(farmactivityscoresbyActivitySpecies.narrow$Activity),collapse = dQuote(","))


farmactivityscoresbyActivitySpecies.narrow$Activity<-factor(farmactivityscoresbyActivitySpecies.narrow$Activity, levels = c("AssistBirth" , "Crutch"  , "Milk"    ,  "Dock" ,      "Drench" , "Palpate"  ,  "Shear"      ,      "Clean", "Slaughter" ,"Homekill" , "Welfare" , "Dress" , "Other" , "Totalscore"))
for (i in 1:nrow(farmactivityscoresbyActivitySpecies.narrow)){
  
  #creating a column with animal group - basically, unidentified, beef or dairy cattle becomes any cattle 
  farmactivityscoresbyActivitySpecies.narrow$Group[i]<-ifelse(grepl(pattern = "Cattle|Beef|Dairy" , farmactivityscoresbyActivitySpecies.narrow$Species[i], ignore.case = T), "AnyCattle", as.character(farmactivityscoresbyActivitySpecies.narrow$Species[i])) } 

farmactivityscoresbyActivitySpecies.narrow$Group<-factor(as.factor(farmactivityscoresbyActivitySpecies.narrow$Group), levels= c("AnyCattle","Sheep","Pigs","Deer","Goats","Horses","Poultry","Unknown","Totalscore"))


FarmActivitiescompleteScores<-farmactivityscoresbyActivitySpecies.narrow
FarmActivitiescompleteScores$Score[is.na(FarmActivitiescompleteScores$Score)]<-0

#making columns with total scores to be inserted to the final dataset so that we can run analyses (e.g. GLM )
head(FarmActivitiescompleteScores)
widescorebyactivityspecies<-spread(FarmActivitiescompleteScores[-c(2,3,7)],key = ActivitySpecies, value = Score)
widescorebyactivityspecies$TotalSum<-apply(widescorebyactivityspecies[3:ncol(widescorebyactivityspecies)],MARGIN = 1, function(x)sum(x,na.rm=T))
widescorebyactivityspecies$TotalSum
widescorebyactivityspecies.2<-spread(widescorebyactivityspecies[-c(3:(ncol(widescorebyactivityspecies)-1))],key = Species, value = TotalSum)
widescorebyactivityspecies.2$Totalscore<- apply(widescorebyactivityspecies.2[2:(ncol(widescorebyactivityspecies.2)-1)],MARGIN = 1, function(x)sum(x,na.rm=T))
widescorebyactivityspecies.2
#remove the NA COLUMN
names(widescorebyactivityspecies.2)
widescorebyactivityspecies.2<-widescorebyactivityspecies.2[-which(names(widescorebyactivityspecies.2)=="<NA>")]
widescorebyactivityspecies.2
#create a variable for any cattle
widescorebyactivityspecies.2$AnyCattle<-apply(widescorebyactivityspecies.2[which(grepl("Cattle|Dairy|Beef",names(widescorebyactivityspecies.2)))], MARGIN = 1, function(x)sum(x, na.rm=T))

#changing column order
widescorebyactivityspecies.2<-widescorebyactivityspecies.2[c(1,which(names(widescorebyactivityspecies.2)=="AnyCattle"),2:(ncol(widescorebyactivityspecies.2)-1))]


#Do a dataframe separated by activities
widescorebyactivityspecies.3<-spread(FarmActivitiescompleteScores[-c(2,4,7)],key = ActivitySpecies, value = Score)
widescorebyactivityspecies.3$TotalSum<-apply(widescorebyactivityspecies.3[3:ncol(widescorebyactivityspecies.3)],MARGIN = 1, function(x)sum(x,na.rm=T))
widescorebyactivityspecies.3$TotalSum
widescorebyactivityspecies.4<-spread(widescorebyactivityspecies.3[-c(3:(ncol(widescorebyactivityspecies.3)-1))],key = Activity, value = TotalSum)
widescorebyactivityspecies.4$Totalscore<- apply(widescorebyactivityspecies.4[2:(ncol(widescorebyactivityspecies.4)-1)],MARGIN = 1, function(x)sum(x,na.rm=T))
widescorebyactivityspecies.4

#binding the two dataframes, in one - the variable group for species and the variable group for activities.
Final.widescorebyactivityspecies<-cbind(widescorebyactivityspecies.2,widescorebyactivityspecies.4[-c(1,ncol(widescorebyactivityspecies.4))])
#creating a variable called AnySlaughter to add up :Slaugter, HomeKill, WelfareKilling and Dressing Carcass
Final.widescorebyactivityspecies$AnySlaughter<-apply(Final.widescorebyactivityspecies[which(names(Final.widescorebyactivityspecies)=="Slaughter"):which(names(Final.widescorebyactivityspecies)=="Dress")],MARGIN = 1, function(x)sum(x,na.rm=T))
#putting in the desired order( AnySlaughter after all the types of slaughter variables)
Final.widescorebyactivityspecies<-Final.widescorebyactivityspecies[c(1:(which(names(Final.widescorebyactivityspecies)=="Dress")),(which(names(Final.widescorebyactivityspecies)=="AnySlaughter")),(which(names(Final.widescorebyactivityspecies)=="Dress")+1))]
#changing the name of the variables to be correctly identified in the final dataset
names(Final.widescorebyactivityspecies)[2:ncol(Final.widescorebyactivityspecies)]<- paste("Score",names(Final.widescorebyactivityspecies)[2:ncol(Final.widescorebyactivityspecies)],"Activity",sep = "")
dat<-cbind(dat,Final.widescorebyactivityspecies[2:ncol(Final.widescorebyactivityspecies)])

categoryactivityscores<-which(names(dat)=="ScoreAnyCattleActivity"):which(names(dat)=="ScoreOtherActivity")

#now to the plots
#By species
#Including 0s
ggplot(FarmActivitiescompleteScores[!is.na(FarmActivitiescompleteScores$Species),],aes(x=Species,y=Score, fill=types)) + geom_boxplot(varwidth=TRUE, na.rm = T) + labs( x="", y="Farm activities score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=10)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

##Excluding 0s
ggplot(FarmActivitiescompleteScores[!(is.na(FarmActivitiescompleteScores$Species)|FarmActivitiescompleteScores$Score==0),],aes(x=Species,y=Score, fill=types)) + geom_boxplot(varwidth=TRUE, na.rm = T) + labs( x="", y="Farm activities score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=10)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#By activity 
#including 0s
ggplot(FarmActivitiescompleteScores[!is.na(FarmActivitiescompleteScores$Species),],aes(x=Activity,y=Score, fill=types)) + geom_boxplot(varwidth=TRUE, na.rm = T) + labs( x="", y="Farm activities score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=10)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#excluding 0s

ggplot(FarmActivitiescompleteScores[!(is.na(FarmActivitiescompleteScores$Species)|FarmActivitiescompleteScores$Score==0) ,],aes(x=Activity,y=Score, fill=types)) + geom_boxplot(varwidth=TRUE, na.rm = T) + labs( x="", y="Farm activities score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=10)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#By animal group
#including 0s
ggplot(FarmActivitiescompleteScores[!is.na(FarmActivitiescompleteScores$Species),],aes(x=Group,y=Score, fill=types)) + geom_boxplot(varwidth=TRUE, na.rm = T) + labs( x="", y="Farm activities score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=10)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#excluding 0s
ggplot(FarmActivitiescompleteScores[!(is.na(FarmActivitiescompleteScores$Species)|FarmActivitiescompleteScores$Score==0) ,],aes(x=Group,y=Score, fill=types)) + geom_boxplot(varwidth=TRUE, na.rm = T) + labs( x="", y="Farm activities score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=10)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#Now doing some Wilcoxon tests to test the difference between cases and controls.
head(score.FarmActivity)
testsforfarmactivities<-as.data.frame(matrix(ncol=5,nrow=ncol(score.FarmActivity)-2))
names(testsforfarmactivities)<-c("Meancases","Mediancases","MeanControl","MedianControl","Wilcoxon.p-value")
rownames(testsforfarmactivities)<-names(score.FarmActivity)[3:ncol(score.FarmActivity)]
testsforfarmactivities
for (i in 3:(ncol(score.FarmActivity))) {
  testsforfarmactivities[i-2,5]<- ifelse(any(score.FarmActivity[dat$Type=="Case",][,i]>0)&any(score.FarmActivity[dat$Type=="Control",][,i]>0),wilcox.test(score.FarmActivity[dat$Type=="Case",][,i], score.FarmActivity[dat$Type=="Control",][,i] )$p.value, NA)
  testsforfarmactivities[i-2,1]<-mean(score.FarmActivity[dat$Type=="Case",][,i],na.rm = T)
  testsforfarmactivities[i-2,2]<-median(score.FarmActivity[dat$Type=="Case",][,i],na.rm = T)
  testsforfarmactivities[i-2,3]<-mean(score.FarmActivity[dat$Type=="Control",][,i],na.rm = T)
  testsforfarmactivities[i-2,4]<-median(score.FarmActivity[dat$Type=="Control",][,i],na.rm = T) 
}

testsforfarmactivities

#######################Integration of Livestock contact and Farm Activities#####
names(dat[contactcolumns])
names(dat[activitygroupcolumns])
names(dat[glmactivitycolumns])
names(dat[which(names(dat)=="contactwithfarm")])
names(dat[which(names(dat)=="AnyActivityFarm")])
#These variables do not talk to each other so well when we put them in multivariable models. Some of them become protective against another that becomes a risk factor (for example. dairy cow contact protective and dairy cow activity risk factor when both are used. I will try to integrate them into one variable foreach category.)
#We could think in: 0 - no contact and no activity / 1: just contact / 2 contact and activity

dat[dat$contactwithfarm==0&dat$AnyActivityFarm==-1,1]
# these observations had activities but not contact. let's have a look
dat[dat$contactwithfarm==0&dat$AnyActivityFarm==-1,c(contactcolumns,activitiescolumns,which(names(dat)=="ActivityAnyOtherSpecify"))]
#changing answers from respondent(s)
dat[dat$ActivityAnyOtherSpecify=="Shifting cattle"&!is.na(dat$ActivityAnyOtherSpecify),c(which(names(dat)=="contactwithfarm"),which(names(dat)=="ContactCattle"))]<--1

dat$LivestockContorAct<-ifelse(dat$contactwithfarm==0,0,ifelse(dat$AnyActivityFarm==0,1,ifelse(dat$AnyActivityFarm==-1,2,NA)))


livestockstrings<-c("Dairy","Beef","Cattle","Sheep", "Pigs", "Horses","Deer","Goats", "Poultry")

activityorcontact<-data.frame(matrix(nrow=nrow(dat),ncol=length(livestockstrings)))

head(activityorcontact)

for (i in 1:length(livestockstrings)) { 
  activityorcontact[i]<-ifelse(dat[contactcolumns[grep(livestockstrings[i],names(dat[contactcolumns]),ignore.case = T )]]==0,0,ifelse(dat[glmactivitycolumns[grep(livestockstrings[i],names(dat[glmactivitycolumns]),ignore.case = T )]]==0,1,ifelse(dat[ glmactivitycolumns[grep(livestockstrings[i],names(dat[glmactivitycolumns]),ignore.case = T )]]==-1,2,NA))) 
  
}

names(activityorcontact)<-paste(livestockstrings,"ContorAct", sep = "")
names(activityorcontact)

activityorcontact$CattleContorAct<-ifelse(activityorcontact$DairyContorAct==2|activityorcontact$BeefContorAct==2,2,ifelse(activityorcontact$DairyContorAct==1|activityorcontact$BeefContorAct==1,1,0))

for (i in 1:length(activityorcontact)) { 
  attr(activityorcontact[[i]], "dimnames")[[2]]<-NULL }

dat<-cbind(dat,activityorcontact)
activityorcontactvector<-grep("ContorAct",names(dat),ignore.case = T)

#These are the variables created.
head(activityorcontact)

#Epilogue: Using these variables in the MV models did not solve the problems I identified in the beginning of this section. The variables acted the same way. When 0 was the base for comparison, in many variables, the value 1 was protective, and the value 2 was a risk factor.

##################################PPEs/ Wash Hands and livestock activities#####
#first let's enumerate which individulas had either contact or activities with livestock. Those who had no contact have to be differentiated from those that had contact and did not use PPEs. 
dat$AnyActivityFarm
dat$contactwithfarm

#looking for respondents that did not have livestock contact and had farm activities. How could that be? 
dat[which(dat$AnyActivityFarm==-1 & dat$contactwithfarm==0),c(1,contactcolumns,activitiescolumns)]
contactoractivity<-dat$AnyActivityFarm|dat$contactwithfarm

#create a vector for the PPE column numbers 
livestockPPEcolumns<-(which(letters=="j")*26+which(letters=="m")-1):(which(letters=="j")*26+which(letters=="u")-1)
livestockPPEcolumns
names(dat[livestockPPEcolumns])

#change 999s to NAs
for (i in livestockPPEcolumns) {
  dat[is.na(dat[,i])|dat[i]==999,i]<-NA }


#find respondents that did not have contact or activities with livestock but answered positively to PPE
dat[contactoractivity==0&apply(dat[livestockPPEcolumns[1:6]],MARGIN = 1, function(x)any(x==-1)),c(1,contactcolumns,activitiescolumns, livestockPPEcolumns)][apply(dat[contactoractivity==0&apply(dat[livestockPPEcolumns[1:6]],MARGIN = 1, function(x)any(x==-1)),c(1,contactcolumns,activitiescolumns, livestockPPEcolumns)],MARGIN = 1, function(x)!all(is.na(x))),]

#checking if the column PPENone is working. I have made a command to select rows for PPENone==-1 and any positive answer for the use of PPE in the columns. If no columns appear, then it is correctly working.
dat[dat$PPENone==-1&apply(dat[livestockPPEcolumns[1:6]],1,function(x)any(x==-1))&apply(dat[livestockPPEcolumns[1:6]],1,function(x)!all(is.na(x))),livestockPPEcolumns]

dat$PPEAny<-ifelse(dat$PPENone==0,-1,ifelse(dat$PPENone==-1,0,NA))
dat$PPEAny

ctab(as.factor(dat$PPENone), dat$Type)
PPE.test.fisher<-fisher.test(ctab(as.factor(dat$PPEAny), dat$Type)$table)
PPE.test.fisher



AnyUsePPE<-(c("AnyUseofPPE",as.numeric(PPE.test.fisher$estimate[1]),as.numeric(PPE.test.fisher$conf.int[1]),as.numeric(PPE.test.fisher$conf.int[2])))

#Contingency Tables
comparison<-tibble(1,2,3,4,5,6)
for (i in c(livestockPPEcolumns[1:6],which(names(dat)=="PPEAny"))) {
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}

comparisonPPE<-as.data.frame(matrix(ncol=4,nrow=length(livestockPPEcolumns)-3))
names(comparisonPPE)<-c("PPEGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonPPE
comparisonPPE$PPEGroup<-names(dat[livestockPPEcolumns[1:6]])
comparisonPPE<-rbind(comparisonPPE,AnyUsePPE)
comparisonPPE
#fisher tests with the confidence intervals
for (i in 1:length(livestockPPEcolumns[1:6])) {
  comparisonPPE[i,1]<-names(dat[livestockPPEcolumns[i]])
  comparisonPPE[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,livestockPPEcolumns[i]]),dat$Type)$table)$estimate)
  comparisonPPE[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,livestockPPEcolumns[i]]),dat$Type)$table)$conf.int[1])
  comparisonPPE[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,livestockPPEcolumns[i]]),dat$Type)$table)$conf.int[2])
}

for (i in 2:4) {
  comparisonPPE[,i]<-as.numeric(comparisonPPE[,i])}

forestplot(labeltext = comparisonPPE$PPEGroup, mean=comparisonPPE$fisherOR,lower=comparisonPPE$Min.conf.int, upper=comparisonPPE$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           is.summary = c(rep(F,6),T), 
           boxsize = 1/exp(abs(log(comparisonPPE$Max.conf.int)-(log(comparisonPPE$Min.conf.int)))),
           clip = c(0,60), title="Use of protective equipment for contact with livestock risk of leptospirosis cases")

dat$HandWashFreq[dat$HandWashFreq==999]<-NA
table(dat$HandWashFreq)
dat$HandWashFreqOther
#check what participants that answered 4 ( other frequencies) said in the open field and try to put them into one of the groups.
dat$HandWashFreqOther[dat$HandWashFreq==4&!is.na(dat$HandWashFreqOther)]
dat$HandWashFreq[dat$HandWashFreqOther=="hardly ever"&!is.na(dat$HandWashFreqOther)]<-3
dat$HandWashFreq[dat$HandWashFreqOther=="4 or 5 times a day"&!is.na(dat$HandWashFreqOther)]<-1
table(dat$HandWashFreq)

ctab(as.factor(dat$HandWashFreq), dat$Type)
wilcox.test(dat$HandWashFreq[dat$Type=="Case"&dat$HandWashFreq<4],dat$HandWashFreq[dat$Type=="Control"&dat$HandWashFreq<4])
#no significant difference in the Wilcoxon test

### PPE 
dat$PPEAny
head(dat[livestockPPEcolumns])


#To analyse the use of PPE as a risk factor or protective factor, we need to isolate the participánts that had livestock activities. We cannot compare use of PPE in livestock using participants that did not have contact with livestock.
#You can see that there is a line that is preceded by a pound sign. Use the first line if you want just slaughter activities and the second line if ou want all livestock activities. 
#PPElivestockdf<-dat[dat$AnySlaughterActivity==-1,c(livestockPPEcolumns,match(c("Type", "PPEAny"),names(dat)))]
PPElivestockdf<-dat[dat$AnySlaughterActivity==-1|dat$AnyActivityFarm==-1|dat$DeadAnimals==-1|dat$AbortedAnimals==-1,c(livestockPPEcolumns,match(c("Type", "PPEAny"),names(dat)))]
#Removing NA values
PPElivestockdf[sapply(PPElivestockdf,function(x)class(x)!="factor")][is.na(PPElivestockdf[sapply(PPElivestockdf,function(x)class(x)!="factor")])]<-0
PPElivestockdf$Type<-factor(PPElivestockdf$Type, levels=c("Control", "Case"))


PPElivestockdf$PPEAny<-apply(PPElivestockdf[1:6],1,function(x)ifelse(any(x==-1),-1,0))


#Univariable Logistic regression
U.V.PPEregression<-data.frame(matrix(nrow=7,ncol=4))
names(U.V.PPEregression)<-c("Variable","OR","Min.CI", "Max.CI")
U.V.PPEregression[1]<-names(PPElivestockdf[c(1:6,ncol(PPElivestockdf))])

U.V.PPEregression[2]<-sapply(PPElivestockdf[c(1:6,ncol(PPElivestockdf))],function(x)exp(-1*summary(glm(Type~as.factor(x), data=PPElivestockdf, family="binomial"))$coefficients[2,1]))

U.V.PPEregression[4]<-sapply(PPElivestockdf[c(1:6,ncol(PPElivestockdf))],function(x)exp(-1*confint(glm(Type~as.factor(x), data=PPElivestockdf, family="binomial"))[2,1]))
U.V.PPEregression[3]<-sapply(PPElivestockdf[c(1:6,ncol(PPElivestockdf))],function(x)exp(-1*confint(glm(Type~as.factor(x), data=PPElivestockdf, family="binomial"))[2,2]))
U.V.PPEregression

forestplot(labeltext = U.V.PPEregression$Variable, mean=U.V.PPEregression$OR,lower=U.V.PPEregression$Min.CI, upper=U.V.PPEregression$Max.CI, xlab="Univariable logistic regression odds-ratio with confidence intervals", zero = 1,
           is.summary = c(rep(F,6),T), 
           boxsize = 1/exp(abs(log(U.V.PPEregression$Max.CI)-(log(U.V.PPEregression$Min.CI)))),
           clip = c(0,60), title="Use of protective equipment for contact with livestock risk of leptospirosis cases")


#prepare the spreadsheet for the MV logistic regression
PPElivestockdf<-dat[dat$AnySlaughterActivity==-1|dat$AnyActivityFarm==-1|dat$DeadAnimals==-1|dat$AbortedAnimals==-1,c(livestockPPEcolumns,match(c("Type", "PPEAny","Age", "Sex", "Gen.RuralityHomeaddr"),names(dat)))]
PPElivestockdf[sapply(PPElivestockdf,function(x)class(x)!="factor")][is.na(PPElivestockdf[sapply(PPElivestockdf,function(x)class(x)!="factor")])]<-0




################################################Pets###############
####Binding pets contact in 1 variable. contact or cleaning up urine/faeces
####### changing Nas, 99s and 999s to 0s
names(dat[c((407+3*(0:6)),429+3*(0:6))])
petscolumns<-c((407+3*(0:6)),429+3*(0:6))

for (i in petscolumns) {
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0 }

#Managing the database for pet variables
#Check for observations about other pets
dat$PetOthersSpecify[!is.na(dat$PetOthersSpecify)&!dat$PetOthersSpecify==999 ]

#should Pet Chicken be considered contact with poultry? what about other birds? 

dat[grepl("chicken",dat$PetOthersSpecify, ignore.case = T),c(7,petscolumns,contactcolumns,which(names(dat)=="PetOthersSpecify"))]
dat[grepl("cockatoo|duck|birds",dat$PetOthersSpecify, ignore.case = T),c(7,petscolumns,contactcolumns,which(names(dat)=="PetOthersSpecify"))]

#same thing for pigs
dat[grepl("pig",dat$PetOthersSpecify, ignore.case = T),c(7,petscolumns,contactcolumns,which(names(dat)=="PetOthersSpecify"))]

#same thing for sheeps
dat[grepl("sheep|lamb",dat$PetOthersSpecify, ignore.case = T),c(7,petscolumns,contactcolumns,which(names(dat)=="PetOthersSpecify"))]




#changing the other pet contact status - some people referred to livestock animals as pets. I have previously changed the option to livestock contact and classified the contact as outside work.
#chicken
dat[grepl("chicken",dat$PetOthersSpecify, ignore.case = T),c(7,petscolumns,contactcolumns,which(names(dat)=="PetOthersSpecify"))]
dat[dat$PetOthersSpecify %in%  c("Chickens","chickens","chicken") ,c(petscolumns,match("PetOthersSpecify",names(dat)))]
dat[dat$PetOthersSpecify %in%  c("Chickens","chickens","chicken") ,match("PetOthers",names(dat))]<-0
dat[dat$PetOthersSpecify %in%  c("Chickens","chickens","chicken") ,match("PetOthersDays",names(dat))]<-0
#Pigs
dat[grepl("pig",dat$PetOthersSpecify, ignore.case = T),c(7,petscolumns,contactcolumns,match(c("PetOthers","PetOthersSpecify","PetOthersDays","PetOthersType"),names(dat)))]
dat[grepl("pig",dat$PetOthersSpecify, ignore.case = T),match("PetOthers",names(dat))]<- 0
dat[grepl("pig",dat$PetOthersSpecify, ignore.case = T),match("PetOthersDays",names(dat))]<-0
#sheep
dat[grepl("sheep",dat$PetOthersSpecify, ignore.case = T),match("PetOthers",names(dat))]<-0
dat[grepl("sheep|lamb",dat$PetOthersSpecify, ignore.case = T),match("PetOthersDays",names(dat))]<-0


#creating a dataframe with all the pets columns
AnyActivitypets<-cbind(dat[407+3*(0:6)],dat[429+3*(0:6)])
AnyActivitypets #many NAs that seem to have the same meaning as 0
AnyActivitypets[is.na(AnyActivitypets)]<-0 #changing them to 0 
AnyActivitypets

summarypets<-rep(0,nrow(dat))                                          
#Creating a variable for any contact or activity with Pets
for (i in 1:nrow(dat)){                                             
  summarypets[i]<-                                             
    ifelse(any(AnyActivitypets[i,]==-1),-1,0) 
}

summarypets
dat$AnyActivityPets<-summarypets
ctab(as.factor(dat$AnyActivityPets),as.factor(dat$Type))
chisq.test(as.factor(dat$AnyActivityPets), as.factor(dat$Type))
test.fisher8<-fisher.test(as.factor(dat$AnyActivityPets), as.factor(dat$Type))
anypetactivity<-(c("AnyPetActivity",as.numeric(test.fisher8$estimate[1]),as.numeric(test.fisher8$conf.int[1]),as.numeric(test.fisher8$conf.int[2])))
anypetactivity


### separating pets in groups of pets
####categories for pets contact
names(dat[(407+3*(0:6))])


# contingency tables
comparison<-tibble(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
for (i in c((407+3*(0:6)),429+3*(0:6))){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}


#Tables with Fisher's O.R.s
comparisonactivitypets<-as.data.frame(matrix(ncol=4,nrow=length(petscolumns)))
names(comparisonactivitypets)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonactivitypets
comparisonactivitypets$AnimalGroup<-names(dat[c((407+3*(0:6)),429+3*(0:6))])
comparisonactivitypets<-rbind(comparisonactivitypets,anypetactivity)
comparisonactivitypets

#fisher tests with the confidence intervals
for (i in 1:length(petscolumns)){
 tryCatch({ comparisonactivitypets[i,1]<-names(dat[petscolumns[i]])
  comparisonactivitypets[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,petscolumns[i]]),dat$Type)$table)$estimate)
  comparisonactivitypets[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,petscolumns[i]]),dat$Type)$table)$conf.int[1])
  comparisonactivitypets[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,petscolumns[i]]),dat$Type)$table)$conf.int[2])
  print(as.numeric(fisher.test(ctab(as.factor(dat[,petscolumns[i]]),dat$Type)$table)$p.value))}, error=function(e){})
}

comparisonactivitypets
str( comparisonactivitypets)
for (i in 2:4){ comparisonactivitypets[,i]<-as.numeric(comparisonactivitypets[,i])}
forestplot(labeltext = comparisonactivitypets$AnimalGroup, mean=comparisonactivitypets$fisherOR ,lower=comparisonactivitypets$Min.conf.int, 
           upper=comparisonactivitypets$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           clip = c(0,20), title="Risk of contact with pets or their urine/faeces", 
           is.summary = c(rep(F,14),T), 
           boxsize = 1/exp(abs(log(comparisonactivitypets$Max.conf.int)-(log(comparisonactivitypets$Min.conf.int)))))

##################################################Days in contact with Pets######################
#creating a score for contact with pets

names(dat[c((407+3*(0:6)),429+3*(0:6))])
score.pets<-as.data.frame(matrix(ncol=length( petscolumns),nrow=nrow(dat)))
names(score.pets)<-names(dat[ petscolumns])
head(score.pets)

for (i in 1:length(petscolumns)) { for (j in 1:nrow(dat)){
  score.pets[j,i]<-as.numeric(dat[j,petscolumns[i]])*-as.numeric(dat[j,petscolumns[i]+1])
}}

score.pets$Totalscore<-apply(score.pets,1,sum, na.rm=T)
dat$petsScore<-score.pets$Totalscore
hist(dat$petsScore)
# the distribution does not seem to behave as normal distribution. I will try to apply a non-parametric test. In this case, Wilcoxon signed rank sum test
dat$petsScore[dat$Type=="Case"]
dat$petsScore[dat$Type=="Control"]
# values with no zeros: 
dat$petsScore[dat$Type=="Control"&dat$petsScore!=0]
dat$petsScore[dat$Type=="Case"&dat$petsScore!=0]
#means and medians for all values
mean(dat$petsScore[dat$Type=="Case"]) ; median(dat$petsScore[dat$Type=="Case"]) 
mean(dat$petsScore[dat$Type=="Control"]); median(dat$petsScore[dat$Type=="Control"])
#means and medians excluding the non=exposures (sum of exposure days=0)
mean(dat$petsScore[dat$Type=="Case"&dat$petsScore!=0]); median(dat$petsScore[dat$Type=="Case"&dat$petsScore!=0])
mean(dat$petsScore[dat$Type=="Control"&dat$petsScore!=0]);median(dat$petsScore[dat$Type=="Control"&dat$petsScore!=0])

#the mean and median had little difference from Case to control
#wilcoxon test: 
wilcox.test(dat$petsScore[dat$Type=="Case"], dat$petsScore[dat$Type=="Control"])
# the wilcoxon signed rank test ( Mann-Whitney) was insignifficant for difference in means.
wilcox.test(dat$petsScore[dat$Type=="Case"&dat$petsScore!=0], dat$petsScore[dat$Type=="Control"&dat$petsScore!=0])
# signifficant - among the people that had contact with pets, cases had more days of contact with pets or cleaning up their faeces.

#boxplot including 0s
boxplot(dat$petsScore[dat$Type=="Case"], dat$petsScore[dat$Type=="Control"])
#boxplot excluding 0s
boxplot(dat$petsScore[dat$Type=="Case"&dat$petsScore!=0], dat$petsScore[dat$Type=="Control"&dat$petsScore!=0])


ggplot(dat,aes(x=Type,y=petsScore)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Pet exposure score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=20)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

# as the last tests were inconclusive, I will try to divide in pet species and activities (contact or clean up urine and faeces)

#creating a score for contact with pets

c((407+3*(0:6)))
score.PetContact<-as.data.frame(matrix(ncol=length(c((407+3*(0:6)))),nrow=nrow(dat)))
names(score.PetContact)<-names(dat[c((407+3*(0:6)))])
head(score.PetContact)

for (i in c((407+3*(0:6)))) { for (j in 1:nrow(dat)){
  score.PetContact[j,(i-404)/3]<-as.numeric(dat[j,i])*-as.numeric(dat[j,i+1])
}}

score.PetContact$Totalscore<-apply(score.PetContact,1,sum, na.rm=T)
dat$PetContactScore<-score.PetContact$Totalscore
hist(dat$PetContactScore)
# the distribution does not seem to behave as normal distribution. I will try to apply a non-parametric test. In this case, Wilcoxon signed rank sum test
dat$PetContactScore[dat$Type=="Case"]
dat$PetContactScore[dat$Type=="Control"]
# values with no zeros: 
dat$PetContactScore[dat$Type=="Control"&dat$PetContactScore!=0]
dat$PetContactScore[dat$Type=="Case"&dat$PetContactScore!=0]
#means and medians for all values
mean(dat$PetContactScore[dat$Type=="Case"]) ; median(dat$PetContactScore[dat$Type=="Case"]) 
mean(dat$PetContactScore[dat$Type=="Control"]); median(dat$PetContactScore[dat$Type=="Control"])
#means and medians excluding the non=exposures (sum of exposure days=0)
mean(dat$PetContactScore[dat$Type=="Case"&dat$PetContactScore!=0]); median(dat$PetContactScore[dat$Type=="Case"&dat$PetContactScore!=0])
mean(dat$PetContactScore[dat$Type=="Control"&dat$PetContactScore!=0]);median(dat$PetContactScore[dat$Type=="Control"&dat$PetContactScore!=0])

#the mean and median had little difference from Case to control
#wilcoxon test: 
wilcox.test(dat$PetContactScore[dat$Type=="Case"], dat$PetContactScore[dat$Type=="Control"])

# the wilcoxon signed rank test ( Mann-Whitney) was signifficant for difference in means.
wilcox.test(dat$PetContactScore[dat$Type=="Case"&dat$PetContactScore!=0], dat$PetContactScore[dat$Type=="Control"&dat$PetContactScore!=0])
# signifficant - among the people that had contact with Pets, cases had more days of contact with pets

#boxplot including 0s
boxplot(dat$PetContactScore[dat$Type=="Case"], dat$PetContactScore[dat$Type=="Control"])
#boxplot excluding 0s
boxplot(dat$PetContactScore[dat$Type=="Case"&dat$PetContactScore!=0], dat$PetContactScore[dat$Type=="Control"&dat$PetContactScore!=0])


ggplot(dat,aes(x=Type,y=PetContactScore)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Pet exposure score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=20)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


#subsetting by species to see if there is some species that represent a larger difference
wilcox.tests.pets<-list(0)
testsforpets<-as.data.frame(matrix(ncol=5,nrow=ncol(score.PetContact)-1))
names(testsforpets)<-c("Meancases","Mediancases","MeanControl","MedianControl","Wilcoxon.p-value")
rownames(testsforpets)<-names(score.PetContact)[1:ncol(score.PetContact)-1]
for (i in 1:(ncol(score.PetContact)-1)) {
  testsforpets[i,5]<- ifelse(any(score.PetContact[dat$Type=="Case",][,i]>0)&any(score.PetContact[dat$Type=="Control",][,i]>0),wilcox.test(score.PetContact[dat$Type=="Case",][,i], score.PetContact[dat$Type=="Control",][,i] )$p.value, NA)
  testsforpets[i,1]<-mean(score.PetContact[dat$Type=="Case",][,i],na.rm = T)
  testsforpets[i,2]<-median(score.PetContact[dat$Type=="Case",][,i],na.rm = T)
  testsforpets[i,3]<-mean(score.PetContact[dat$Type=="Control",][,i],na.rm = T)
  testsforpets[i,4]<-median(score.PetContact[dat$Type=="Control",][,i],na.rm = T) 
}

testsforpets
names(wilcox.tests.pets[[ length(wilcox.tests.pets)]])<-(names(score.PetContact[i]))
wilcox.tests.pets
#############################################Cleaning pets faeces and urine###############
# doing the same for Number of Days cleaning pet faeces/urine 
#creating a score for activity with pets

score.PetsCleanup<-as.data.frame(matrix(ncol=length(  c(429+3*(0:6))),nrow=nrow(dat)))
names(score.PetsCleanup)<-names(dat[  c(429+3*(0:6))])
head(score.PetsCleanup)

for (i in  c(429+3*(0:6))) { for (j in 1:nrow(dat)){
  score.PetsCleanup[j,ifelse(i<426,(i-404)/3,(i-405)/3)]<-as.numeric(dat[j,i])*-as.numeric(dat[j,i+1])
}}

score.PetsCleanup$Totalscore<-apply(score.PetsCleanup,1,sum, na.rm=T)
dat$PetsCleanupScore<-score.PetsCleanup$Totalscore
hist(dat$PetsCleanupScore)
# the distribution does not seem to behave as normal distribution. I will try to apply a non-parametric test. In this case, Wilcoxon signed rank sum test
dat$PetsCleanupScore[dat$Type=="Case"]
dat$PetsCleanupScore[dat$Type=="Control"]
# values with no zeros: 
dat$PetsCleanupScore[dat$Type=="Case"&dat$PetsCleanupScore!=0]
dat$PetsCleanupScore[dat$Type=="Control"&dat$PetsCleanupScore!=0]
#means and medians for all values
mean(dat$PetsCleanupScore[dat$Type=="Case"]) ; median(dat$PetsCleanupScore[dat$Type=="Case"]) 
mean(dat$PetsCleanupScore[dat$Type=="Control"]); median(dat$PetsCleanupScore[dat$Type=="Control"])
#means and medians excluding the non=exposures (sum of exposure days=0)
mean(dat$PetsCleanupScore[dat$Type=="Case"&dat$PetsCleanupScore!=0]); median(dat$PetsCleanupScore[dat$Type=="Case"&dat$PetsCleanupScore!=0])
mean(dat$PetsCleanupScore[dat$Type=="Control"&dat$PetsCleanupScore!=0]);median(dat$PetsCleanupScore[dat$Type=="Control"&dat$PetsCleanupScore!=0])

#the mean and median had little difference from Case to control
#wilcoxon test: 
wilcox.test(dat$PetsCleanupScore[dat$Type=="Case"], dat$PetsCleanupScore[dat$Type=="Control"])
# the wilcoxon signed rank test ( Mann-Whitney) was insignifficant for difference in means.
wilcox.test(dat$PetsCleanupScore[dat$Type=="Case"&dat$PetsCleanupScore!=0], dat$PetsCleanupScore[dat$Type=="Control"&dat$PetsCleanupScore!=0])
# also insignifficant - for cleaning up pet faeces.

#boxplot including 0s
boxplot(dat$PetsCleanupScore[dat$Type=="Case"], dat$PetsCleanupScore[dat$Type=="Control"])
#boxplot excluding 0s
boxplot(dat$PetsCleanupScore[dat$Type=="Case"&dat$PetsCleanupScore!=0], dat$PetsCleanupScore[dat$Type=="Control"&dat$PetsCleanupScore!=0])


ggplot(dat,aes(x=Type,y=PetsCleanupScore)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Pet exposure score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=20)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

########################################WILD#ANIMALS###################
##################################################Contact with Wild Animals############
#creating variable for contact with any wild animal

dat$contactwithwild<- ifelse(dat$WildPossum==-1 | dat$WildPigs==-1 | dat$WildHedgehog==-1| dat$WildRats==-1 | 
                               dat$WildMice==-1 |dat$WildFerret==-1 | dat$WildRabbits==-1 | dat$WildDeer==-1 | 
                               dat$WildGoat==-1 | dat$WildCats==-1 | dat$WildOthers==-1,-1,0 )

dat$contactwithwild

#Contingency table for contact with any Wild Animal
ctab(as.factor(dat$contactwithwild),as.factor(dat$Type))

test.fisher5<-fisher.test(ctab(as.factor(dat$contactwithwild),as.factor(dat$Type))$table)
fisher.test(ctab(as.factor(dat$contactwithwild),as.factor(dat$Type))$table)$estimate
fisher.test(ctab(as.factor(dat$contactwithwild),as.factor(dat$Type))$table)$conf.int
anycontactwild<-(c("Anycontact",as.numeric(test.fisher5$estimate[1]),as.numeric(test.fisher5$conf.int[1]),as.numeric(test.fisher5$conf.int[2])))
anycontactwild



###names for Wild animal contact variables
names(dat[283+3*(0:10)])
names(dat[(283+3*(0:10))+1])
contactwithwildcolumns<-283+3*(0:10)


####### changing Nas, 99s and 999s to 0s
#contact with wild animals
for (i in contactwithwildcolumns){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999|dat[i]==3,i]<-0
}

#Days in contact with wild animals
for (i in (contactwithwildcolumns)+1){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}


###contingency tables
for (i in contactwithwildcolumns){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}

comparisonwildcontact<-as.data.frame(matrix(ncol=4,nrow=length(contactwithwildcolumns)))
names(comparisonwildcontact)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonwildcontact
comparisonwildcontact$AnimalGroup<-names(dat[contactwithwildcolumns])
comparisonwildcontact<-rbind(comparisonwildcontact,anycontactwild)
comparisonwildcontact
#fisher tests with the confidence intervals
for (i in 1:length(contactwithwildcolumns)){
  comparisonwildcontact[i,1]<-names(dat[contactwithwildcolumns[i]])
  comparisonwildcontact[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,contactwithwildcolumns[i]]),dat$Type)$table)$estimate)
  comparisonwildcontact[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,contactwithwildcolumns[i]]),dat$Type)$table)$conf.int[1])
  comparisonwildcontact[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,contactwithwildcolumns[i]]),dat$Type)$table)$conf.int[2])
}
comparisonwildcontact

str( comparisonwildcontact)
for (i in 2:4){ comparisonwildcontact[,i]<-as.numeric(comparisonwildcontact[,i])}

forestplot(labeltext = comparisonwildcontact$AnimalGroup, mean=comparisonwildcontact$fisherOR ,lower=comparisonwildcontact$Min.conf.int, 
           upper=comparisonwildcontact$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           clip = c(0,20), title="Wild animal contact risk of leptospirosis cases", 
           is.summary = c(rep(F,11),T), 
           boxsize = 1/exp(abs(log(comparisonwildcontact$Max.conf.int)-(log(comparisonwildcontact$Min.conf.int)))))


#creating a score for Wild animal contact
score.wild.contact<-as.data.frame(matrix(ncol=11,nrow=nrow(dat)))
names(score.wild.contact)<-names(dat[contactwithwildcolumns])
head(score.wild.contact)

for (i in contactwithwildcolumns) { for (j in 1:nrow(dat)){
  score.wild.contact[j,(i-280)/3]<-as.numeric(dat[j,i])*-as.numeric(dat[j,i+1])
}}
score.wild.contact

score.wild.contact$Totalscore<-apply(score.wild.contact,1,sum, na.rm=T)
dat$wildContactScore<-score.wild.contact$Totalscore
hist(dat$wildContactScore)
# the distribution does not seem to behave as normal distribution. I will try to apply a non-parametric test. In this case, Wilcoxon signed rank sum test
dat$wildContactScore[dat$Type=="Case"]
dat$wildContactScore[dat$Type=="Control"]
# values with no zeros: 
dat$wildContactScore[dat$Type=="Control"&dat$wildContactScore!=0]
dat$wildContactScore[dat$Type=="Case"&dat$wildContactScore!=0]
#means and medians for all values
mean(dat$wildContactScore[dat$Type=="Case"]) ; median(dat$wildContactScore[dat$Type=="Case"]) 
mean(dat$wildContactScore[dat$Type=="Control"]); median(dat$wildContactScore[dat$Type=="Control"])
#means and medians excluding the non=exposures (sum of exposure days=0)
mean(dat$wildContactScore[dat$Type=="Case"&dat$wildContactScore!=0]); median(dat$wildContactScore[dat$Type=="Case"&dat$wildContactScore!=0])
mean(dat$wildContactScore[dat$Type=="Control"&dat$wildContactScore!=0]);median(dat$wildContactScore[dat$Type=="Control"&dat$wildContactScore!=0])

#the mean and median had little difference from Case to control
#wilcoxon test: 
wilcox.test(dat$wildContactScore[dat$Type=="Case"], dat$wildContactScore[dat$Type=="Control"])
# the wilcoxon signed rank test ( Mann-Whitney) was insignifficant for difference in means.
wilcox.test(dat$wildContactScore[dat$Type=="Case"&dat$wildContactScore!=0], dat$wildContactScore[dat$Type=="Control"&dat$wildContactScore!=0])
#p-value was even greater eliminating 0s (it was expected)
#boxplot including 0s
boxplot(dat$wildContactScore[dat$Type=="Case"], dat$wildContactScore[dat$Type=="Control"])
#boxplot excluding 0s
boxplot(dat$wildContactScore[dat$Type=="Case"&dat$wildContactScore!=0], dat$wildContactScore[dat$Type=="Control"&dat$wildContactScore!=0])


ggplot(dat,aes(x=Type,y=wildContactScore)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="wild contact score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=20)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

###################################################Evidence of Wild Animals###################
#evidence for wild rodents. After running analyses of wild animal evidence, there were some ideas that rats and mice may cancel each other, because not always can the evidence for one of these be characteristic. so we will create a new variable. Rats or mice (evidence for any rodents).

for (i in 1:nrow(dat)){ 
  dat$EvidenceAnyRodent[i]<-ifelse(dat$EvidenceWildRats[i]==-1|dat$EvidenceWildMice[i]==-1,-1,0) } 
dat$EvidenceAnyRodent<-as.factor(dat$EvidenceAnyRodent)
dat$EvidenceAnyRodent
ctab(dat$EvidenceAnyRodent,dat$Type)
fisher.test(ctab(dat$EvidenceAnyRodent,dat$Type)$table)




#which are the column numbers of wild animal evidence? 
wildevidencecolumns<-which(grepl("Evidence",names(dat))&!grepl("None|Specify|WildAnimals",names(dat)))

names(dat[wildevidencecolumns])



#creating variable for any evidence of wild animal 
dat[,wildevidencecolumns] #many NAs - change the NAs for 0s
wildevidences<-dat[,wildevidencecolumns]
wildevidences[is.na(wildevidences)]<-0
for (i in wildevidencecolumns){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}

wildevidences$anywildevidence<-ifelse(wildevidences$EvidenceFeralCat==-1 | wildevidences$EvidencePossum ==-1 | wildevidences$EvidenceHedgehog ==-1 |
                                        wildevidences$EvidenceWildRats ==-1 | wildevidences$EvidenceWildMice ==-1 | wildevidences$EvidenceFerret ==-1 |
                                        wildevidences$EvidenceWildRabbit ==-1 | wildevidences$EvidenceWildDeer ==-1 | wildevidences$EvidenceWildGoat ==-1 |
                                        wildevidences$EvidenceWildPig ==-1 | wildevidences$EvidenceOthers ==-1 , -1,0)

dat$anywildevidence<-wildevidences$anywildevidence
ctab(as.factor(dat$anywildevidence),as.factor(dat$Type))

test.fisher6<-fisher.test(ctab(as.factor(dat$anywildevidence),as.factor(dat$Type))$table)
anyevidencewild<-(c("AnyEvidence",as.numeric(test.fisher6$estimate[1]),as.numeric(test.fisher6$conf.int[1]),as.numeric(test.fisher6$conf.int[2])))
anyevidencewild

#Creating a vector with the columns of wild animals
wildevidencecolumns<-c(wildevidencecolumns,which(names(dat)=="anywildevidence"))


#contingency tables
comparison<-tibble(1,2,3,4,5,6,7,8,9,10,11)
for (i in (wildevidencecolumns)){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}

comparisonwildevidence<-as.data.frame(matrix(ncol=4,nrow=length(wildevidencecolumns)))
names(comparisonwildevidence)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonwildevidence
comparisonwildevidence$AnimalGroup<-names(dat[wildevidencecolumns])
comparisonwildevidence

head(dat[wildevidencecolumns])

# #fisher tests with the confidence intervals
for (i in 1:length(wildevidencecolumns)){
  comparisonwildevidence[i,1]<-names(dat[wildevidencecolumns[i]])
  comparisonwildevidence[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,wildevidencecolumns[i]]),dat$Type)$table)$estimate)
  comparisonwildevidence[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,wildevidencecolumns[i]]),dat$Type)$table)$conf.int[1])
  comparisonwildevidence[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,wildevidencecolumns[i]]),dat$Type)$table)$conf.int[2])
  print(names(dat[wildevidencecolumns[i]]))
  print(as.numeric(fisher.test(ctab(as.factor(dat[,wildevidencecolumns[i]]),dat$Type)$table)$p.value))
}
comparisonwildevidence
str( comparisonwildevidence)
for (i in 2:4){ comparisonwildevidence[,i]<-as.numeric(comparisonwildevidence[,i])}
forestplot(labeltext = comparisonwildevidence$AnimalGroup, mean=comparisonwildevidence$fisherOR ,lower=comparisonwildevidence$Min.conf.int, 
           upper=comparisonwildevidence$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           clip = c(0,20), title="Wild animal evidence risk for leptospirosis notifications", 
           is.summary = c(rep(F,12),T), 
           boxsize = 1/exp(abs(log(comparisonwildevidence$Max.conf.int)-(log(comparisonwildevidence$Min.conf.int)))))


# Checking evidence of rodents throughout the year
dat$EvidenceAnyRodent
ggplot(data=dat, aes(x=IntervieworSickSolsticeDist, y=EvidenceAnyRodent, fill=Type ))+ geom_jitter(aes(colour = Type))

############################################################Trap activities######################
#setting or emptying traps - binding all variables in one***********************************
AnyActivitytraps<-cbind(dat[332+3*(0:9)],dat[364+3*(0:9)])
AnyActivitytraps #many NAs that seem to have the same meaning as 0
AnyActivitytraps[is.na(AnyActivitytraps)]<-0 #changing them to 0 
AnyActivitytraps

trapactivitycolumns<-c(332+3*(0:9),364+3*(0:9))
for (i in trapactivitycolumns){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}
dat[,trapactivitycolumns]
#making a column that creates a -1 if any trap activity is detected for any animal whether setting or emptying it.

summarytraps<-rep(0,nrow(dat))                                          
for (i in 1:nrow(dat)){                                             
  summarytraps[i]<-                                             
    ifelse(any(dat[i,trapactivitycolumns]==-1),-1,0) 
}
summarytraps
dat$AnyActivitytraps<-summarytraps

ctab(as.factor(dat$AnyActivitytraps),as.factor(dat$Type))

test.fisher7<-fisher.test(ctab(as.factor(dat$AnyActivitytraps),as.factor(dat$Type))$table)
AnyActivitytraps<-(c("AnyActivity",as.numeric(test.fisher7$estimate[1]),as.numeric(test.fisher7$conf.int[1]),as.numeric(test.fisher7$conf.int[2])))
AnyActivitytraps


#contingency tables
comparison<-tibble(1,2,3,4,5,6,7,8,9,10,11)
for (i in trapactivitycolumns){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}



trapscomparison<-list(1)
for (i in 1:length(trapactivitycolumns)) {
  trapscomparison[[i]]<-(data.frame(ctab(as.factor(dat[,trapactivitycolumns[i]]),dat$Type)$table)) 
  names(trapscomparison[[i]])<-c(names(dat[trapactivitycolumns[i]]),"Group", "Frequency")
  
}

trapscomparison


#creating a variable with the no exposure variables (cases and controls exposure equals 0)
noexposuretraps<-c(0)
for (i in 1:length(trapscomparison) ){
  noexposuretraps[i]<-all(trapscomparison[[i]] [which(trapscomparison[[i]][[1]] ==-1),3]==0)}


# some contingency tables had no positive values for exposure. they will be eliminated adding the variable noexposuretraps to the formula

comparisonactivitytraps<-as.data.frame(matrix(ncol=4,nrow=20))
names(comparisonactivitytraps)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonactivitytraps
comparisonactivitytraps$AnimalGroup<-names(dat[trapactivitycolumns])
comparisonactivitytraps<-rbind(comparisonactivitytraps,AnyActivitytraps)
comparisonactivitytraps





# #fisher tests with the confidence intervals
for (i in c(332+3*(0:9),364+3*(0:9))[noexposuretraps==0]){
  comparisonactivitytraps[ifelse(i<364,(i-329)/3,(i-331)/3),1]<-names(dat[i])
  comparisonactivitytraps[ifelse(i<364,(i-329)/3,(i-331)/3),2]<-as.numeric(fisher.test(ctab(as.factor(dat[,i]),dat$Type)$table)$estimate)
  comparisonactivitytraps[ifelse(i<364,(i-329)/3,(i-331)/3),3]<-as.numeric(fisher.test(ctab(as.factor(dat[,i]),dat$Type)$table)$conf.int[1])
  comparisonactivitytraps[ifelse(i<364,(i-329)/3,(i-331)/3),4]<-as.numeric(fisher.test(ctab(as.factor(dat[,i]),dat$Type)$table)$conf.int[2])
}
comparisonactivitytraps
str( comparisonactivitytraps)
for (i in 2:4){ comparisonactivitytraps[,i]<-as.numeric(comparisonactivitytraps[,i])}
forestplot(labeltext = comparisonactivitytraps$AnimalGroup, mean=comparisonactivitytraps$fisherOR ,lower=comparisonactivitytraps$Min.conf.int, 
           upper=comparisonactivitytraps$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           clip = c(0,20), title="Risk for Setting or Emptying traps", 
           is.summary = c(rep(F,20),T), 
           boxsize = 1/exp(abs(log(comparisonactivitytraps$Max.conf.int)-(log(comparisonactivitytraps$Min.conf.int)))))

### Dividing setting traps from emptying traps
settingtraps<-dat[332+3*(0:9)]
emptyingtraps<-dat[364+3*(0:9)]
names(settingtraps)
names(emptyingtraps)

#making a column that creates a -1 if any trap activity is detected for any animal whether setting or emptying it.

settraps<-rep(0,nrow(dat))

for (i in 1:nrow(dat)){                                             
  settraps[i]<-                                             
    ifelse(any(settingtraps[i,]==-1),-1,0) 
}
settraps


emptytraps<-rep(0,nrow(dat))

for (i in 1:nrow(dat)){                                             
  emptytraps[i]<-                                             
    ifelse(any(emptyingtraps[i,]==-1),-1,0) 
}
emptytraps


dat$settraps<-settraps
dat$emptytraps<-emptytraps

ctab(as.factor(dat$settraps),as.factor(dat$Type))
ctab(as.factor(dat$emptytraps),as.factor(dat$Type))


setemptytraps<-as.data.frame(matrix(ncol=4,nrow=2))
names(setemptytraps)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
setemptytraps
setemptytraps$AnimalGroup<-names(dat[c(which(names(dat)=="settraps"),which(names(dat)=="emptytraps"))])
setemptytraps<-rbind(setemptytraps,AnyActivitytraps)
setemptytraps



# #fisher tests with the confidence intervals
for (i in c(which(names(dat)=="settraps"),which(names(dat)=="emptytraps"))){
  setemptytraps[ifelse(names(dat)[i]=="settraps",1,2),1]<-names(dat[i])
  setemptytraps[ifelse(names(dat)[i]=="settraps",1,2),2]<-as.numeric(fisher.test(ctab(as.factor(dat[,i]),dat$Type)$table)$estimate)
  setemptytraps[ifelse(names(dat)[i]=="settraps",1,2),3]<-as.numeric(fisher.test(ctab(as.factor(dat[,i]),dat$Type)$table)$conf.int[1])
  setemptytraps[ifelse(names(dat)[i]=="settraps",1,2),4]<-as.numeric(fisher.test(ctab(as.factor(dat[,i]),dat$Type)$table)$conf.int[2])
}
setemptytraps
str( setemptytraps)
for (i in 2:4){ setemptytraps[,i]<-as.numeric(setemptytraps[,i])}
forestplot(labeltext = setemptytraps$AnimalGroup, mean=setemptytraps$fisherOR ,lower=setemptytraps$Min.conf.int, 
           upper=setemptytraps$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           clip = c(0,20), title="Risk for Setting or Emptying traps", 
           is.summary = c(rep(F,2),T), 
           boxsize = 1/exp(abs(log(setemptytraps$Max.conf.int)-(log(setemptytraps$Min.conf.int)))))

################################################Any Exposure to Wild Animal######################
# Categorizing any exposure (contact, evidence or setting or emptying traps) to each wild species
wildanimals<-c("Possum","Hedgehog","WildRats","WildMice","Ferret","WildRabbits","WildDeer","WildGoat","FeralCats") 

names(dat[,grepl(paste(wildanimals, collapse="|"),names(dat), ignore.case = T)&!grepl("days|type",names(dat), ignore.case = T)])

#creating dataframe for the exposure to each Wild animal group
wildspecies<-as.data.frame(matrix(ncol=9,nrow=nrow(dat)))
names(wildspecies)<-wildanimals
for (i in 1:9) {
  for (j in which( grepl(paste(wildanimals[i], collapse="|"),names(dat), ignore.case = T)&!grepl("days|type",names(dat), ignore.case = T))) { for (k in (1:nrow(dat))) {wildspecies[k,i]<- ifelse(any(dat[k,j]==-1),-1,0)}}}

wildspecies  

wildspecies<-cbind(dat[names(dat)=="Type"],wildspecies)


for (i in 1:nrow(wildspecies)) {
  wildspecies$AnyWildAnimal[i]<- ifelse(any(wildspecies[i,2:10]==-1),-1,0)}
wildspecies$AnyWildAnimal


#contingency tables of exposure to wild animals divided by species

wildspeciescomparison<-list(1)
for (i in 2:11) {
  wildspeciescomparison[[i-1]]<-(data.frame(ctab(as.factor(wildspecies[,i]),wildspecies$Type)$table)) 
  names(wildspeciescomparison[[i-1]])<-c(names(wildspecies[i]),"Group", "Frequency")
}

wildspeciescomparison

noexposurewildspecies<-c(0)
for (i in 1:length(wildspeciescomparison) ){
  noexposurewildspecies[i]<-all(wildspeciescomparison[[i]] [which(wildspeciescomparison[[i]][[1]] ==-1),3]==0)}
noexposurewildspecies


comparisonactivitywildspecies<-as.data.frame(matrix(ncol=4,nrow=ncol(wildspecies)-1))
names(comparisonactivitywildspecies)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonactivitywildspecies



for (i in c(2:11)[noexposurewildspecies==0]){
  comparisonactivitywildspecies[i-1,2]<-as.numeric(fisher.test(ctab(as.factor(wildspecies[,i]),wildspecies$Type)$table)$estimate)
  comparisonactivitywildspecies[i-1,3]<-as.numeric(fisher.test(ctab(as.factor(wildspecies[,i]),wildspecies$Type)$table)$conf.int[1])
  comparisonactivitywildspecies[i-1,4]<-as.numeric(fisher.test(ctab(as.factor(wildspecies[,i]),wildspecies$Type)$table)$conf.int[2])
}
comparisonactivitywildspecies$AnimalGroup<-c(wildanimals,"AnySpecies")

comparisonactivitywildspecies
str( comparisonactivitywildspecies)

for (i in 2:4){ comparisonactivitywildspecies[,i]<-as.numeric(comparisonactivitywildspecies[,i])}
forestplot(labeltext = comparisonactivitywildspecies$AnimalGroup, mean=comparisonactivitywildspecies$fisherOR ,lower=comparisonactivitywildspecies$Min.conf.int, 
           upper=comparisonactivitywildspecies$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           clip = c(0,20), title="Wild animal species exposure risk of leptospirosis cases", 
           is.summary = c(rep(F,9),T), 
           boxsize = 1/exp(abs(log(comparisonactivitywildspecies$Max.conf.int)-(log(comparisonactivitywildspecies$Min.conf.int)))))

############################Scores/Number of days with Wild animal exposure#################
#creating a score for activity with traps
#some values over 30 (999s) - has to be corrected, otherwise working with numerical values will have an interference. Even when the exposure with traps was positive, some 999s occurred (maybe respondents were unsure(?))
dat[c(333+3*(0:9),365+3*(0:9))][dat[c(332+3*(0:9),364+3*(0:9))]==-1&dat[c(333+3*(0:9),365+3*(0:9))]>30]
#3 "999" values - they will be modified to Nas

for (i in c(333+3*(0:9),365+3*(0:9))) {
  dat[is.na(dat[,i])|dat[i]==999,i]<-NA }


score.wild.traps<-as.data.frame(matrix(ncol=length(c(332+3*(0:9),364+3*(0:9))),nrow=nrow(dat)))
names(score.wild.traps)<-names(dat[c(332+3*(0:9),364+3*(0:9))])
head(score.wild.traps)

for (i in c(332+3*(0:9),364+3*(0:9))) { for (j in 1:nrow(dat)){
  score.wild.traps[j,ifelse(i<364,(i-329)/3,(i-331)/3)]<-as.numeric(dat[j,i])*-as.numeric(dat[j,i+1])
}}

score.wild.traps$Totalscore<-apply(score.wild.traps,1,sum, na.rm=T)
dat$wildtrapsScore<-score.wild.traps$Totalscore
hist(dat$wildtrapsScore)
# the distribution does not seem to behave as normal distribution. I will try to apply a non-parametric test. In this case, Wilcoxon signed rank sum test
dat$wildtrapsScore[dat$Type=="Case"]
dat$wildtrapsScore[dat$Type=="Control"]
# values with no zeros: 
dat$wildtrapsScore[dat$Type=="Control"&dat$wildtrapsScore!=0]
dat$wildtrapsScore[dat$Type=="Case"&dat$wildtrapsScore!=0]
#means and medians for all values
mean(dat$wildtrapsScore[dat$Type=="Case"]) ; median(dat$wildtrapsScore[dat$Type=="Case"]) 
mean(dat$wildtrapsScore[dat$Type=="Control"]); median(dat$wildtrapsScore[dat$Type=="Control"])
#means and medians excluding the non=exposures (sum of exposure days=0)
mean(dat$wildtrapsScore[dat$Type=="Case"&dat$wildtrapsScore!=0]); median(dat$wildtrapsScore[dat$Type=="Case"&dat$wildtrapsScore!=0])
mean(dat$wildtrapsScore[dat$Type=="Control"&dat$wildtrapsScore!=0]);median(dat$wildtrapsScore[dat$Type=="Control"&dat$wildtrapsScore!=0])

#the mean and median had little difference from Case to control
#wilcoxon test: 
wilcox.test(dat$wildtrapsScore[dat$Type=="Case"], dat$wildtrapsScore[dat$Type=="Control"])
# the wilcoxon signed rank test ( Mann-Whitney) was insignifficant for difference in means.
wilcox.test(dat$wildtrapsScore[dat$Type=="Case"&dat$wildtrapsScore!=0], dat$wildtrapsScore[dat$Type=="Control"&dat$wildtrapsScore!=0])
#also insignifficant

#boxplot including 0s
boxplot(dat$wildtrapsScore[dat$Type=="Case"], dat$wildtrapsScore[dat$Type=="Control"])
#boxplot excluding 0s
boxplot(dat$wildtrapsScore[dat$Type=="Case"&dat$wildtrapsScore!=0], dat$wildtrapsScore[dat$Type=="Control"&dat$wildtrapsScore!=0])


ggplot(dat,aes(x=Type,y=wildtrapsScore)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Wild traps score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=20)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


# Subsetting Wild animal scores
score.wild.contact<-cbind(QID, types, score.wild.contact)
score.wild.contact
narrow.score.wild.contact<-gather(score.wild.contact,key = "Species" , value = "Days", 3:ncol(score.wild.contact))
narrow.score.wild.contact
narrow.score.wild.contact$Species<-factor(narrow.score.wild.contact$Species, levels = unique(narrow.score.wild.contact$Species) )
narrow.score.wild.contact
str(narrow.score.wild.contact)  

narrow.score.wild.contact

ggplot(narrow.score.wild.contact,aes(x=Species,y=Days, fill=types)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Wild animals contact score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=8)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


ggplot(narrow.score.wild.contact[!(narrow.score.wild.contact$Days==0),],aes(x=Species,y=Days, fill=types)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Wild animals contact score (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=8)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#Set / empty traps
head(dat[c(332+3*(0:9),364+3*(0:9))+1])  
score.wild.traps[is.na(score.wild.traps)]<-0
score.wild.traps
score.wild.traps<-cbind(QID, types, score.wild.traps)
head(score.wild.traps)
narrow.score.wild.traps<-gather(score.wild.traps,key = "Activity" , value = "Days", 3:ncol(score.wild.traps))
head(narrow.score.wild.traps)
str(narrow.score.wild.traps)
narrow.score.wild.traps$Activity<-factor(narrow.score.wild.traps$Activity, levels = unique(narrow.score.wild.traps$Activity) )
narrow.score.wild.traps

ggplot(narrow.score.wild.traps,aes(x=Activity,y=Days, fill=types)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Wild animals trap activities (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=8)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

ggplot(narrow.score.wild.traps[!narrow.score.wild.traps$Days==0,],aes(x=Activity,y=Days, fill=types)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Wild animal trap activities (days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=8)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


comparisonwildcontact
comparisonwildevidence
comparisonactivitytraps



##############################Protective equipment for Wild animal exposure#####################
#these are the participants that will be investigatedfor the use of protective equipment against wild animal exposure. Only the people who had wild animal exposure will be used in the case-control comparison for PPE Use.
dat$wildPPEparticipants<-ifelse(dat$emptytraps==-1|dat$settraps==-1|dat$contactwithwild==-1,-1,0)
ctab(as.factor(dat$wildPPEparticipants), dat$Type)
fisher.test(ctab(as.factor(dat$wildPPEparticipants), dat$Type)$table)
names(dat[which(grepl("Wild", names(dat))&grepl("PPE", names(dat)))])
dat$PPEWildOthersSpecify[!is.na(dat$PPEWildOthersSpecify)]    
dat[dat$PPEWildOthers==-1&!is.na(dat$PPEWildOthers),"PPEWildOthersSpecify"]
dat$PPEWildUnsure
wildPPEparticipantsdf<-dat[dat$wildPPEparticipants==-1, c(which(grepl("Wild", names(dat))&grepl("PPE", names(dat))&!grepl("Unsure|None|Specify", names(dat))),match("Type",names(dat)))]
wildPPEparticipantsdf[is.na(wildPPEparticipantsdf)|wildPPEparticipantsdf==999]<-0
anywildPPE<-apply(wildPPEparticipantsdf,1,function(x)ifelse(any(x==-1),-1,0))
wildPPEparticipantsdf$anywildPPE<-anywildPPE
wildPPEcolumns<-c(1:6,8)


wildPPEparticipantsdf


for (i in (wildPPEcolumns)){
  comparison<-ctab(as.factor(wildPPEparticipantsdf[,i]),wildPPEparticipantsdf$Type)
  print(names(wildPPEparticipantsdf[i]))
  print(comparison)
}

comparisonwildPPE<-data.frame(matrix(ncol=4,nrow=length(wildPPEcolumns)))
names(comparisonwildPPE)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonwildPPE
comparisonwildPPE$AnimalGroup<-names(wildPPEparticipantsdf[wildPPEcolumns])
comparisonwildPPE
#fisher tests with the confidence intervals
for (i in 1:length(wildPPEcolumns)) {
  comparisonwildPPE[i,1]<-names(wildPPEparticipantsdf[wildPPEcolumns[i]])
  tryCatch({  comparisonwildPPE[i,2]<-as.numeric(fisher.test(ctab(as.factor(wildPPEparticipantsdf[,wildPPEcolumns[i]]),wildPPEparticipantsdf$Type)$table)$estimate)
  comparisonwildPPE[i,3]<-as.numeric(fisher.test(ctab(as.factor(wildPPEparticipantsdf[,wildPPEcolumns[i]]),wildPPEparticipantsdf$Type)$table)$conf.int[1])
  comparisonwildPPE[i,4]<-as.numeric(fisher.test(ctab(as.factor(wildPPEparticipantsdf[,wildPPEcolumns[i]]),wildPPEparticipantsdf$Type)$table)$conf.int[2]) } , error=function(e){})
}
comparisonwildPPE

for (i in 2:4){ comparisonwildPPE[,i]<-as.numeric(comparisonwildPPE[,i])}


#########drawing a forest plot with the ORs and confidence intervals
forestplot(labeltext = comparisonwildPPE$AnimalGroup, mean=comparisonwildPPE$fisherOR,lower=comparisonwildPPE$Min.conf.int, upper=comparisonwildPPE$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           is.summary = c(rep(F,6),T), 
           boxsize = 1/exp(abs(log(comparisonwildPPE$Max.conf.int)-(log(comparisonwildPPE$Min.conf.int)))),
           clip = c(0,20), title="wildPPE activities risk of leptospirosis")

##################Other type of contact with animals###################
otheran<-match(c("HuntAnimals","SkinAnimals","DeadAnimals","AbortedAnimals","AnimalFeed","RawMilk","ExposureSoilManure"),names(dat))
names(dat[otheran])
otheran

####### changing Nas, 99s and 999s to 0s
for (i in otheran){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}

###contingency tables
for (i in otheran){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}

#Fisher ORs table
comparisonotheran<-as.data.frame(matrix(ncol=4,nrow=5))
names(comparisonotheran)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonotheran
#comparisonotheran$AnimalGroup<-names(otheran)
comparisonotheran

#fisher tests with the confidence intervals
for (i in 1:length(otheran)){
  comparisonotheran[i,1]<-names(dat[otheran[i]])
  comparisonotheran[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,otheran[i]]),dat$Type)$table)$estimate)
  comparisonotheran[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,otheran[i]]),dat$Type)$table)$conf.int[1])
  comparisonotheran[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,otheran[i]]),dat$Type)$table)$conf.int[2])
}




comparisonotheran
str( comparisonotheran)
for (i in 2:4){ comparisonotheran[,i]<-as.numeric(comparisonotheran[,i])}
forestplot(labeltext = comparisonotheran$AnimalGroup, mean=comparisonotheran$fisherOR ,lower=comparisonotheran$Min.conf.int, 
           upper=comparisonotheran$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           clip = c(0,20), title="Other types of animal contact risk of leptospirosis ", 
           is.summary = c(rep(F,5)), 
           boxsize = 1/exp(abs(log(comparisonotheran$Max.conf.int)-(log(comparisonotheran$Min.conf.int)))))


#########################################Animal Feed#######

# Yes/No contact analysis 
dat$AnimalFeed

dat[115,459:475] #this answer should be considered as Yes (-1)
dat[115,459]<--1

ctab(as.factor(dat$AnimalFeed), as.factor(dat$Type))
fisher.test(ctab(as.factor(dat$AnimalFeed), as.factor(dat$Type))$table)

#number of days for each feed contact
animalfeeddays<-461+4*(0:3)
names(dat[animalfeeddays])

#Transforming NAs to 0s
for (i in 1:nrow(dat)) { for (j in animalfeeddays) {
  dat[i,j]<-ifelse(dat[i,j]==999|dat[i,j]==99|is.na(dat[i,j]),0, dat[i,j]) } }
dat[animalfeeddays]
names(dat[animalfeeddays])



# Type of Feed.
Feedtype<-which(grepl(pattern ="FeedType",x=names(dat), ignore.case = T))
head(dat[Feedtype])
names(dat[Feedtype[1]-1])

#Everyone that answered not having contact with Feed should have these fields empty:
dat[which(dat[Feedtype[1]-1]==0),Feedtype]

#Some 999s have the same meaning as Nas. We will change them to NA.
dat[Feedtype][dat[Feedtype]==999]<-NA
#all the values that were 0 in column "contact with feed Y/N" ,  have Nas - that was the expected
dat[which(dat[Feedtype[1]-1]==-1),Feedtype]

#there are some Nas. We will have to change these Nas to "Unknown". There are also some "O"s, "B"s and "W"s which were probably incorrectly inserted. "Feedblend" is also insufficient information. All of them will be changed to Unknown. 

#inserting "unknown" on factor levels from columns.
for (j in 1:length(Feedtype)) { levels(dat[Feedtype][,j])<-c(levels(dat[Feedtype][,j]),"Unknown") }
dat[,Feedtype][!is.na(dat[,Feedtype])&(dat[,Feedtype]=="W"|dat[,Feedtype]=="O"|dat[,Feedtype]=="B")|dat[,Feedtype]=="Feed blend"]<-"Unknown"
#converting Nas from animal feed positive contact respondents in first column of AnimalFeedtype to "Unknown
dat[which(dat[Feedtype[1]-1]==-1),Feedtype[1]][which(is.na(dat[which(dat[Feedtype[1]-1]==-1),Feedtype][,1]))]<-"Unknown"


sum(!is.na(gather(dat[Feedtype])[,2])) # that is the total number of responses regarding feed types.
gather(dat[Feedtype])[,2][!is.na(gather(dat[Feedtype])[,2])] #all  different responses
unique(gather(dat[Feedtype])[,2][!is.na(gather(dat[Feedtype])[,2])]) #all the unique responses

#Now I will create an algorithm to identify type of feed by strings
Cattlefeed<-c("Calf","Cow")
Sheepfeed<-c("Sheep")
Goatfeed<-c("Goat")
Horsefeed<-c("Horse")
Pigfeed<-c("Pig")
Poultryfeed<-c("Chicken","Bird")
Catfeed<-c("Cat")
Dogfeed<-c("Dog")
Milkfeed<-c("Milk")
Othervegetablefeed<-c("Silage","Bilage", "Bailage", "Hay", "Hat=y", "Crop", "Palm", "Barley", "Maize","grass")
Unknown<-c("Unknown")
livestockfeed<-c(Cattlefeed,Sheepfeed,Goatfeed, Horsefeed,Pigfeed,Poultryfeed)
livestockfeedpluscrop<-c("Cattlefeed","Sheepfeed","Goatfeed", "Horsefeed","Pigfeed","Poultryfeed","Milkfeed","Othervegetablefeed")
petfood<-c("Catfeed","Dogfeed")
allfeedsvector<-c(Cattlefeed,Sheepfeed,Goatfeed, Horsefeed,Pigfeed,Poultryfeed,Catfeed, Dogfeed, Milkfeed, Othervegetablefeed, Unknown)
allfeeds<-list(Othervegetablefeed,Milkfeed, Cattlefeed,Sheepfeed,Goatfeed, Horsefeed,Pigfeed,Poultryfeed,Catfeed, Dogfeed, Unknown)
names(allfeeds)<-c("Othervegetablefeed","Milkfeed","Cattlefeed","Sheepfeed","Goatfeed", "Horsefeed","Pigfeed","Poultryfeed","Catfeed", "Dogfeed","Unknown")
allfeeds
feedcount<-list(1)
for (i in 1:length(allfeeds)) {
  feedcount[[i]]<-sum(grepl(paste(allfeeds[[i]],collapse ="|"),x=gather(dat[Feedtype])[[2]], ignore.case =T))
  names(feedcount)[[i]]<-names(allfeeds)[[i]]}
feedcount 
#above is the count of number of answers per type of feed. 

#checking for values that weren`t chosen by any of the feed patterns.all the following values must be Nas, otherwise the values must be included in some pattern.
gather(dat[Feedtype])[[2]][!grepl(paste(rle(unlist(allfeeds))$values, collapse = "|"),x=gather(dat[Feedtype])[[2]],ignore.case=T)]

# looking for repeated values among the categories. 
for (i in 1:length(feedcount)){  
  print(gather(dat[Feedtype])[[2]][(grepl(paste(allfeeds[[which(names(feedcount)==names(feedcount)[i])]],collapse ="|"),x=gather(dat[Feedtype])[[2]],ignore.case=T))&(grepl(paste(Reduce(c, allfeeds[-i]),collapse ="|"),x=gather(dat[Feedtype])[[2]],ignore.case=T))])}
# these are the repeated values.

#remove values that were repeated in Othervegetablefeed and other category
gather(dat[Feedtype])[[2]][(grepl(paste(allfeeds[[which(names(feedcount)=="Othervegetablefeed")]],collapse ="|"),x=gather(dat[Feedtype])[[2]],ignore.case=T))&(grepl(paste(livestockfeed,collapse ="|"),x=gather(dat[Feedtype])[[2]],ignore.case=T))]
#othervegetablefeed may include values which were previously marked in another category. Going to remove those values.
feedcount[[which(names(feedcount)=="Othervegetablefeed")]] <-sum((grepl(paste(allfeeds[[which(names(feedcount)=="Othervegetablefeed")]],collapse ="|"),x=gather(dat[Feedtype])[[2]],ignore.case=T))&(!grepl(paste(livestockfeed,collapse ="|"),x=gather(dat[Feedtype])[[2]],ignore.case=T)))
feedcount

#remove values that were repeated in milk and other category
gather(dat[Feedtype])[[2]][(grepl(paste(allfeeds[[which(names(feedcount)=="Milkfeed")]],collapse ="|"),x=gather(dat[Feedtype])[[2]],ignore.case=T))&(grepl(paste(livestockfeed,collapse ="|"),x=gather(dat[Feedtype])[[2]],ignore.case=T))]
feedcount[[which(names(feedcount)=="Milkfeed")]] <-sum((grepl(paste(allfeeds[[which(names(feedcount)=="Milkfeed")]],collapse ="|"),x=gather(dat[Feedtype])[[2]],ignore.case=T))&(!grepl(paste(livestockfeed,collapse ="|"),x=gather(dat[Feedtype])[[2]],ignore.case=T)))

sum(unlist(feedcount))
#calculating the total score for number of days in contact with animal feed.
feeddaysscore<-dat[animalfeeddays]
feeddaysscore$TotalScore<-apply(X = feeddaysscore, FUN=function(x) sum(x,na.rm = T),MARGIN = 1)
feeddaysscore<-cbind(QID,types,feeddaysscore)
head(feeddaysscore)

# Doing one dataframe for categorizinng each of the answers. Some respondents answered more than once. So all the answers have to be captured.
#dummy dataframe
feedanswerscategory<-data.frame(matrix(ncol = length(Feedtype)+1, nrow=nrow(dat)))
feedanswerscategory
#loop identifying string patterns for feed - they will be categorized in groups
for(i in 1:length(allfeeds)){ for (j in 1:length(Feedtype)) { for (k in 1:nrow(dat)) {
  feedanswerscategory[k,j]<-ifelse(grepl(paste(allfeeds[[i]], collapse = "|"), dat[Feedtype][k,j], ignore.case = T),names(allfeeds)[[i]], next)}}}
# this is the reslt dataframe
feedanswerscategory
# binding the dataframe with QID number, case/control column, the 4 columns from feedcontact score and the total score column.
feedanswerscategory<-cbind(dat[,c(1,7)], feedanswerscategory, dat[,animalfeeddays],feeddaysscore$TotalScore)
#changing the TotalScore colummn name 
names(feedanswerscategory)[12]<-"TotalScore"
head(feedanswerscategory)
#checking if the number of rows is according to the number of rows of the main dataframe
dim(feedanswerscategory); dim(dat)

#now it is a little more complicated step. we need to separate the dataframe in 2. and apply the gather function to each part of the dataframe. One for the feed types and the other for the score from each observation. In the end, both narrow dataframes need to have the same dimmensions so that they can be remerged again., 

narrowfeedanswerscategory<- gather(feedanswerscategory,key="N.Answer",value = "Feedtype", c(3:7))
narrowfeeddayscategory<- gather(feedanswerscategory,key="N.Answer",value = "Score", 8:12 )

dim(narrowfeeddayscategory)
dim(narrowfeedanswerscategory)
#same dimmensions - check how the dataframes were organized
head(narrowfeedanswerscategory)
head(narrowfeeddayscategory)

#binding columns to create one final dataframe.
narrowfeed.days.type<- cbind(narrowfeedanswerscategory[,c(1,2,9)], narrowfeeddayscategory[,c(8,9)])
head(narrowfeed.days.type)
tail(narrowfeed.days.type)

#the TotalScore values values are in the end of the dataframe. The Feedtype column has also to be named after TotalScore in the corresponding rows.

sum(narrowfeed.days.type$N.Answer=="TotalScore") #correct. the number of values have to be one for each respodent
narrowfeed.days.type[narrowfeed.days.type$N.Answer=="TotalScore",]$Feedtype<-"TotalScore"
narrowfeed.days.type

#Work further on the dataframe. We will categorize again the feed types in:Livestock feed , Pet food and Unknown.


#First, we will separate total score dataframe from the other feedtypes dataframe. We will have to work further on the dataframe, and the Totalscore values cannot be edited, otherwise the number of observations will change. We will remove them , work on the dataframe and then reinsert them.
Feedtotalscore<-narrowfeed.days.type[!is.na(narrowfeed.days.type$Feedtype)&narrowfeed.days.type$Feedtype=="TotalScore",]
#creating a dummy column so that the feedtotalscore dataframe cna be reinserted to the main feed dataframe (narrowfeed.days.type.categ).
Feedtotalscore$Category<-rep("TotalScore", nrow(Feedtotalscore))
#repeat the same values for the dataframe
Feedtotalscore$Cat.score<-Feedtotalscore$Score
#Checking an initial boxplot for the totalscore among cases and controls.
boxplot(Feedtotalscore$Cat.score~Feedtotalscore$Type)

#Remove TotalScore rows and keep the NAs. they will be important for the respondents that did not have contact with Feed. Their answers have to count towards the calculations and plots.
narrowfeed.days.type<-filter(narrowfeed.days.type,!narrowfeed.days.type$Feedtype=="TotalScore"|is.na(narrowfeed.days.type$Feedtype) )
narrowfeed.days.type
# If we maintain the feedtype as Na, they will be removed by the dplyr tool gather. So we will change to the name "Nofeed".
narrowfeed.days.type[is.na(narrowfeed.days.type$Feedtype),3]<-"Nofeed"
#change the NAs socres to 0. Because otherwise R will only calculate and plot answers that had contact. As we want to do with and without 0s, we have to include the 0s now and remove them when necessary.
head(narrowfeed.days.type)
#Make a wide table to create 0 values for all the categories. this is necessary because otherwise ggplot will only plot the non 0 values for each category.
Wide.feed.days.type<-spread(narrowfeed.days.type,key = Feedtype, value = Score)

head(Wide.feed.days.type)
#change NAs to 0s.
Wide.feed.days.type[is.na(Wide.feed.days.type)]<-0

#now that we have put 0s to all the variables, we change the dataframe back to narrow.
narrowfeed.days.type<-gather(Wide.feed.days.type,"Feedtype", "Score", 4:15)

# now we will insert high level categories for feed with ifelse commands. 
narrowfeed.days.type$livestock<-ifelse(grepl(paste(livestockfeedpluscrop, collapse = "|"),narrowfeed.days.type$Feedtype, ignore.case = T), narrowfeed.days.type$Score, 0  )
narrowfeed.days.type$petfood<-ifelse(grepl(paste(petfood, collapse = "|"),narrowfeed.days.type$Feedtype, ignore.case = T), narrowfeed.days.type$Score, 0  )
narrowfeed.days.type$unknown<-ifelse(grepl("unknown",narrowfeed.days.type$Feedtype, ignore.case = T), narrowfeed.days.type$Score, 0  )

head(narrowfeed.days.type)
# now that we have inserted the categories, we transform them to narrow.
narrowfeed.days.type.categ<-gather(narrowfeed.days.type,"Category", "Cat.score", 6:8)
head(narrowfeed.days.type.categ)  

#Reinsert the Total score results to the dataframe
narrowfeed.days.type.categ<-rbind(narrowfeed.days.type.categ,Feedtotalscore)

#Now we prepare thevalues to be plotted. here, I am changing the values to factors and defining the orders that they have to appear in the boxplot.
narrowfeed.days.type$Feedtype<- factor(narrowfeed.days.type$Feedtype, levels=c("Cattlefeed", "Sheepfeed", "Pigfeed", "Poultryfeed" , "Horsefeed" , "Goatfeed" , "Milkfeed" , "Othervegetablefeed" , "Dogfeed" ,"Catfeed", "Unknown", "TotalScore"))

narrowfeed.days.type.categ$Feedtype<- factor(narrowfeed.days.type.categ$Feedtype, levels=c("Cattlefeed", "Sheepfeed", "Pigfeed", "Poultryfeed" , "Horsefeed" , "Goatfeed" , "Milkfeed" , "Othervegetablefeed" , "Dogfeed" ,"Catfeed", "Unknown", "TotalScore"))

narrowfeed.days.type.categ$Category<-factor(narrowfeed.days.type.categ$Category, levels=c("livestock", "petfood", "unknown", "TotalScore"))

###This is the table comparing different categories against cases and controls. 
ctab(as.factor(narrowfeedanswerscategory$Feedtype), narrowfeedanswerscategory$Type )
#number of types of animal feed per respondent, among those who declared that had contact with animal feed
head(narrowfeed.days.type)
#Table to verifiy the number of answers ofr each respondent.
ctab(as.factor(narrowfeed.days.type[narrowfeed.days.type$Score>0,]$N.Answer))

ggplot(feeddaysscore,aes(x=types,y=TotalScore, fill=types)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Handling animal feed score (combined days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=20)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 15))

#if Visualizing the names is too difficult, press Ctrl + Shift + 6. Return to normal view with Ctrl+Alt+Shift+0
ggplot(na.omit(narrowfeed.days.type.categ),aes(x=Feedtype,y=Score, fill=Type)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Handling animal feed score (combined days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=15)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 15))

ggplot(na.omit(narrowfeed.days.type.categ[!narrowfeed.days.type.categ$Score==0,]),aes(x=Feedtype,y=Score, fill=Type)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Handling animal feed score (combined days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=15)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 15))


ggplot(na.omit(narrowfeed.days.type.categ),aes(x=Category,y=Cat.score, fill=Type)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Handling animal feed score (combined days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=10)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 15))

ggplot(narrowfeed.days.type.categ[!narrowfeed.days.type.categ$Cat.score==0,],aes(x=Category,y=Cat.score, fill=Type)) + geom_boxplot(varwidth=TRUE) + labs( x="", y="Handling animal feed score (combined days)") + theme(axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0.25)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_blank()) + theme(axis.title=element_text(size=20)) + theme(text = element_text(size=10)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 15))


##Using gloves when handling animal feed
#creating vector with column numbers for handling animal feed with gloves
animalfeedgloves<-animalfeeddays+2
head(dat[animalfeedgloves])
dat[animalfeedgloves]
# inserting Nas in 999s
dat[animalfeedgloves][dat[animalfeedgloves]==999&!is.na(dat[animalfeedgloves])]<-NA

#checking if values different from NAs were marked in respondents that had no contact with animal feed.
for (i in 1:nrow(dat)){ ifelse(dat[i,animalfeedgloves[1]-4]==0&any(!is.na(dat[i,animalfeedgloves])), print(dat[i,animalfeedgloves]),next)}
#although these rows were marked as 0 for animal feed contact, gloves were not used, which would make no sense. NAs should apply. 
#changing these values to NAs. 
for (i in 1:nrow(dat)){ for( k in 1:length(animalfeedgloves)){ dat[i,animalfeedgloves[k]]<-ifelse(dat[i,animalfeedgloves[1]-4]==0&!is.na(dat[i,animalfeedgloves[k]]), NA, dat[i,animalfeedgloves[k]])}}
dat[animalfeedgloves]

# create a dataframe to work just with animal feed gloves. We will use the dataframe that was created for 
data.feed.gloves<-dat[c(1,7,animalfeedgloves)]
head(data.feed.gloves)

narrow.data.feedgloves<-gather(data.feed.gloves,key = "N.answer", value = "GloveUse",3:6 )
narrow.data.feedgloves
#Contingency table for use of gloves and being a case
ctab(as.factor(narrow.data.feedgloves$GloveUse),as.factor(narrow.data.feedgloves$Type))
fisher.test(ctab(as.factor(narrow.data.feedgloves$GloveUse),as.factor(narrow.data.feedgloves$Type))$table)
#Very high odds-ratio. Similar issue with some PPE use.

######################WATER#EXPOSURES###############################################

##########################WATER SUPPLY#AND#TREATMENT#############
############################################home water#supply#and #reatment####
######## Home Water supply

names(dat[(476+2*(0:6))])
waterandtreatmentcolumns


watersourcecolumns<-which(grepl("Bore|Creek|WaterOther|Rain|Tanker|WaterTown", names(dat))&!grepl("Treat",names(dat)))


names(dat[waterandtreatmentcolumns])
names(dat[watersourcecolumns])
unsurewatercolumns<-which(grepl("Unsure", names(dat))&grepl("Water", names(dat)))
names(dat[unsurewatercolumns])


# access to untreated water.
names(dat[grepl("Treated", names(dat))])
treatmentcolumns<-grep("Treated", names(dat))

#Creating a variable called access to untreated water. If a respondent answered untreated water in any question, will return -1.
dat$AccessUntreatedWater<-factor(apply(dat[treatmentcolumns],1,function(x)ifelse(any(x==0,na.rm = T),-1,0)))
#contingency table for access to untreated water:
ctab(dat$AccessUntreatedWater,dat$Type)$table

#OR for access to untreated water
fisher.test(ctab(dat$AccessUntreatedWater,dat$Type)$table)


#remove Nas, 99s and 999s from water sources. But for water treatment, keep NAs and change 99s and 999s to NAs. It means the respondent is unsure. 
for (i in watersourcecolumns) {
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}
for (i in unsurewatercolumns){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}

for (i in treatmentcolumns){
 dat[!is.na(dat[,i])&(dat[i]==99|dat[i]==999),i]<-NA
}

#Analysing number of water sources and how we can describe the data.

morethanonesourcehome<-ifelse(as.numeric(dat$WaterTown)+as.numeric(dat$WaterBore)+as.numeric(dat$WaterTanker)+ 
                                as.numeric(dat$WaterRain)+as.numeric(dat$WaterCreek)+
                                as.numeric(dat$WaterOther)+as.numeric(dat$WaterUnsure)< -1,-1,0)

sum(morethanonesourcehome)  ##there are several respondents with more than one source of water at home


morethanonesource<-dat[morethanonesourcehome==-1,c(1,7,(476+2*(0:6)))] ## these are the respondents with more than one source
morethanonesource

onesource<-dat[morethanonesourcehome==0,c(1,7,(476+2*(0:6)))] ## these are the respondents with one source
onesource

#create a narrow table with one column for all the respondents with only one source of water supply
onesourcenarrow<-gather(onesource,"Source","Total",3:9)
dim(onesourcenarrow)
head(onesourcenarrow)
#Removing the rows with 0 as value
onesourcenarrow<-onesourcenarrow[-which(onesourcenarrow$Total==0),]
dim(onesourcenarrow)
head(onesourcenarrow)
#contingency table for respondents with one source divided by case/control
ctab(as.factor(onesourcenarrow$Source),as.factor(onesourcenarrow$Type))
fisher.test(ctab(as.factor(onesourcenarrow$Source),as.factor(onesourcenarrow$Type))$table) ##significantly different values between cases and controls 

#creating a table for more than one water supply
morethanonesource
morethanonesourcenarrow<-gather(morethanonesource,"Source", "Total",3:9)
dim(morethanonesourcenarrow)
morethanonesourcenarrow
morethanonesourcenarrow<-morethanonesourcenarrow[-which(morethanonesourcenarrow$Total==0),]
dim(morethanonesourcenarrow)
morethanonesourcenarrow
#contingency table for sources of respondents with more than one source divided by case/control
ctab(as.factor(onesourcenarrow$Source),as.factor(onesourcenarrow$Type))
fisher.test(ctab(as.factor(onesourcenarrow$Source),as.factor(onesourcenarrow$Type))$table)  #significantly different

#water supply from any number of water sources
watersource<-dat[c(1,7,476+2*(0:6))]
watersource
dim(watersource)
narrowwatersource<-gather(watersource,"Source","Total",3:9)
dim(narrowwatersource)
head(narrowwatersource)
#Removing rows with 0s
narrowwatersource<-narrowwatersource[-which(narrowwatersource$Total==0),]
head(narrowwatersource)
dim(narrowwatersource)
#contingency table for respondents with any number of sources divided by case/control
ctab(as.factor(narrowwatersource$Source),as.factor(narrowwatersource$Type))$table
fisher.test(ctab(as.factor(narrowwatersource$Source),as.factor(narrowwatersource$Type))$table) #siginificantly different

#seing the comment section about home water supply
dat[dat[486]!=0, c(486,490)]
#This respondent answered "well" for the water source. I am going change it to water bore and remove it from other
dat[217,c(waterandtreatmentcolumns,489,490,505)]
dat[217,grepl("Bore",names(dat))&!grepl("Treated",names(dat))]<--1
dat[217,grepl("WaterOther",names(dat))&!grepl("Treated",names(dat))]<-0

dat[dat[486]!=0, c(486,490)] #now no other comments made by people that responded they had other sources of water supply at home (by September, 2021 - please review). 
#see other comments from WaterComments variable
othersourceshome<-dat[which(!is.na(dat[490])), c(1,7,476+2*(0:6),490)]
othersourceshome
narrowothersourceshome<-gather(othersourceshome,"Source","Total",3:9)
narrowothersourceshome
narrowothersourceshome<-narrowothersourceshome[-which(narrowothersourceshome$Total==0),]
narrowothersourceshome #these are all the comments about Home water supply with the respective 
#water source and study group

# contingency tables comparing home water sources and study group (case/control)
for (i in (476+2*(0:6))) {
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}





################### test for water treatment at home

names(dat[476:489])

#creating a variable that returns is "-1" when the water is treated, "0" when the water is not treated and, returns NA when the 
#water source was not used by the respondent 
treatedwater<-data.frame(c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))))

treatedwater
for (i in 476+2*(0:6)) {
  treatedwater[,ceiling((i-475)/2)]<- ifelse(dat[i]==-1 & dat[i+1]==-1,-1,ifelse(dat[i]==-1 & dat[i+1]==0,0,NA))
}
head(treatedwater)

#adding control and cases column to the treated water dataframe
types<-dat$Type
treatedwater<-cbind(types,treatedwater)
head(treatedwater)

#creating a variable that sums the number of treatments
summ.treated<-data.frame(c(rep(0,nrow(dat))))

for (i in 1:nrow(dat)) {
  summ.treated[i,]<-sum(treatedwater[i,2:8], na.rm=T)
}
summ.treated

#turning the variables with -1 and 0 as factors in the treatedwater data frame to work with frequency
ftreated<-data.frame(c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))))
for (i in 1:8) {
  ftreated[i]<-as.factor(treatedwater[,i])
}
head(ftreated)
names(ftreated)<-names(dat[c(7,(476+2*(0:6)))])
head(ftreated)
ftreatednarrow<-gather(ftreated,"Source", "Total", 2:8)
str(ftreatednarrow)
head(ftreatednarrow)
unlist(ftreatednarrow$Total)
ftreatednarrow$Source<-as.factor(ftreatednarrow$Source)
ftreatednarrow$Total<-as.factor(ftreatednarrow$Total)
summary(ftreatednarrow)
##removing rows with NAs
ftreatednarrow<-ftreatednarrow[-which(is.na(ftreatednarrow$Total)),]
ftreatednarrow
summary(ftreatednarrow)

#contingency table for case/control against treated/untreated water
ctab(ftreatednarrow$Total,ftreatednarrow$Type)

#contingency table for types of source x treated/untreated
ctab(ftreatednarrow$Source,ftreatednarrow$Total)

############################################Work water#supply#and#treatment##### 

names(dat[491+2*(0:6)])

names(dat[waterandtreatmentcolumns])

# creating a variable with respondents with more than our source of water at work
morethanonesourceworkanswers<-ifelse(as.numeric(dat$WorkWaterTown)+as.numeric(dat$WorkWaterBore)+as.numeric(dat$WorkWaterTanker)+ 
                                as.numeric(dat$WorkWaterRain)+as.numeric(dat$WorkWaterCreek)+
                                as.numeric(dat$WorkWaterOther)+as.numeric(dat$WorkWaterUnsure)< -1,-1,0)

morethanonesourceworkanswers


sum(morethanonesourceworkanswers) ##there are several respondents with more than one source of water at work


morethanonesourcework<-dat[morethanonesourceworkanswers==-1,c(1,7,(491+2*(0:6)))] ## these are the respondents with more than one source
morethanonesourcework

onesourcework<-dat[morethanonesourceworkanswers==0,c(1,7,(491+2*(0:6)))] ## these are the respondents with one source
onesourcework

#create a narrow table with one column for all the respondents with only one source of water supply
onesourceworknarrow<-gather(onesourcework,"Source","Total",3:9)
dim(onesourceworknarrow)
head(onesourcenarrow)
onesourceworknarrow<-onesourceworknarrow[-which(onesourceworknarrow$Total==0),]
dim(onesourcenarrow)
onesourceworknarrow
#contingency table for respondents with one source of water supply at work split by case/control
ctab(as.factor(onesourceworknarrow$Source),as.factor(onesourceworknarrow$Type))
fisher.test(ctab(as.factor(onesourceworknarrow$Source),as.factor(onesourceworknarrow$Type))$table) #significantly different

#creating a table for more than one water supply
morethanonesourcework
morethanonesourceworknarrow<-gather(morethanonesourcework,"Source", "Total",3:9)
dim(morethanonesourceworknarrow)
morethanonesourceworknarrow
morethanonesourceworknarrow<-morethanonesourceworknarrow[-which(morethanonesourceworknarrow$Total==0),]
dim(morethanonesourceworknarrow)
morethanonesourceworknarrow
#contingency table for respondents with more that one source divided by case/control (regardless of the repetition of respondents )
ctab(as.factor(morethanonesourceworknarrow$Source),as.factor(morethanonesourceworknarrow$Type))

#water supply from any number of water sources
watersourcework<-dat[c(1,7,491+2*(0:6))]
watersourcework
dim(watersourcework)
narrowwatersourcework<-gather(watersourcework,"Source","Total",3:9)
dim(narrowwatersourcework)
head(narrowwatersourcework)
narrowwatersourcework<-narrowwatersourcework[-which(narrowwatersourcework$Total==0),]
head(narrowwatersourcework)
dim(narrowwatersourcework)
#contingency table for respondents with any number of sources divided by case/control at work
ctab(as.factor(narrowwatersourcework$Source),as.factor(narrowwatersourcework$Type))$table
fisher.test(ctab(as.factor(narrowwatersource$Source),as.factor(narrowwatersource$Type))$table)# significantly different

#seing comment section according to water source at work
othersourceswork<-dat[which(!is.na(dat[505]),505), c(1,7,491+2*(0:6),505)]
narrowothersourceswork<-gather(othersourceswork,"Source","Total",3:9)
narrowothersourceswork<-narrowothersourceswork[-which(narrowothersourceswork$Total==0),]
narrowothersourceswork


#All contingency tables for work water sources
for (i in 491+2*(0:6)) {
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}


################### test for water treatment at work

head(dat[491:504])
names(dat[491:504])

#creating a variable that returns is -1 when the water is treated, and 0 when the water is not treated.returns NA when the water source was not used by the respondent 
treatedwaterwork<-data.frame(c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))))

head(treatedwaterwork)

for (i in 491+2*(0:6)) {
  treatedwaterwork[,ceiling((i-490)/2)]<- ifelse(dat[i]==-1 & dat[i+1]==-1,-1,ifelse(dat[i]==-1 & dat[i+1]==0,0,NA))
}

head(treatedwaterwork)
#adding control and cases column to the treated water dataframe
treatedwaterwork<-cbind(types,treatedwaterwork)
head(treatedwaterwork)

#creating a variable that sums the number of treatments
summ.treatedwork<-data.frame(c(rep(0,nrow(dat))))

for (i in 1:nrow(dat)) {
  summ.treatedwork[i,]<-sum(treatedwaterwork[i,2:8], na.rm=T)
}
head(summ.treatedwork)

#turning the variables with -1 and 0 as factors to allow the calculation of frequencies and preparing the data frame for the calculation
head(treatedwaterwork)
ftreatedwork<-data.frame(c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))),c(rep(0,nrow(dat))))
for (i in 1:8) {
  ftreatedwork[i]<-as.factor(treatedwaterwork[,i])
}
head(ftreatedwork)
names(ftreatedwork)<-names(dat[c(7,(491+2*(0:6)))])
head(ftreatedwork)
##gathering the columns for treatment into one.
ftreatednarrowwork<-gather(ftreatedwork,"Source", "Total", 2:8)
str(ftreatednarrowwork)
head(ftreatednarrowwork)

unlist(ftreatednarrowwork$Total)
ftreatednarrowwork$Source<-as.factor(ftreatednarrowwork$Source)
ftreatednarrowwork$Total<-as.factor(ftreatednarrowwork$Total)
summary(ftreatednarrowwork) #The results do not differentiate source of water the next lines will do that job.
##removing rows with NAs
ftreatednarrowwork<-ftreatednarrowwork[-which(is.na(ftreatednarrowwork$Total)),]
ftreatednarrowwork
summary(ftreatednarrowwork)

#contingency table for case/control against treated/untreated water
ctab(ftreatednarrowwork$Total,ftreatednarrowwork$Type)
fisher.test(ctab(ftreatednarrowwork$Total,ftreatednarrowwork$Type)$table) #O.R. 0.47 - not significantly different

#contingency table for types of source x treated/untreated
ctab(ftreatednarrowwork$Source,ftreatednarrowwork$Total)

#########################Combined#home#and#work#water#supply#and#treatment ##################

sum(morethanonesourcehome)+sum(morethanonesourceworkanswers)  ##there are several responses with more than one source of water at home or work


morethanonesourceall<-dat[morethanonesourcehome==-1 & morethanonesourceworkanswers==-1,c(1,7,(476+2*(0:6)),(491+2*(0:6)))] 
## these are the respondents with more than one source at home and at work
morethanonesourceall 

#create a narrow table with one column for all the respondents with only one source of water supply
Home<-rep("home",dim(onesourcenarrow)[1])
onesourcenarrow<-cbind(Home,onesourcenarrow)
onesourcenarrow
Work<-rep("work",dim(onesourceworknarrow)[1])
onesourceworknarrow<-cbind(Work,onesourceworknarrow)
names(onesourceworknarrow)[1]<-"Location"
names(onesourcenarrow)[1]<-"Location"
onesourcenarrowall<-rbind(onesourcenarrow,onesourceworknarrow)
onesourcenarrowall$Source<-gsub("Work","",onesourcenarrowall$Source)

#contingency table for respondents with one source divided by case/control
ctab(as.factor(onesourcenarrowall$Source),as.factor(onesourcenarrowall$Type))
fisher.test(ctab(as.factor(onesourcenarrowall$Source),as.factor(onesourcenarrowall$Type))$table) #

#creating a table for more than one water supply
Home<-rep("home",dim(morethanonesourcenarrow)[1])
Work<-rep("work",dim(morethanonesourceworknarrow)[1])
morethanonesourcenarrow<-cbind(Home,morethanonesourcenarrow)
morethanonesourceworknarrow<-cbind(Work,morethanonesourceworknarrow)
morethanonesourcenarrow
morethanonesourceworknarrow
names(morethanonesourceworknarrow)[1]<-"Location"
names(morethanonesourcenarrow)[1]<-"Location"
morethanonesourceall<-rbind(morethanonesourcenarrow,morethanonesourceworknarrow)
morethanonesourceall$Source<-gsub("Work","",morethanonesourceall$Source)
morethanonesourceall


#contingency table for respondents with one source divided by case/control
ctab(as.factor(morethanonesourceall$Source),as.factor(morethanonesourceall$Type))
fisher.test(ctab(as.factor(morethanonesourceall$Source),as.factor(morethanonesourceall$Type))$table)

#water supply from any number of water sources
head(narrowwatersource)
head(narrowwatersourcework)
Home<-rep("home",dim(narrowwatersource)[1])
Work<-rep("work",dim(narrowwatersourcework)[1])
narrowwatersource<-cbind(Home,narrowwatersource)
narrowwatersourcework<-cbind(Work,narrowwatersourcework)
names(narrowwatersource)[1]<-"Location"
names(narrowwatersourcework)[1]<-"Location"
narrowatersourceall<-rbind(narrowwatersource,narrowwatersourcework)
narrowatersourceall$Source<-gsub("Work","",narrowatersourceall$Source)
dim(narrowatersourceall)
#contingency table for respondents with any number of sources divided by case/control
ctab(as.factor(narrowatersourceall$Source),as.factor(narrowatersourceall$Type))$table
chisq.test(ctab(as.factor(narrowatersourceall$Source),as.factor(narrowatersourceall$Type))$table)
fisher.test(ctab(as.factor(narrowatersourceall$Source),as.factor(narrowatersourceall$Type))$table)

################### test for water treatment at home or work

head(dat[476:489])
names(dat[476:489])

head(treatedwater)
head(treatedwaterwork)
Home<-rep("home",dim(treatedwater)[1])
Work<-rep("work",dim(treatedwaterwork)[1])
treatedwater<-cbind(Home,treatedwater)
treatedwaterwork<-cbind(Work,treatedwaterwork)
names(treatedwater)[1]<-"Location"
names(treatedwaterwork)[1]<-"Location"
head(treatedwater)
head(treatedwaterwork)
treatedwaterall<-rbind(treatedwater,treatedwaterwork)
treatedwaterall
dim(narrowatersourceall)

#Total number of water treatments per respondent
summ.treated+summ.treatedwork

#turning the variables with -1 and 0 as factors in the treatedwater data frame to work with frequency

head(ftreatednarrow)
head(ftreatednarrowwork)
Home<-rep("home",dim(ftreatednarrow)[1])
Work<-rep("work",dim(ftreatednarrowwork)[1])
ftreatednarrow<-cbind(Home,ftreatednarrow)
ftreatednarrowwork<-cbind(Work,ftreatednarrowwork)
names(ftreatednarrow)[1]<-"Location"
names(ftreatednarrowwork)[1]<-"Location"
ftreatednarrowall<-rbind(ftreatednarrow,ftreatednarrowwork)
ftreatednarrowall
ftreatednarrowall$Source<-gsub("Work","",ftreatednarrowall$Source)
str(ftreatednarrowall)
ftreatednarrowall$Source<-as.factor(ftreatednarrowall$Source)
summary(ftreatednarrowall)
#contingency table for water treatment of any source vs case or control
ctab(ftreatednarrowall$Total,ftreatednarrowall$Type)
fisher.test(ctab(ftreatednarrowall$Total,ftreatednarrowall$Type)$table)
#in this case crude OR was under 0, with marginal significance. Would Water Treatment be protective? 

ctab(ftreatednarrowall$Source,ftreatednarrowall$Total)



#Water treatment home and work comparison between cases and controls

worktreatmentcolumns<-names(dat[492+2*(0:6)])
hometreatmentcolumns<-names(dat[477+2*(0:6)])


treatmentintersection<-list(0)

for (i in 1:length(worktreatmentcolumns)) { treatmentintersection[[i]] <- ctab(as.factor(dat[!dat[,worktreatmentcolumns[i]] %in% c(99,999),worktreatmentcolumns[i]]),dat$Type[!dat[,worktreatmentcolumns[i]] %in% c(99,999)])$table 
names(treatmentintersection)[[i]]<-names(dat[worktreatmentcolumns[i]])
}
treatmentintersection
hometreatmentintersection<-list(0)

for (i in 1:length(hometreatmentcolumns)) { hometreatmentintersection[[i]] <- ctab(as.factor(dat[!dat[,hometreatmentcolumns[i]] %in% c(99,999),hometreatmentcolumns[i]]),dat$Type[!dat[,hometreatmentcolumns[i]] %in% c(99,999)])$table 
names(hometreatmentintersection)[[i]]<-names(dat[hometreatmentcolumns[i]])
}


treatmentintersection[[which(sapply(treatmentintersection,function(x)nrow(x)<2))]]<-rbind(treatmentintersection[[which(sapply(treatmentintersection,function(x)nrow(x)<2))]],c(0,0))

treatmentintersection
hometreatmentintersection



alltreatmentintersection<-list(0)
for (i in 1:length(hometreatmentintersection)) { 
  tryCatch({ alltreatmentintersection[[i]]<- hometreatmentintersection[[i]]+treatmentintersection[[i]] } , error=function(e){})
  names(alltreatmentintersection)[[i]]<-names(dat[hometreatmentcolumns[i]])
}
alltreatmentintersection

#Cases proportion of treated water by source
sapply(alltreatmentintersection, function(x)x[1,1]/(x[1,1]+x[2,1]))
#controls proportion of treated water by source
sapply(alltreatmentintersection, function(x)x[1,2]/(x[1,2]+x[2,2]))

#odds ratios for the water treatment by source
sapply(alltreatmentintersection,function(x)1/fisher.test(x)$estimate)
sapply(alltreatmentintersection,function(x)1/fisher.test(x)$conf.int)


#contingency tabls for water treatment
for (i in treatmentcolumns) {
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}

#Creating vectors diferentiating home and work water wources
worksourcecolumns<-which(grepl("WorkWater", names(dat))&!grepl("Other|Unsure|Comments|Treated", names(dat)))
homesourcecolumns<-which(grepl("Water", names(dat))&!grepl("Other|Unsure|Comments|Treated|Work|Activity|Flood|Effluent|Mud|Drainag|Wetland|Landsc|Lepto|Contact|Access|Situation|Troughs", names(dat)))

#I am going to do a new dataframe summarizing the water source from work and home. E.g.,If a participant has water source from town at work, and bore from home, then this participant will be shown as town and bore positive (-1) . 
allsourcedf<-data.frame(matrix(nrow=nrow(dat), ncol=length(homesourcecolumns)))

for (i in 1:length(homesourcecolumns)) { 
  allsourcedf[i]<-ifelse(dat[homesourcecolumns[i]]==-1|dat[worksourcecolumns[i]]==-1,-1,0) }
head(allsourcedf)

#Now from this dataframe, goin to calculate Fisher O.R.s
comparisonallsources<-as.data.frame(matrix(ncol=4,nrow=ncol(allsourcedf)))
names(comparisonallsources)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonallsources
comparisonallsources$AnimalGroup<-names(dat[homesourcecolumns])
comparisonallsources




for (i in 1:nrow(comparisonallsources) ){
  comparisonallsources[i,2]<-as.numeric(fisher.test(ctab(as.factor(allsourcedf[,i]),dat$Type)$table)$estimate)
  comparisonallsources[i,3]<-as.numeric(fisher.test(ctab(as.factor(allsourcedf[,i]),dat$Type)$table)$conf.int[1])
  comparisonallsources[i,4]<-as.numeric(fisher.test(ctab(as.factor(allsourcedf[,i]),dat$Type)$table)$conf.int[2])
}

comparisonallsources
str( comparisonallsources)
for (i in 2:4){ comparisonallsources[,i]<-as.numeric(comparisonallsources[,i])}
forestplot(labeltext = comparisonallsources$AnimalGroup, mean=comparisonallsources$fisherOR ,lower=comparisonallsources$Min.conf.int, 
           upper=comparisonallsources$Max.conf.int, xlab="Unadjusted Fisher's test odds-ratio with confidence intervals. Water source from home or work.", zero = 1,
           clip = c(0,20), title="Risk of water sources for leptospirosis notifications", 
           is.summary = c(rep(F,14),T), 
           boxsize = 1/exp(abs(log(comparisonallsources$Max.conf.int)-(log(comparisonallsources$Min.conf.int)))))


####RECREATIONAL/OTHER#ENVIRONMENTAL#WATER#EXPOSURES################

#Environmental water exposures
names(dat[(520+3*(0:6))])
waterexposurecolumns<-c(520+3*(0:6))

####variables for recreational waters
names(dat[(511+2*(0:3))])
recreationalcolumns<-c(511+2*(0:3),match(c("WaterContactOcean","WaterContactRiver","WaterContactOthers"),names(dat)))
names(dat[recreationalcolumns])

#some respondents have NA as answers
for (i in c(waterexposurecolumns,recreationalcolumns)){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}




#########there are values 1 and -1 to represent exposure. Will standardize to -1 for all of the values.
for (i in recreationalcolumns){
  dat[dat[i]==1,i]<--1
}

#CORRECTIONS ON THE DATABASE CONCERNING WATER EXPOSURES
#_____________________________________________________________________________________________________________________
#making changes to the column WaterSituationOther
### enumerate other Water exposures
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,520+3*6+3]
# #[1] "Water troughs for cattle from creek"
# [2] "Cleaning duck pond, digging fence holes"
# [3] "Cleaning and fixing water troughs"
# [4] "water from a dam"
# [5] "native bush"
# [6] "building sites"
# [7] "salt water"
# [8] "renovations to houses"
# [9] "Water troughs"
# [10] "Outside shower from creek"
# [11] "River crossing for hunting, drinking out of streams in backcountry"
# [12] "Mud and flood in the river he kayaked"
# [13] "River water"
# [14] "Fixed water trough on farm"
# [15] "Animal feaces splashes in dairy shed"
# [16] "Irrigation water"
# [17] "Water trough"
# [18] "See notes"
# [19] Potted plants with potting mix                                    
# [20] Water trough                                                      
# [21] Water leaks from troughs                                          
# [22] picking up hay and storing it                                     
# [23] walking through little streams through lake waikerimoana //       
# [24] Contact with river, which seemed dirty                            
# [25] gardening                                                         
# [26] Caving      

#Creating category Water Troughs
dat$WaterTroughs<-ifelse(grepl("Water troughs for cattle from creek|Cleaning and fixing water troughs|Water troughs|Fixed water trough on farm|Water trough|Water leaks from troughs",dat$WaterSituationSpecify),-1,0)

dat[grepl("Water troughs for cattle from creek|Cleaning and fixing water troughs|Water troughs|Fixed water trough on farm|Water trough|Water leaks from troughs",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0

#Already mentioned hiking - I believe this could be removed.
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),grep("Outdoor",names(dat)))][5,]
dat[grepl("native bush",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0

#already mentioned contact with salt water in WaterContactOcean
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),grep("Outdoor|Ocean|River",names(dat)))][7,]
dat[grepl("salt water",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0

#already mentioned contact with river, ocean, camping, swimming, fishing
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),grep("Outdoor|Ocean|River",names(dat)))][10,]
dat[grepl("Outside shower from creek",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0

# Changing river exposure in WaterContactRiver and removing the WaterSituationOther status
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(1,waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),which(names(dat)=="AnimalFeed"),grep("Outdoor|Ocean|River|ExposureSoil",names(dat)))][11,]
dat[grepl("River crossing for hunting",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0
dat[grepl("River crossing for hunting",dat$WaterSituationSpecify),which(names(dat)=="WaterContactRiver")]<-0

# He already answered river in WaterContactRiver
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),grep("Outdoor|Ocean|River",names(dat)))][13,]
dat[grepl("River water",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0

#I wouldn`t consider this a water exposure. Faeces splash has to do with livestock
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(1,waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),grep("Outdoor|Ocean|River",names(dat)))][15,]
dat[grepl("Animal feaces splashes in dairy shed",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0

# Does volunteer work for Water native plants from drain water and creek water using bucket and wildlife gloves
#Walked barefoot in the paddock with cows, 3 times, neighbour grazes his dairy cattle on his livestock paddock
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(1,waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),grep("Outdoor|Ocean|River|ExposureSoil|Barefoot",names(dat)))][18,]
dat[grepl("",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0



# Potted plants with potting mix, wouldn't it have something to do with gardening? Already responded yes to gardening.
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(1,waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),grep("Outdoor|Ocean|River|ExposureSoil",names(dat)))][19,]
dat[grepl("Potted plants with potting mix",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0

# picking up hay and storing it - already mentioned having contact with animal feed
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(1,waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),which(names(dat)=="AnimalFeed"),grep("Outdoor|Ocean|River|ExposureSoil",names(dat)))][22,]
dat[grepl("picking up hay and storing it",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0

#walking through little streams through lake waikerimoana - specified having contact with rivers and hiking.
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(1,waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),which(names(dat)=="AnimalFeed"),grep("Outdoor|Ocean|River|ExposureSoil",names(dat)))][23,]
dat[grepl("walking through little streams",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0

# Contact with river, which seemed dirty. Mentioned contact with river already
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(1,waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),which(names(dat)=="AnimalFeed"),grep("Outdoor|Ocean|River|ExposureSoil",names(dat)))][24,]
dat[grepl("Contact with river, which seemed dirty",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0

#Gardening is another variable and the respondent has already answered having this activity
dat[!is.na(dat[520+3*6+3])&!dat[520+3*6+3]==999,c(1,waterexposurecolumns,recreationalcolumns,which(names(dat)=="WaterSituationSpecify"),which(names(dat)=="AnimalFeed"),grep("Outdoor|Ocean|River|ExposureSoil",names(dat)))][25,]
dat[grepl("gardening",dat$WaterSituationSpecify),which(names(dat)=="WaterSituationOther")]<-0


dat[!is.na(dat$WaterContactSpecify)&!dat$WaterContactSpecify==999,match(c("Type","WaterContactSpecify"),names(dat))]
#I will include creek,lakes,streams, dams in the same category River
dat[grepl("creek|lake|dam|stream",dat$WaterContactSpecify,ignore.case = T),match(c("WaterContactRiver"),names(dat))]<- -1

dat[grepl("creek|lake|dam|stream",dat$WaterContactSpecify,ignore.case = T),match(c("WaterContactOthers"),names(dat))]<-0

dat[dat$WaterContactOthers==-1&!is.na(dat$WaterContactSpecify)&!dat$WaterContactSpecify==999,match(c("Type","WaterContactSpecify"),names(dat))]

#If person declared that had other contact with water and specified beach, then water would be from ocean, correct?
dat[dat$WaterContactOthers==-1&!is.na(dat$WaterContactSpecify)&dat$WaterContactSpecify=="beach",match(c("Type","WaterContactSpecify","WaterContactOcean","WaterActivitySwimming"),names(dat))]
dat[dat$WaterContactOthers==-1&!is.na(dat$WaterContactSpecify)&dat$WaterContactSpecify=="beach",match(c("WaterContactOcean"),names(dat))]<- -1
dat[dat$WaterContactOthers==-1&!is.na(dat$WaterContactSpecify)&dat$WaterContactSpecify=="beach",match(c("WaterContactOthers"),names(dat))]<-0


#Is swimming in swimming pool considered swimming for our study???? As agreed with Jackie and Sha, swimming in swimming pool will be removed from recreational swimmming.
dat[!is.na(dat$WaterContactSpecify)&grepl("pool",dat$WaterContactSpecify, ignore.case = T),match(c("Type","WaterContactSpecify","WaterContactOcean","WaterActivityOthers","WaterContactOthers","WaterContactRiver","WaterActivitySwimming"),names(dat))]



dat[dat$WaterContactOthers==-1&!is.na(dat$WaterContactSpecify)&grepl("pool",dat$WaterContactSpecify, ignore.case = T),match(c("Type","WaterContactSpecify","WaterContactOcean","WaterActivityOthers","WaterContactOthers","WaterContactRiver","WaterActivitySwimming"),names(dat))]

dat[dat$WaterContactOthers==-1&!is.na(dat$WaterContactSpecify)&grepl("Pool",dat$WaterContactSpecify, ignore.case = T)&dat$WaterContactOcean==0&dat$WaterContactRiver==0,match(c("Type","WaterContactSpecify","WaterContactOcean","WaterContactOthers","WaterContactRiver","WaterActivitySwimming"),names(dat))]

dat[!is.na(dat$WaterContactSpecify)&grepl("pool",dat$WaterContactSpecify, ignore.case = T)&dat$WaterContactOcean==0&dat$WaterContactRiver==0,"WaterActivitySwimming"]<-0

dat[dat$WaterContactOthers==-1&!is.na(dat$WaterContactSpecify)&grepl("pool",dat$WaterContactSpecify, ignore.case = T),"WaterContactOthers"]<-0

dat[dat$WaterContactOthers==-1&!is.na(dat$WaterContactSpecify),c(waterexposurecolumns,recreationalcolumns,match(c("WaterContactSpecify","QID"),names(dat)))]

dat[dat$WaterActivitySwimming==-1&dat$WaterContactOcean==-1&dat$WaterContactRiver==0,c(waterexposurecolumns,recreationalcolumns,match(c("WaterContactSpecify","QID","Type"),names(dat)))]

#__________________________________CORRECTIONS END
###############################################Water#Recreational#exposures#############

#Any water recreational activity:
dat$recreational<-ifelse(dat$WaterActivitySwimming==-1|dat$WaterActivityBoating==-1|dat$WaterActivityFishing==1|
                           dat$WaterActivityOthers==-1,-1,0)
#Contingency table
ctab(as.factor(dat$recreational),as.factor(dat$Type))
#Fisher O.R.
recreational<-fisher.test(ctab(as.factor(dat$recreational),as.factor(dat$Type))$table)
anyrecreational<-(c("AnyRecreationalExposure",as.numeric(recreational$estimate[1]),as.numeric(recreational$conf.int[1]),as.numeric(recreational$conf.int[2])))
anyrecreational




#creating contingency tables for recreational water exposure

for (i in (recreationalcolumns)){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}

#Table with Fisher's O.R.s
comparisonrecreation<-as.data.frame(matrix(ncol=4,nrow=length(recreationalcolumns)))
names(comparisonrecreation)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonrecreation
comparisonrecreation$AnimalGroup<-names(dat[recreationalcolumns])
comparisonrecreation<-rbind(comparisonrecreation,anyrecreational)
comparisonrecreation
#fisher tests with the confidence intervals
for (i in 1:length(recreationalcolumns)) {
  comparisonrecreation[i,1]<-names(dat[recreationalcolumns[i]])
  comparisonrecreation[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,recreationalcolumns[i]]),dat$Type)$table)$estimate)
  comparisonrecreation[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,recreationalcolumns[i]]),dat$Type)$table)$conf.int[1])
  comparisonrecreation[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,recreationalcolumns[i]]),dat$Type)$table)$conf.int[2])
}
comparisonrecreation
str( comparisonrecreation)
for (i in 2:4){ comparisonrecreation[,i]<-as.numeric(comparisonrecreation[,i])}

#########drawing a forest plot with the ORs and confidence intervals
forestplot(labeltext = comparisonrecreation$AnimalGroup, mean=comparisonrecreation$fisherOR,lower=comparisonrecreation$Min.conf.int, upper=comparisonrecreation$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           is.summary = c(rep(F,7),T), 
           boxsize = 1/exp(abs(log(comparisonrecreation$Max.conf.int)-(log(comparisonrecreation$Min.conf.int)))),
           clip = c(0,3), title="Recreational activities risk of leptospirosis")



#Plotting exposure vs time of the year
for (i in 1:3) { 
  plot (dat$IntervieworSickSolsticeDist,as.character(dat[,recreationalcolumns[i]]))
}

ggplot(data=dat, aes(x=IntervieworSickSolsticeDist, y=WaterActivitySwimming, fill=Type ))+ geom_jitter(aes(colour = Type))
#even in  summer, the cases swam much less than controls. Why would that be???
ctab(as.factor(dat$WaterActivitySwimming[dat$IntervieworSickSolsticeDist>120]),dat$Type[dat$IntervieworSickSolsticeDist>120])
#######################################Other#environmental#water#exposures ######################


names(dat[waterexposurecolumns])


Anywatercontact<-rep(0,nrow(dat))  
#any environemntal water contact exposure
for(i in 1:nrow(dat)) {
  Anywatercontact[i] <-  ifelse(any(dat[i,(520+3*(0:6))]==-1),-1,0)
}
Anywatercontact                                          
dat$anywatercontact<-Anywatercontact
ctab(as.factor(dat$anywatercontact), as.factor(dat$Type))
test.fisher9<-fisher.test(ctab(as.factor(dat$anywatercontact), as.factor(dat$Type))$table)

anywatercontact<-(c("AnyWatercontact",as.numeric(test.fisher9$estimate[1]),as.numeric(test.fisher9$conf.int[1]),as.numeric(test.fisher9$conf.int[2])))
anywatercontact

waterexposurecolumns<-c(waterexposurecolumns,which(names(dat)=="anywatercontact"))
waterexposurecolumns


#creating contingency tables for each of the water exposures
comparison<-tibble(1,2,3,4,5,6,7)
for (i in (waterexposurecolumns)){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}

comparisonwatercontact<-as.data.frame(matrix(ncol=4,nrow=length(waterexposurecolumns)))
names(comparisonwatercontact)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonwatercontact

#fisher tests with the confidence intervals
for (i in 1:length(waterexposurecolumns)){
  tryCatch( {  comparisonwatercontact[i,1]<-names(dat[waterexposurecolumns[i]])
  comparisonwatercontact[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,waterexposurecolumns[i]]),dat$Type)$table)$estimate)
  comparisonwatercontact[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,waterexposurecolumns[i]]),dat$Type)$table)$conf.int[1])
  comparisonwatercontact[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,waterexposurecolumns[i]]),dat$Type)$table)$conf.int[2]) }, error=function(e){})
} 

comparisonwatercontact
str( comparisonwatercontact)
for (i in 2:4){ comparisonwatercontact[,i]<-as.numeric(comparisonwatercontact[,i])}
forestplot(labeltext = comparisonwatercontact$AnimalGroup, mean=comparisonwatercontact$fisherOR ,lower=comparisonwatercontact$Min.conf.int, 
           upper=comparisonwatercontact$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           clip = c(0,20), title="Sources of water contact risk of leptospirosis", 
           is.summary = c(rep(F,7),T), 
           boxsize = 1/exp(abs(log(comparisonwatercontact$Max.conf.int)-(log(comparisonwatercontact$Min.conf.int)))))

#Regions from Wetlands 
summary(dat[dat$WaterWetlands==-1,"RegionHomeaddr"])

#vector for all the dichotomous columns of water exposure
names(dat[(which(grepl("Water",names(dat))))])
names(dat[(which(grepl("Water",names(dat))&!grepl("Type|None|Treated|Days|Lepto|Work|Specify|Rain|Creek|Town|Bore|Tanker|Comments|Unsure|WaterOther|Freq",names(dat))))])

#Will use this vector further in MV logistic regression
allwaterexposure<-c(which(grepl("Water",names(dat))&!grepl("Type|None|Treated|Days|Lepto|Work|Specify|Rain|Creek|Town|Bore|Tanker|Comments|Unsure|WaterOther|Freq",names(dat))))
allwaterexposure

######################OTHER EXPOSURES############

glmotherexposures<-c("ExposureSoil","OutdoorHiking","OutdoorCamping","OutdoorOthers","Barefoot","TravelOverseas")
glmotherexposures<-match(glmotherexposures,names(dat))

#Correcting some variables
#someone mentions countries of travel, but the dichotomous variable is 0. 
dat[!is.na(dat$TravelCountries),c("TravelCountries","TravelOverseas","Type")]
dat[!is.na(dat$TravelCountries)&dat$TravelOverseas==0,"TravelOverseas"]<- -1

glmotherexposures
names(dat[glmotherexposures])
otherexp<-c(542,563,565)
names(dat[otherexp])

####### changing Nas, 99s and 999s to 0s
for (i in otherexp){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}

###contingency tables
for (i in otherexp){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}


#calculating fisher test odds ratios
comparisonotherexp<-as.data.frame(matrix(ncol=4,nrow=length(otherexp)))
names(comparisonotherexp)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonotherexp


for (i in 1:length(otherexp)){
  comparisonotherexp[i,1]<-names(dat[otherexp[i]])
  comparisonotherexp[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,otherexp[i]]),dat$Type)$table)$estimate)
  comparisonotherexp[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,otherexp[i]]),dat$Type)$table)$conf.int[1])
  comparisonotherexp[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,otherexp[i]]),dat$Type)$table)$conf.int[2])
}

comparisonotherexp
str( comparisonwatercontact)
for (i in 2:4){ comparisonotherexp[,i]<-as.numeric(comparisonotherexp[,i])}
forestplot(labeltext = comparisonotherexp$AnimalGroup, mean=comparisonotherexp$fisherOR ,lower=comparisonotherexp$Min.conf.int, 
           upper=comparisonotherexp$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           clip = c(0,20), title="Other exposures risk of leptospirosis", 
           is.summary = c(rep(F,7),T), 
           boxsize = 1/exp(abs(log(comparisonotherexp$Max.conf.int)-(log(comparisonotherexp$Min.conf.int)))))

########################################################Travelling overseas############
dat[dat$TravelOverseas==-1,c("Type","QID","TravelCountries")]
daysoverseas<-na.omit((as.Date(dat$TravelReturn)- as.Date(dat$TravelDeparture)))
(as.numeric(daysoverseas))
median(as.numeric(daysoverseas))
dat[!is.na(dat$TravelCountries),c("TravelCountries","TravelOverseas","Type")]
sort(na.omit(as.Date(dat$TravelReturn)))


#####################################Protective equipment for soil exposure##################

match("ExposureSoil", names(dat))
#Contingency table for soil exposure
dat$ExposureSoil
ctab(as.factor(dat$ExposureSoil), dat$Type)
fisher.test(ctab(as.factor(dat$ExposureSoil), dat$Type)$table)

#Variables for PPE - soil exposure
names(dat[which(grepl("Soil", names(dat))&grepl("PPE", names(dat)))])
#other answers for PPE
dat$SoilPPESpecify[!is.na(dat$SoilPPESpecify)]    
dat$SoilPPEUnsure

#these are the participants that will be investigatedfor the use of protective equipment. Only those who had exposure to soil:
SoilPPEparticipantsdf<-dat[dat$ExposureSoil==-1, c(which(grepl("Soil", names(dat))&grepl("PPE", names(dat))&!grepl("Unsure|None|Specify", names(dat))),match("Type",names(dat)))]
head(SoilPPEparticipantsdf)
SoilPPEparticipantsdf[is.na(SoilPPEparticipantsdf)|SoilPPEparticipantsdf==999]<-0
anysoilPPE<-apply(SoilPPEparticipantsdf,1,function(x)ifelse(any(x==-1),-1,0))
SoilPPEparticipantsdf$anywildPPE<-anysoilPPE
SoilPPEcolumns<-c(1:6,8)
SoilPPEparticipantsdf

#contingency tables for PPE use
for (i in (SoilPPEcolumns)){
  comparison<-ctab(as.factor(SoilPPEparticipantsdf[,i]),SoilPPEparticipantsdf$Type)
  print(names(SoilPPEparticipantsdf[i]))
  print(comparison)
}

comparisonSoilPPE<-data.frame(matrix(ncol=4,nrow=length(SoilPPEcolumns)))
names(comparisonSoilPPE)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonSoilPPE
comparisonSoilPPE$AnimalGroup<-names(SoilPPEparticipantsdf[SoilPPEcolumns])
comparisonSoilPPE
#fisher tests with the confidence intervals
for (i in 1:length(SoilPPEcolumns)) {
  comparisonSoilPPE[i,1]<-names(SoilPPEparticipantsdf[SoilPPEcolumns[i]])
  tryCatch({  comparisonSoilPPE[i,2]<-as.numeric(fisher.test(ctab(as.factor(SoilPPEparticipantsdf[,SoilPPEcolumns[i]]),SoilPPEparticipantsdf$Type)$table)$estimate)
  comparisonSoilPPE[i,3]<-as.numeric(fisher.test(ctab(as.factor(SoilPPEparticipantsdf[,SoilPPEcolumns[i]]),SoilPPEparticipantsdf$Type)$table)$conf.int[1])
  comparisonSoilPPE[i,4]<-as.numeric(fisher.test(ctab(as.factor(SoilPPEparticipantsdf[,SoilPPEcolumns[i]]),SoilPPEparticipantsdf$Type)$table)$conf.int[2]) } , error=function(e){})
}
comparisonSoilPPE

for (i in 2:4){ comparisonSoilPPE[,i]<-as.numeric(comparisonSoilPPE[,i])}

#########drawing a forest plot with the ORs and confidence intervals
forestplot(labeltext = comparisonSoilPPE$AnimalGroup, mean=comparisonSoilPPE$fisherOR,lower=comparisonSoilPPE$Min.conf.int, upper=comparisonSoilPPE$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           is.summary = c(rep(F,6),T), 
           boxsize = 1/exp(abs(log(comparisonSoilPPE$Max.conf.int)-(log(comparisonSoilPPE$Min.conf.int)))),
           clip = c(0,20), title="SoilPPE activities risk of leptospirosis")

##########################################################Outdoor variables####

#converting letters from excel spreadsheet to column number in R
outdoors<-c("UM", "UN" , "UO")
outdoorsn<-c(1)
for(i in 1:3){
  outdoorsn[i]<- which(LETTERS==substring(outdoors[i],1,1))*26+which(LETTERS==substring(outdoors[i],2,2))-1   
}
outdoorsn<-c(558,559,560)

###names for other exposures variables
names(dat[outdoorsn])

####### changing Nas, 99s and 999s to 0s
for (i in outdoorsn){
  dat[is.na(dat[,i])|dat[i]==99|dat[i]==999,i]<-0
}

dat$OutdoorSpecify[!is.na(dat$OutdoorSpecify)&!dat$OutdoorSpecify==999]
#Some people answered hunting. These answers should be in the specific session.
dat$OutdoorSpecify[grepl("hunt",dat$OutdoorSpecify,ignore.case = T)]
dat[grepl("hunt",dat$OutdoorSpecify,ignore.case = T),which(names(dat)=="HuntAnimals")]<- -1
dat[grepl("hunt",dat$OutdoorSpecify,ignore.case = T),which(names(dat)=="OutdoorOthers")]<-0

# Horse riding would be better put in contact with horses. the respondent has already answered that.
dat[grepl("Horse Riding",dat$OutdoorSpecify,ignore.case = T),c(contactcolumns,activitiescolumns)]
dat[grepl("Horse Riding",dat$OutdoorSpecify,ignore.case = T),which(names(dat)=="OutdoorOthers")]<-0

###contingency tables
for (i in outdoorsn){
  comparison<-ctab(as.factor(dat[,i]),dat$Type)
  print(names(dat[i]))
  print(comparison)
}

anyoutdoors<-dat[outdoorsn]
head(anyoutdoors)

#creating a column that creates a -1 if any outdoors activity was made

summaryoutdoors<-rep(0,nrow(dat))                                          
for (i in 1:nrow(dat)){                                             
  summaryoutdoors[i]<-                                             
    ifelse(any(anyoutdoors[i,]==-1),-1,0) 
}
summaryoutdoors

dat$anyoutdoorsactivity<-summaryoutdoors
ctab(as.factor(dat$anyoutdoorsactivity),as.factor(dat$Type))

test.fisher10<-fisher.test(as.factor(dat$anyoutdoorsactivity), as.factor(dat$Type))
test.fisher10
anyoutdoorsactivity<-(c("AnyOutdoorActivity",as.numeric(test.fisher10$estimate[1]),as.numeric(test.fisher10$conf.int[1]),as.numeric(test.fisher10$conf.int[2])))
anyoutdoorsactivity

comparisonoutdooractivities<-as.data.frame(matrix(ncol=4,nrow=3))
names(comparisonoutdooractivities)<-c("AnimalGroup","fisherOR","Min.conf.int.","Max.conf.int")
comparisonoutdooractivities

comparisonoutdooractivities<-rbind(comparisonoutdooractivities,anyoutdoorsactivity)
comparisonoutdooractivities
#fisher tests with the confidence intervals

for (i in 1:length(outdoorsn)) {
  comparisonoutdooractivities[i,1]<-names(dat[i])
  comparisonoutdooractivities[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,outdoorsn[i]]),dat$Type)$table)$estimate)
  comparisonoutdooractivities[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,outdoorsn[i]]),dat$Type)$table)$conf.int[1])
  comparisonoutdooractivities[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,outdoorsn[i]]),dat$Type)$table)$conf.int[2])
}
comparisonoutdooractivities

str( comparisonoutdooractivities)
for (i in 2:4){ comparisonoutdooractivities[,i]<-as.numeric(comparisonoutdooractivities[,i])}

#########drawing a forest plot with the ORs and confidence intervals
forestplot(labeltext = comparisonoutdooractivities$AnimalGroup, mean=comparisonoutdooractivities$fisherOR,lower=comparisonoutdooractivities$Min.conf.int, upper=comparisonoutdooractivities$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,
           is.summary = c(rep(F,3),T), 
           boxsize = 1/exp(abs(log(comparisonoutdooractivities$Max.conf.int)-(log(comparisonoutdooractivities$Min.conf.int)))),
           clip = c(0,60), title="Outdoor activities risk of leptospirosis cases")

##########HEALTH#-#MEDICAL#CONDITIONS########




#Medical conditions columns
medicalconditionscols<-which((grepl("Health|Medication", names(dat))|names(dat)=="Antibiotics")&!grepl("Years|Specify|Details|Others", names(dat)))

#Cuts and covering wounds
#these are the answers from particpants who cut themselves about covering or not:
dat[dat[[which(names(dat)=="HealthCutScratch")]]==-1&!is.na(dat[[which(names(dat)=="HealthCutScratch")]]),which(names(dat)=="HealthDressWound")]
#above we see that all respondents that had a cut either covered it or not. 

dat[dat[[which(names(dat)=="HealthCutScratch")]]==0&!is.na(dat[[which(names(dat)=="HealthCutScratch")]]),which(names(dat)=="HealthDressWound")]   #all respondents that did not cut, did not answer about covering wounds - that was expected

dat[is.na(dat[[which(names(dat)=="HealthCutScratch")]]),which(names(dat)=="HealthDressWound")] #some respondents were unsure about cutting, the answer was 0 or NA in the next question

#we have too many 999s
dat$HealthDressWound
dat$HealthDressWound[dat$HealthDressWound==999]<-NA

#I will work with creating one variable for the following conditions: 
# 4 - Had wound and did not cover
# 3 - Had wound and covered it
# 2 - were unsure about wounds
# 1-  Did not have wound
#Classification according to hazard will be 1 to 4.

dat$HealthCutandCover<-as.factor(ifelse(dat[[which(names(dat)=="HealthCutScratch")]]==-1&dat[[which(names(dat)=="HealthDressWound")]]==0&!is.na(dat[which(names(dat)=="HealthCutScratch")]),4,ifelse(dat[[which(names(dat)=="HealthCutScratch")]]==-1&dat[[which(names(dat)=="HealthDressWound")]]==-1&!is.na(dat[which(names(dat)=="HealthCutScratch")]),3,ifelse(dat[[which(names(dat)=="HealthCutScratch")]]==0&!is.na(dat[which(names(dat)=="HealthCutScratch")]),1,ifelse(is.na(dat[which(names(dat)=="HealthCutScratch")])|grepl("99",dat[which(names(dat)=="HealthCutScratch")]),2,0)))))

dat$HealthCutandCover
#This variable puts -1 for those that had cuts and did not cover against 0 for all the rest.
dat$HealthCutandnotCover<-factor(ifelse(dat$HealthCutandCover==4,-1,0),levels=c(-1,0))
#This variable puts -1 for those that had cuts and did cover against 0 for all the rest.
dat$NewDressWound<-dat$HealthDressWound
dat$NewDressWound[is.na(dat$NewDressWound)|dat$NewDressWound==999]<-0
dat$NewDressWound

#Many descriptions for HealthOther
dat$HealthOtherSpecify[dat$HealthOther==-1&!is.na(dat$HealthOtherSpecify)]
#let's try to summarize it. many respondents with high Pressure
dat$HealthHighPressure<-ifelse((grepl("Blood Pressure",dat$HealthOtherSpecify, ignore.case = T)),-1,0) 

#Now remove those that responded only high Pressure in others
dat$HealthOther[(dat$HealthOtherSpecify=="High blood pressure"&!is.na(dat$HealthOtherSpecify))]<-0

#there is already a variable for stress 
dat$HealthOtherSpecify[grepl("stress" , dat$HealthOtherSpecify, ignore.case = T)]
dat$HealthOther[grepl("stress" , dat$HealthOtherSpecify, ignore.case = T)]<-0
which(grepl("heart", dat$HealthOtherSpecify, ignore.case = T))

#Same for heart disease
dat$HealthOtherSpecify[grepl("heart" , dat$HealthOtherSpecify, ignore.case = T)]
dat$HealthOtherSpecify[dat$HealthHeart==0&grepl("heart", dat$HealthOtherSpecify, ignore.case = T)]
dat$HealthHeart[144]<-0
dat$HealthOther[grepl("heart" , dat$HealthOtherSpecify, ignore.case = T)]<-0

#Cancer
dat$HealthCancer<-ifelse((grepl("Cancer|tumor",dat$HealthOtherSpecify, ignore.case = T)),-1,0) 
dat$HealthOther[grepl("Prostate cancer|Brain tumor|Bowel cancer",dat$HealthOtherSpecify, ignore.case = T)]<-0

#All the variables that are in HealthOtherSpecify are answered. Not necessary to include as oother health status
dat[dat$HealthOtherSpecify=="Got diabetes since having lepto, had depression for 7 months 18 years ago,"&!is.na(dat$HealthOtherSpecify),medicalconditionscols]
dat[dat$HealthOtherSpecify=="Got diabetes since having lepto, had depression for 7 months 18 years ago,"&!is.na(dat$HealthOtherSpecify),"HealthOther"]<-0

dat$HealthOtherSpecify[dat$HealthOther==-1&!is.na(dat$HealthOtherSpecify)]


# The variable Smoke Regular is for people that smoked regurlarly through some part of their lives. There are people that responded that currently smoke, but the variable SmokeRegular is not showing that.
dat[dat$HealthSmoke==-1&!dat$HealthSmokeRegular==-1&!is.na(dat$HealthSmokeRegular),medicalconditionscols]

#Changing the values of current smokers to -1 in SmokeRegular variable. 
for (i in 1:nrow(dat)) { 
  dat$HealthSmokeRegular[i]<-ifelse(dat$HealthSmoke[i]==-1&dat$HealthSmokeRegular[i]!=-1,-1,dat$HealthSmokeRegular[i])  }


medicalconditionscols<-which((grepl("Health|Medication", names(dat))|names(dat)=="Antibiotics")&!grepl("Years|Specify|Details|Others", names(dat)))
head(dat[medicalconditionscols])
medicalconditionscols
#changing tthe order of the variables
medicalconditionscols<-c(match(c("HealthCutandCover","NewDressWound","HealthCutandnotCover"),names(dat)),medicalconditionscols[-c(match(match(c("HealthCutandCover","HealthDressWound","HealthCutandnotCover"),names(dat)),medicalconditionscols))])
names(dat[medicalconditionscols])

#inserting NA in the cells with 99s and 999s
for (i in 1:length(medicalconditionscols)-1) { dat[medicalconditionscols[-1][i]][(dat[medicalconditionscols[-1][i]]==999|dat[medicalconditionscols[-1][i]]==99)&!is.na(dat[medicalconditionscols[-1][i]])] <-NA }
dat[is.na(dat[[which(names(dat)=="HealthOther")]]),which(names(dat)=="HealthOther")]<-0
head(dat[medicalconditionscols])

#Crude ORs
comparisonmedical<-data.frame(matrix(nrow = length(medicalconditionscols), ncol = 4 ))
names(comparisonmedical)<- c("Exposure", "Crude.O.R","Min.conf.int.", "Max.conf.int")
as.numeric(fisher.test(ctab(as.factor(dat[,medicalconditionscols[1]]),dat$Type)$table)$conf.int[2])
names(dat[medicalconditionscols])

for ( i  in 1:nrow(comparisonmedical) ) {
  comparisonmedical[i,1]<-names(dat[medicalconditionscols[i]])
  tryCatch({comparisonmedical[i,2]<-as.numeric(fisher.test(ctab(as.factor(dat[,medicalconditionscols[i]]),dat$Type)$table)$estimate)}, error=function(e){})
  tryCatch({comparisonmedical[i,3]<-as.numeric(fisher.test(ctab(as.factor(dat[,medicalconditionscols[i]]),dat$Type)$table)$conf.int[1])}, error=function(e){})
  tryCatch({comparisonmedical[i,4]<-as.numeric(fisher.test(ctab(as.factor(dat[,medicalconditionscols[i]]),dat$Type)$table)$conf.int[2])}, error=function(e){})
  print(names(dat[medicalconditionscols[i]]))
  print(ctab(as.factor(dat[,medicalconditionscols[i]]), dat$Type))
}
str(comparisonmedical)
comparisonmedical$Exposure<- c("HealthCutandCover", "Cutting and covering","Cutting and not covering","Having cuts","Currently smoking", "Been a regular smoker",   "Hay fever", "Asthma" ,"Diabetes", "Health disease" , "Lung disease", "Anxiety" ,"Depression" ,"Other disorders", "On regular medication", "Use of antibiotics*", "High Pressure","Has or had cancer")



#########drawing a forest plot with the ORs and confidence intervals
ticks <- c(0.125,0.25,0.5,1,2,4,8,16,32,64,128)
forestplot(labeltext = comparisonmedical$Exposure, mean=comparisonmedical$Crude.O.R ,lower=comparisonmedical$Min.conf.int, 
           upper=comparisonmedical$Max.conf.int, xlab="Fisher's test odds-ratio with confidence intervals", zero = 1,          boxsize = 1/exp(abs(log(comparisonmedical$Max.conf.int)-(log(comparisonmedical$Min.conf.int)))),
           clip = c(0.125,160), title="Health Conditions risk for leptospirosis notification", xlog = T, xticks=ticks)


####Some obserations I made about medical conditions

#people that smoke are  quite younger
summary(lm(Age~HealthSmoke, data=dat))
summary(glm(as.factor(HealthSmoke)~Age, data=dat, family = "binomial"))
boxplot(dat$Age~ dat$HealthSmoke)

mean(dat$Age[dat$HealthSmoke==-1])
mean(dat$Age[dat$HealthSmoke==0])



mean(as.numeric(dat$HealthCutandCover[dat$Type=="Case"]));mean(as.numeric(dat$HealthCutandCover[dat$Type=="Control"]))
median(as.numeric(dat$HealthCutandCover[dat$Type=="Case"]));median(as.numeric(dat$HealthCutandCover[dat$Type=="Control"]))
boxplot(as.numeric(dat$HealthCutandCover[dat$Type=="Case"]),as.numeric(dat$HealthCutandCover[dat$Type=="Control"])) #apparently, cases had more risk behaviours concerning cuts.

wilcox.test(as.numeric(dat$HealthCutandCover[dat$Type=="Case"]),as.numeric(dat$HealthCutandCover[dat$Type=="Control"])) #wicoxon test als shows there is significant difference between cases and controls

ctab(dat$HealthCutandCover,dat$Type)

#interesting correlations
# the largest correlations with history of smoking (over 0.2) are: 
# negative correlation : ContactCattle  contactwithfarm  AnyActivityFarm  drystockworkers  sheepworkers  AnyAssistBirth  livestockworkers EvidenceWildMice
#positive correlation : jobless 


#########################CORRELATIONS######

#The idea in this section is to study and understand correlations between variables from the case-control study, specially the indirect relationships between exposures, to evaluate why certain variables behave in a certain manner when put in multivariable models, why some strong risk factors lose their significance, and try to explain some expurious findings from the multivariable models. In this database, many correlations over 0.2 ( most times considered not very strong correlation) returned significant univariable logistic regression results. In conclusion, correlations in this magnitude (0.2-0.3) could maybe suggest some type of confounding or interaction, however not in a very evident way. 

#Beware that not all correlations have necessarily relevance. The fact that categorical values are transformed to numerical may not always be appropriate. Therefore they should be analysed with parsimony. Remember that this database uses -1 as Yes in many variables. sometimes a negative correlation is positive ( e.g.,correlation between number of days of dairy activities and having beef cattle contact (YES/NO). If the correlation is negative, it means there is a positive association between having beef cattle contact and number of days of dairy cattle activity. But if we analyse the correlation between contact with dairy cattle (YES/NO) and contact with beef cattle (YES/NO), then a positive correlation means a positive association. Both variables have -1 as YES.)

#first I will create a cross table with all the variables. During my dissertation, I used excel to analyse the cross table, so I exported to csv. That was because I could viasualize the data better with Excel. Likewise it can be done with R. The paths in excel were the following: 
#1  - Open csv file #2 - Select the whole range of the table then Press alt+D+P and open PivotTable and Pivotchart Wizard #3 - Select multiple consolidation ranges and press next # 4 - Create a single field page for me #5 - Select the whole range of the table again and press add. Then press next. #5 - New work sheet and press finish. #6 - In the pivot table fields, drag row and column to the rows box, then drag Value to the Values box. Press the down arrow in the value box and choose Summarize value field box by Sum and press OK. #7 the Pivot table should show all the variables and the correlation with other variables. #8 I recommend to filter these values. In the filter menu, select field column and then value filters, then "Not Between". Put the filter you want. E.g., not between -0.2 and 0.2.The strongest correlations should appear for each variable. If necessary filter again for the desired variables to investigate.


#producing a cross correlation table
numericdat<-dat
#all values transformed to numeric.
for (i in 1:ncol(dat)) {  
  numericdat[i]<- as.numeric(as.character(dat[[i]])) }

#some steps to remove NAs form the table
for (i in 1:ncol(dat)) { 
  numericdat[[i]]<- if(all(is.na(numericdat[i]))) {as.numeric(dat[[i]]) } else {numericdat[[i]]} }
head(numericdat)
str(numericdat)
dim(numericdat)
#For some values, we will change NAs to the mean of the whole Variable, so that it does not affect the correlation. 
for (i in 1:ncol(numericdat)) { 
  numericdat[i][is.na(numericdat[i])]<-mean(numericdat[i],na.rm=T)   }
head(numericdat)
#For those that mean could not be calculated, they were changed to 0.
for (i in 1:ncol(numericdat)) { 
  numericdat[i][is.na(numericdat[i])]<-0   }
head(numericdat)
#Now we calculate the correlation in a cross table.
cortable<-cor(numericdat)
# and the values will be rounded. We don't need more than 2 decimals and the file size becomes too large with more decimals
cortable<-round(cortable,2)
write.csv(x = cortable,file = "cortable.csv")


##############################Correlation analysis of independent variables################ 

#After analysing the correlation values, I have tested the variables with other statistical tests and plots.

#####Income
#Who has more income has more contact with risk waters - more  travel(?)
summary(glm(factor(dat$anywatercontact)~dat$PersonalIncome, family = "binomial"))
plot(dat$anywatercontact,jitter(as.numeric(dat$PersonalIncome), 2))
abline(lm(as.numeric(dat$PersonalIncome)~dat$anywatercontact))


#Agricuture workers earn more than average study participants

summary(glm(as.factor(dat$agricultureworkers)~as.numeric(dat$PersonalIncome), family = "binomial"))

#Obviously jobless people have less income 
summary(glm(as.factor(dat$jobless)~as.numeric(dat$PersonalIncome), family = "binomial"))

# People wit more income have more conact with livestock
summary(glm(as.factor(dat$contactwithfarm)~as.numeric(dat$PersonalIncome), family = "binomial"))

#they also walk more barefoot,   

summary(glm(dat$dichotomoustype~as.numeric(dat$PersonalIncome), family = "binomial"))

#####Education

#meatworkers have less education level, so do jobless people
summary(glm(as.factor(dat$meatworkers)~as.numeric(dat$Education), family = "binomial"))

#Although it is slightly relevant, Education is still not significant for protecting from cases.
summary(glm(dat$dichotomoustype~as.numeric(dat$Education), family = "binomial"))


#Tongan have studied less

summary(lm(as.numeric(dat$Education)~dat$EthnicityTongan))

#Males tend to have greater income
summary(lm(as.numeric(dat$PersonalIncome)~dat$Sex))
#Males have more contact with wildlife

summary(glm(as.factor(dat$contactwithwild)~Sex,data=dat, family = "binomial"))

#And less activities with horses
summary(glm(as.factor(dat$AnyActivityHorses)~Sex,data=dat, family = "binomial"))



#walking barefoot is confounded by time of the year.
summary(glm(dichotomoustype~Barefoot, data=dat, family = "binomial"))
summary(glm(dichotomoustype~Barefoot+IntervieworSickSolsticeDist, data=dat, family = "binomial"))


#Evidence of wild Rabbbits is positively correlated with evidence of wild rodents and contact with river and wetlands, and contact with livestock
summary(glm(as.factor(EvidenceWildRabbit)~EvidenceAnyRodent, data=dat, family = "binomial"))
summary(glm(as.factor(EvidenceWildRabbit)~EvidenceAnyRodent+Age+Gen.RuralityHomeaddr+Sex, data=dat, family = "binomial"))
summary(glm(as.factor(EvidenceWildRabbit)~Age+Gen.RuralityHomeaddr+Sex+IntervieworSickSolsticeDist, data=dat, family = "binomial"))
summary(glm(as.factor(EvidenceWildRabbit)~WaterContactRiver, data=dat, family = "binomial"))
summary(glm(as.factor(EvidenceWildRabbit)~WaterWetlands, data=dat, family = "binomial"))
summary(glm(as.factor(EvidenceWildRabbit)~contactwithfarm, data=dat, family = "binomial"))
summary(glm(as.factor(EvidenceWildRabbit)~IntervieworSickSolsticeDist , data=dat, family = "binomial"))
summary(glm(dichotomoustype~EvidenceWildRabbit+EvidenceAnyRodent , data=dat, family = "binomial"))
summary(glm(dichotomoustype~EvidenceWildRabbit+EvidenceAnyRodent , data=dat[dat$IntervieworSickSolsticeDist>90,], family = "binomial"))
summary(glm(dichotomoustype~Age+Gen.RuralityHomeaddr+Sex+EvidenceWildRabbit+EvidenceAnyRodent , data=dat[dat$IntervieworSickSolsticeDist>90,], family = "binomial"))


#Evidence of Deer is not affected by season
summary(glm(dichotomoustype~Age+Gen.RuralityHomeaddr+Sex+EvidenceWildDeer+EvidenceAnyRodent , data=dat[dat$IntervieworSickSolsticeDist>90,], family = "binomial"))
summary(glm(dichotomoustype~Age+Gen.RuralityHomeaddr+Sex+EvidenceWildDeer+EvidenceAnyRodent , data=dat, family = "binomial"))



#Evidence of rabbit is affected by season
summary(glm(dichotomoustype~EvidenceWildRabbit , data=dat, family = "binomial"))
summary(glm(dichotomoustype~EvidenceWildRabbit+IntervieworSickSolsticeDist , data=dat, family = "binomial"))
summary(glm(dichotomoustype~EvidenceWildRabbit+EvidenceAnyRodent+IntervieworSickSolsticeDist , data=dat, family = "binomial"))
summary(glm(dichotomoustype~EvidenceAnyRodent+IntervieworSickSolsticeDist , data=dat, family = "binomial"))
summary(glm(dichotomoustype~Age+Gen.RuralityHomeaddr+Sex+IntervieworSickSolsticeDist+EvidenceWildRabbit, data=dat, family = "binomial"))

# ethnicities
#Europeans have more knowledge about leptospirosis 
summary(glm(as.factor(EthnicityNZEuropean)~GetLeptoKnowledge, data=dat, family = "binomial"))
#Few meat workers are Europeans
summary(glm(as.factor(EthnicityNZEuropean)~meatworkers, data=dat, family = "binomial"))
#Europeans see more rodents
summary(glm(as.factor(EthnicityNZEuropean)~EvidenceAnyRodent, data=dat, family = "binomial"))
#Europeans are older in this study
summary(glm(as.factor(EthnicityNZEuropean)~Age, data=dat, family = "binomial"))
summary(glm(as.factor(dichotomoustype)~ethnicity, data=dat, family = "binomial"))
summary(glm(as.factor(EthnicityNZEuropean)~Gen.RuralityHomeaddr, data=dat, family = "binomial"))
summary(glm(as.factor(Gen.RuralityHomeaddr)~as.factor(ethnicity), data=dat, family = "binomial"))


#Maoris are more related to jobs in Abattoirs and all slaughtering activities
summary(glm(as.factor(EthnicityNZMaori)~meatworkers, data=dat, family = "binomial"))
summary(glm(as.factor(EthnicityNZMaori)~SkinAnimals, data=dat, family = "binomial"))
summary(glm(as.factor(EthnicityNZMaori)~ActivityAnyDress, data=dat, family = "binomial"))
summary(glm(as.factor(EthnicityNZMaori)~ActivityAnySlaughter, data=dat, family = "binomial"))
summary(glm(as.factor(EthnicityNZMaori)~ActivityAnyHomekill, data=dat, family = "binomial"))

#They have less study
summary(glm(as.factor(EthnicityNZMaori)~NumericEducation, data=dat, family = "binomial"))

#More high pressure
summary(glm(as.factor(EthnicityNZMaori)~HealthHighPressure, data=dat, family = "binomial"))
#And have more exposure to Wild Deer
summary(glm(as.factor(EthnicityNZMaori)~WildDeer, data=dat, family = "binomial"))

#People who smoke are younger
summary(glm(as.factor(HealthSmoke)~Age, data=dat, family = "binomial"))

# Older people have been interviewed more on the Summer
summary(lm(IntervieworSickSolsticeDist~Age, data=dat))
plot(dat$IntervieworSickSolsticeDist~dat$Age)

#younger people make more slaughtering activities
summary(glm(AnySlaughterActivity~Age, data=dat, family = "binomial"))

#Younger people had more contact with dairy
summary(glm(as.factor(ContactDairy)~Age, data=dat, family = "binomial"))
mean(dat$Age[dat$ContactDairy==-1])
mean(dat$Age[dat$ContactDairy==0])

#people that swam did less farm activities, and are less prone to have contact with dairy animals
summary(glm(as.factor(WaterActivitySwimming)~AnyActivityFarm, data=dat, family = "binomial"))
summary(glm(as.factor(WaterActivitySwimming)~ContactDairy, data=dat, family = "binomial"))

#Past Smokers don`t usually work with animals and other livestock activities
summary(glm(as.factor(HealthSmokeRegular)~livestockworkers, data=dat, family = "binomial"))
cor(numericdat$HealthSmokeRegular,numericdat$livestockworkers)

summary(glm(as.factor(HealthSmokeRegular)~contactwithfarm, data=dat, family = "binomial"))
cor(numericdat$HealthSmokeRegular,numericdat$contactwithfarm)

summary(glm(as.factor(HealthSmokeRegular)~AnyActivityFarm, data=dat, family = "binomial"))
cor(numericdat$HealthSmokeRegular,numericdat$AnyActivityFarm)

#Past Smokers and age is not significant, although they are marginally older. 
summary(glm(as.factor(HealthSmokeRegular)~Age, data=dat, family = "binomial"))

#current smokers are greater among Samoan ethnicity, and do less outdoors activity
summary(glm(as.factor(HealthSmoke)~anyoutdoorsactivity, data=dat, family = "binomial"))
summary(glm(as.factor(HealthSmoke)~EthnicitySamoan, data=dat, family = "binomial"))

#people who use antibiotics did more drainage and plumbing work
summary(glm(as.factor(Antibiotics)~WaterDrainage, data=dat, family = "binomial"))

#POultry - those who had contact with poultry had more water recreational activities
summary(glm(as.factor(ContactPoultry)~recreational, data=dat, family = "binomial"))

#correlations between contact with different livestock animals.
livestock.corr<-round(cor(dat[contactcolumns]),2)
livestock.corr[lower.tri(livestock.corr,diag = T)]<-""
livestock.corr

#contact of beef cattle and sheep is 0.36 
# beef and dairy is 0.32



############GENERALIZED LINEAR MODELS####
#Formulas for GLMs

#Which variables are we using to run the logistic regression models? 
#Naming the variables in which we will run the models against. This section is showing only the variables which we matched or adjusted against, therefore would be forced into the model. We will have vectors for a simple logistic regression, for the matching factors (sex and rurality, for matching factors and age. The last combination of adjusters included also distance from Solstice in days but was dropped in the analysis,

{groupsforglm<-"dichotomoustype"
matchingforglm<-c("Sex","Gen.RuralityHomeaddr") 
matchedadjustedage<-c("Sex","Gen.RuralityHomeaddr", "Age")
matchedadjustedglm<-c("Sex","Gen.RuralityHomeaddr", "Age", "IntervieworSickSolsticeDist")
} 


# creating a function for the formulas of logistic regression. I used this function in the first analyses, but then I developed another function for the whole glm command, which will be shown next.
  glmformula<-function(x)as.formula(paste(groupsforglm,paste(names(dat[x]),collapse="+"), sep ="~" ))
  #For example, I just write the name of the vector with the column numbers inside glmformula and it produces the formula to be placed inside glm function : 
summary(glm(glmformula(contactcolumns), data=dat, family = "binomial"))
#Above was the initial function I used to run the models against. Next I am integrating more commands in a function

# Functions for the whole GLM command. I used different commands as I start adjusting the models: 
#Creating Vectors with numbers from the columns of the matching or adjusting variables. That will be useful next.

#creating vectors for the matching factors and adjusters of the models
{
  Matchingvector<- (match(c("Sex", "Gen.RuralityHomeaddr"), names(dat)))
  Agematchvector<-(match(c("Sex", "Gen.RuralityHomeaddr" , "Age"), names(dat)))
  agematchethnic<-c(match(c("Sex","Gen.RuralityHomeaddr","Age", "ethnicity"),names(dat)))
  Timeagematchvector<- (match(c("Sex", "Gen.RuralityHomeaddr" , "Age","IntervieworSickSolsticeDist"), names(dat)))} 

  # for example: 
Agematchvector

#after defining the names of matching and adjusting variables, we will create the functions for the complete GLM:
{
  #Run the L.R. only against outcome
  mvglmformula<-function(x)glm(as.formula(paste(groupsforglm,paste(names(dat[x]),collapse="+"), sep ="~" )), data=dat, family = "binomial" )
  
  #Run against outcome and matching variables
  matchedglmformula<-function(x)glm(as.formula(paste(groupsforglm,paste(c(matchingforglm, names(dat[x])),collapse="+"), sep ="~" )), data=dat, family = "binomial" )
  
  #Run against outcome, matching variables and adjusting for age
  matchedglmformulaadjage<-function(x)glm(as.formula(paste(groupsforglm,paste(c(matchedadjustedage, names(dat[x])),collapse="+"), sep ="~" )), data=dat, family = "binomial" )
  
  # Run against outcome, matching variables and adjusting for age and time
  adjustedmatchedglmformula<-function(x)glm(as.formula(paste(groupsforglm,paste(c(matchedadjustedglm, names(dat[x])),collapse="+"), sep ="~" )), data=dat, family = "binomial" )
}
#Now I don't need to write the whole formula for glm. just write the function and inside write the vector of independent variables. For example: 
matchedglmformulaadjage (contactcolumns)
summary(matchedglmformulaadjage (contactcolumns))

#Advancing a little more, I am creating a function to develop a model according to the AIC value and p-values. Starting with a large model, it will remove one or more variables according the p-value from each coefficient. Highest p-values are removed one at a time. Variable X is a vector with the column numbers in which we want to run the models (I have created throughout the descriptive statistics many vectors with column numbers, for example "contactcolumns"). Variable y is the type of analysis I want to run: just against the outcome,matched or adjusted, i.e., the functions created over this section. Unfortunately, I could not advance this formula to run categorical variables in it's anlyses. Only numerical and factors.

autoglm_MVLR<-function(x,y) {
  autoglm_contactcolumns<-list(0)
  auto_contactcolumns<-x
  for (i in 1:length(x)) { 
    autoglm_contactcolumns[[i]] <-summary(glm(as.formula(paste(groupsforglm,paste(c(names(dat[y]), names(dat[auto_contactcolumns])),collapse="+"), sep ="~" )), data=dat, family = "binomial" ))
    if (i==1) {
      print(paste("Initial variables:", paste(names(dat[auto_contactcolumns]),collapse = ", ")))
    }
    print(i)
    print(paste( "AIC",autoglm_contactcolumns[[i]]$aic))
    print(paste(names(dat[auto_contactcolumns[(match(max(autoglm_contactcolumns[[i]]$coefficients[((length(y)+2):length(autoglm_contactcolumns[[i]]$coefficients[,4])),4]),(autoglm_contactcolumns[[i]]$coefficients[,4]))-(length(y)+1))]]),"removed"))
    auto_contactcolumns<-auto_contactcolumns[-c(which(is.na(autoglm_contactcolumns[[i]]$coefficients[((length(y)+2):length(autoglm_contactcolumns[[i]]$coefficients[,4]))]))+(length(y)+1),(match(max(autoglm_contactcolumns[[i]]$coefficients[((length(y)+2):length(autoglm_contactcolumns[[i]]$coefficients[,4])),4]),(autoglm_contactcolumns[[i]]$coefficients[,4]))-(length(y)+1)))]
    
  } 
  assign(paste("autoglm_",deparse(substitute(x)),sep = ""),autoglm_contactcolumns,envir = parent.frame()) } 


#As an exmple, I have applied the autoglm_MVLR function to the livestock contact variables:
# First with no adjustment. Just the multivariable (no matching or other adjustment)
autoglm_MVLR(contactcolumns,NULL)
#to access all the models, just type the prefix autoglm_ plus the name of the first term of the x variable at the function. That is: contactcolumns, which was the first element inside the parenthesis. therefore the name would be autoglm_contactcolumns
autoglm_contactcolumns

# to access one particular model, just use the number of the model in "[[]]"
autoglm_contactcolumns [[9]]

# remember, to match or adjust for other variables, just enter in the second element of the parenthesis (element y from the function) a vector with the column number of the variables to be matched or adjusted. These elements will never be removed from the model.
# Now the matched analysis
autoglm_MVLR(contactcolumns,Matchingvector)
# Now the matched analysis with adjustment for age
autoglm_MVLR(contactcolumns,Agematchvector)
# Now the matched analysis with adjustment for time and age
autoglm_MVLR(contactcolumns,Timeagematchvector)


#GLMs 
#transforming all thecolumns in factor and determining the level order. that is important so that the logistic regression model understands that an odds-ratio over 1 is a risk factor and under 1 is a protective factor. Otherwise the values may be confusing. 
#I am going to place all the vectors I am going to use for the models in this formula.
for (i in c(contactcolumns,wildevidencecolumns,match("contactwithfarm", names(dat)),activitiescolumns,glmactivitycolumns,activitygroupcolumns,recreationalcolumns,occupationvector,allwaterexposure,petscolumns,glmotherexposures,otheran,waterandtreatmentcolumns)) { 
  dat[i]<-factor(dat[,i], levels=c(0,-1))    }

sectioncolumns<-list(contactcolumns,wildevidencecolumns,match("contactwithfarm", names(dat)),activitiescolumns,glmactivitycolumns,activitygroupcolumns,recreationalcolumns,occupationvector,allwaterexposure,petscolumns,glmotherexposures,otheran,waterandtreatmentcolumns)

################################Precursor GLM Models##################
#as decided by the team, we are using as adjustors the matching factors (Sex, Rurality ) and Age for the M.V models

##################################Univariable models################
#With the next 2 loops, I am doing logistic regression models adjusted by sex, rurality and age categorized by exposure sections. They are not multivariable models. Only one variable is calculated per model (plus the adjusting variables).
univariableforestplots<-list(0)
for( j in 1:length(sectioncolumns)) {
glmunivariablecompare<-data.frame(matrix(nrow = length(sectioncolumns[[j]]), ncol=4))
names(glmunivariablecompare)<- c("AnimalGroup", "Crude.O.R","Min.conf.int.", "Max.conf.int")
for (i in sectioncolumns[[j]]) {
  tryCatch( { glmunivariablecompare[match(i,sectioncolumns[[j]]),1]<- names(dat[i])
  glmunivariablecompare[match(i,sectioncolumns[[j]]),2]<-as.numeric(exp(matchedglmformulaadjage(i)$coefficients[5]))
  glmunivariablecompare[match(i,sectioncolumns[[j]]),3]<-as.numeric(exp(matchedglmformulaadjage(i)$coefficients[5]-summary(matchedglmformulaadjage(i))$coefficients[5,2]*1.96))
  glmunivariablecompare[match(i,sectioncolumns[[j]]),4]<-as.numeric(exp(matchedglmformulaadjage(i)$coefficients[5]+summary(matchedglmformulaadjage(i))$coefficients[5,2]*1.96))
  glmunivariablecompare[is.na(glmunivariablecompare[3]),3]<-0
  glmunivariablecompare[is.na(glmunivariablecompare[4]),4]<-Inf } , error= function(e) {} ) 
}
univariableforestplots[[j]]<-glmunivariablecompare }

univariableforestplots

#Now we need to plot the models per section
for (i in 1:13) { 
print(forestplot(labeltext = univariableforestplots[[i]]$AnimalGroup, mean=univariableforestplots[[i]]$Crude.O.R, lower=univariableforestplots[[i]]$Min.conf.int, upper=univariableforestplots[[i]]$Max.conf.int, xlab="Univariable logistic regression odds-ratio with confidence intervals", zero = 1,
           boxsize = 1/exp(abs(log(univariableforestplots[[i]]$Max.conf.int)-(log(univariableforestplots[[i]]$Min.conf.int)))), clip = c(0.125,160), title="Risk of leptospirosis cases", xlog = T, xticks=ticks, )) }



################################Multivariable models######
############################GLM - Lvestock Exposures########
######################################################Livestock contact GLM#################



#Multivariable models
#Initial model with all the livestock contact models
summary(matchedglmformulaadjage(contactcolumns))
#I will remove cattle because this variable is colinear with the beef and dairy cattle variables
contactcolumnsnocattle<-c(contactcolumns[-(match(match("ContactCattle", names(dat)),contactcolumns))])

summary(matchedglmformulaadjage(contactcolumnsnocattle))

autoglm_MVLR(contactcolumnsnocattle,Agematchvector)
autoglm_contactcolumnsnocattle[[8]]

#########################################GLM-Days in contact with livestock#############
#Univariable 
for (i in contactcolumns+1) {
  print(summary(matchedglmformulaadjage(i)))}

#Multivariable: 
summary(matchedglmformulaadjage(contactcolumnsnocattle+1))
autoglm_MVLR(contactcolumnsnocattle+1,Agematchvector)
`autoglm_contactcolumnsnocattle + 1`[[6]]

###################################################GLM-Livestock activities##### 

names(dat[activitiescolumns])
names(dat[glmactivitycolumns])
names(dat[activitygroupcolumns])

summary(matchedglmformulaadjage(activitiescolumns))
#which are the variables that do not allow the model to be caluclated? First let's remove the variables with 0% frequency:
names(dat[activitiescolumns[which(!sapply(dat[activitiescolumns],function(x)any(x==-1)))]])
#above are the variables with 0% of occurrence
#remove them: 
activitiescolumns<-activitiescolumns[-which(!sapply(dat[activitiescolumns],function(x)any(x==-1)))]
activitiescolumns
summary(matchedglmformulaadjage(activitiescolumns))
#now we have some variables that return NA as coefficient.
which(is.na((matchedglmformulaadjage(activitiescolumns)$coefficients)))-4
activitiescolumns<-activitiescolumns[-(which(is.na((matchedglmformulaadjage(activitiescolumns)$coefficients)))-4)]
summary(matchedglmformulaadjage(activitiescolumns))
#now we are going to remove Activity any other (chance of information bias)
activitiescolumns<-activitiescolumns[-match("ActivityAnyOther",names(dat[activitiescolumns]))]

autoglm_MVLR(activitiescolumns,Agematchvector)
autoglm_activitiescolumns[[11]]
#although we reached this model , we will use the summary variables, divided by type of activity and species.

#Activities categorized by Species
summary(matchedglmformulaadjage(glmactivitycolumns))
#going to remove the AnyActivityFarm because it is colinear with all the other variables. Cattle also, it is colinear with beef and dairy.
glmactivitycolumns<-glmactivitycolumns[-na.omit(match(c("AnyActivityFarm","AnyActivityCattle"),names(dat[glmactivitycolumns])))]
summary(matchedglmformulaadjage(glmactivitycolumns))
autoglm_MVLR(glmactivitycolumns,Agematchvector)
autoglm_glmactivitycolumns[[6]]

#Categorized by type of activity
summary(matchedglmformulaadjage(activitygroupcolumns))

#Removing "any other activity"
activitygroupcolumns<-activitygroupcolumns[-na.omit(match(c("AnyOtherActivity"),names(dat[activitygroupcolumns])))]
autoglm_MVLR(activitygroupcolumns,Agematchvector)
autoglm_activitygroupcolumns[[5]]


##################################GLM - Wild animals#####
#Using a model for wild animal evidence. According to the descriptive statistics analysis, the most robust questions to analyse risk for wild animal exposure is evidence, so I will focus on that.
 
wildevidencecolumns
names(dat[wildevidencecolumns])
#Rats and mice will be removed because it was agreed during the meetings that differentiating them could be innaccurate. Any wild evidence will also be removed from this study (colinearity)

wildevidencecolumns<-wildevidencecolumns[-na.omit(match(match(c("EvidenceWildRats","EvidenceWildMice","anywildevidence"),names(dat)),wildevidencecolumns))]
wildevidencecolumns
names(dat[wildevidencecolumns])
summary(matchedglmformulaadjage(wildevidencecolumns))
autoglm_MVLR(wildevidencecolumns,Agematchvector)
autoglm_MVLR(wildevidencecolumns,Agematchvector)
autoglm_wildevidencecolumns[[9]]

#After the sensitivity analysis it was decided to remove the Wild rabbit variable and run the wild evidence models again: 
newwildevidencecolumns<-wildevidencecolumns[-na.omit(match(match(c("EvidenceWildRabbit"),names(dat)),wildevidencecolumns))]
names(dat[newwildevidencecolumns])
autoglm_MVLR(newwildevidencecolumns,Agematchvector)
autoglm_newwildevidencecolumns[[8]]

########################GLM - Other animal exposures####
names(dat[otheran])
otheran
#Skin animals is alerady represented in the "slughter activities" I will remove that.
otheran<-otheran[-match("SkinAnimals",names(dat[otheran]))]
summary(matchedglmformulaadjage(otheran))
autoglm_MVLR(otheran,Agematchvector)
autoglm_otheran[[5]]

####GLM - Pets 
names(dat[petscolumns])
summary(matchedglmformulaadjage(petscolumns))
petscolumns<-petscolumns[-which(!sapply(dat[petscolumns],function(x)any(x==-1)))]
summary(matchedglmformulaadjage(petscolumns))
autoglm_MVLR(petscolumns,Agematchvector)
autoglm_petscolumns[[10]]

###############################GLM - Water Exposures####
names(dat[allwaterexposure])
names(dat[waterandtreatmentcolumns])
str(dat[waterandtreatmentcolumns])
waterandtreatmentcolumns
#Going to remove "water situation other" and "water troughs" (possibility of information bias because they were open-ended questions) and the treatment variables. I added access to untreated water variable previously.
allwaterexposure<-allwaterexposure[-na.omit(match(match(c("WaterSituationOther","WaterContactOthers","WaterActivityOthers","WaterTroughs"),names(dat)),allwaterexposure))]

waterandtreatmentcolumns<-waterandtreatmentcolumns[-na.omit(match(match(c("WorkWaterOther","WorkWaterOtherTreated","WaterOther","WaterOtherTreated"),names(dat)),waterandtreatmentcolumns))]
waterandtreatmentcolumns<-waterandtreatmentcolumns[-grep("Treated",names(dat[waterandtreatmentcolumns]))]

#Initial M.V. models
summary(matchedglmformulaadjage(allwaterexposure))
summary(matchedglmformulaadjage(waterandtreatmentcolumns))
autoglm_MVLR(allwaterexposure,Agematchvector)
autoglm_MVLR(waterandtreatmentcolumns,Agematchvector)
autoglm_allwaterexposure[[10]]
autoglm_waterandtreatmentcolumns[[7]]








#Now I will use the information from this automatic model to choose the best variables from this section. I will do that for each of the sections

bestvariables<-paste(deparse(autoglm_contactcolumnsnocattle[[match(min(sapply(autoglm_contactcolumnsnocattle,function(x)(x$aic))),(sapply(autoglm_contactcolumnsnocattle,function(x)(x$aic))))]]$terms[[3]]),collapse = "")
bestvariables


####################GLM - Health/ Medical conditions#####

medicalconditionscols
names(dat[medicalconditionscols])
str(dat[medicalconditionscols])

#these variables are not factors yet. Going to transform all of them to factors
#changing the variables to factors, and change the level order.
for (i in 1:length(medicalconditionscols)) {
  dat[medicalconditionscols[i]]<-factor(dat[[medicalconditionscols[i]]], levels = if (length(levels(as.factor(dat[[medicalconditionscols[i]]])))==2) { c(0,-1) } else{c(levels(as.factor(dat[[medicalconditionscols[i]]])))}) }
  dat$HealthDressWound<-factor(dat$HealthDressWound,levels = c(0,-1))
  str(dat[medicalconditionscols])
  
#since this formula does not work with categorical variables (Health Cut and Cover is categorical), I am going to use the cut and not cover nad new dress wound variables. If necessary ,in the final adjustments I will test which variable fits better. I am going to remove Cancer and High pressure because their origin is from an open-ended question (information bias).
medicalconditionscols
names(dat[medicalconditionscols])
summary(matchedglmformulaadjage(medicalconditionscols))
medicalconditionscols<-medicalconditionscols[-(na.omit(match(c("HealthCutandCover","HealthCancer","HealthHighPressure", "HealthOther"),names(dat[medicalconditionscols]))))]
medicalconditionscols
summary(matchedglmformulaadjage(medicalconditionscols))
#Removing variables that returned NA as coefficient
medicalconditionscols<-medicalconditionscols[-(which(is.na((matchedglmformulaadjage(medicalconditionscols)$coefficients)))-4)]
names(dat[medicalconditionscols])
summary(matchedglmformulaadjage(medicalconditionscols))

autoglm_MVLR(medicalconditionscols,Agematchvector)
autoglm_medicalconditionscols[[10]]

#testing the most relevant combinations of cutting and covering and understanding which variable makes more sense in the model
#Just cutting
summary(glm(dat$dichotomoustype~dat$HealthCutScratch, family="binomial"))
#just covering wounds - we lose too many observations using only this variable. Everyone that did not cut falls off the model
summary(glm(dat$dichotomoustype~dat$HealthDressWound, family="binomial"))
#Putting these two variables in the model may not be the solution. The logistic regression does not run. Probably because of multicolinearity between the two variables ( all the 0s in the cutting variable are Nas in the covering variable)
summary(glm(dat$dichotomoustype~dat$HealthDressWound+dat$HealthCutScratch, family="binomial"))
summary(glm(dat$dichotomoustype~dat$HealthDressWound*dat$HealthCutScratch, family="binomial"))
matchedglmformulaadjage(match(c("HealthDressWound", "HealthCutScratch"),names(dat)))

# Therefore we created 3 other variables 
#The categorical variable
summary(glm(dat$dichotomoustype~as.factor(dat$HealthCutandCover), family="binomial"))
#and two dichotomous variables
summary(glm(dat$dichotomoustype~as.factor(dat$HealthCutandnotCover), family="binomial"))
summary(glm(dat$dichotomoustype~as.factor(dat$NewDressWound), family="binomial"))
# the dichotomous variables return -1 when a participant cuts and covers (NewdressWound variable ) or when cuts and does not cover (Cutandnotcover variable) and 0 for all the other answers (unsure, does not have cuts and covering or not covering depending on the variable). The categorical variable, on the other hand, diferentiates all these answers. so It we will give preference.

############################## GLM - Other Exposures####
glmotherexposures
names(dat[glmotherexposures])
summary(matchedglmformulaadjage(glmotherexposures))
autoglm_MVLR(glmotherexposures,Agematchvector)
autoglm_glmotherexposures[[5]]




##########################Final multivariable models#####
bestpreliminarymodels<- list(autoglm_contactcolumnsnocattle,`autoglm_contactcolumnsnocattle + 1`,autoglm_glmactivitycolumns,autoglm_activitygroupcolumns, autoglm_wildevidencecolumns, autoglm_petscolumns, autoglm_otheran, autoglm_allwaterexposure, autoglm_waterandtreatmentcolumns, autoglm_medicalconditionscols,autoglm_glmotherexposures)
bestvariables<-as.list(rep(0, length(bestpreliminarymodels)))

#This loop will choose the model with the lowest AIC from each category
for (i in 1:length(bestpreliminarymodels)) { 
  bestvariables[[i]]<-paste(deparse(bestpreliminarymodels[[i]][[match(min(sapply(bestpreliminarymodels[[i]],function(x)(x$aic))),(sapply(bestpreliminarymodels[[i]],function(x)(x$aic))))]]$terms[[3]]),collapse = "") }
  
#The list with the list of variables chosen for the final model building:
bestvariables

# Number of variables chosen by each section: 
lengths(regmatches(bestvariables, gregexpr("\\+", bestvariables)))-2
# Total number of variables chosen for the final modelling : 
sum(lengths(regmatches(bestvariables, gregexpr("\\+", bestvariables)))-2)

#Now that we have the variables, we are going to build the final model. First we are going to check what the automatic model selection will encounter:
 
#We will convert the text of the exposures to number of the columns so that we can run the automatic modelling.

bestlistvariables<-list(0)
for (i in 1:length(bestvariables)) { 
  bestlistvariables[[i]]<- unlist(strsplit(gsub(" ", "",bestvariables[[i]]),split = "+", fixed=T))
  bestlistvariables[[i]]<-bestlistvariables[[i]][4:length(bestlistvariables[[i]])]
  bestlistvariables[[i]]<-as.numeric(sapply(bestlistvariables[[i]],function(x)which(names(dat)==x)))
}

bestlistvariables

#putting all the number of columns together:
bestunlistvariables<-unlist(bestlistvariables)
bestunlistvariables

#now we run the function : 
autoglm_MVLR(bestunlistvariables,Agematchvector) 
autoglm_bestunlistvariables[[24]]

#Jonathan proposed I could also use the step function. results were similar, however the model was greater (more variables included)

modelthroughstep<-stepAIC(matchedglmformulaadjage(bestunlistvariables),direction = "backward", scope=list(lower=dichotomoustype ~ Sex+Gen.RuralityHomeaddr+Age))
summary(modelthroughstep)

#Some Medical condition variables appear in the models as protective, specially Hstory of smoking (which we saw is  negatively correlated to livestock activities ), HayFever and Antibiotics ( could it that be that antibiotic treatment blocks the infection?).


#From this point on, I will make mannual adjustments on the model
#I will change the variable Age to AgeOver16, because the youngest participants were 16 years old. that means all the to calculate the OR for someone who is 40 years old, the coefficient should be multiplied by 14. 

manualmodel1<-glm(dichotomoustype~ Sex+Gen.RuralityHomeaddr+AgeOver16+ContactDairy+AnyActivityBeef +AnySlaughterActivity+EvidenceAnyRodent+ EvidenceWildRabbit+WaterActivitySwimming+HealthCutandCover, data=dat,family = "binomial")
summary(manualmodel1)

# Sensitivity analysis. Because of the concentration of controls being interviewed during the end and beginning of the years, I wll run the same model, but with participants limited to 90 days after 21st of June adn 90 days before (all the controls are included in this range)
sensitivitymodel1<-glm(dichotomoustype~ Sex+Gen.RuralityHomeaddr+AgeOver16+ContactDairy+AnyActivityBeef +AnySlaughterActivity+EvidenceAnyRodent+ EvidenceWildRabbit+WaterActivitySwimming+HealthCutandCover, data=dat[dat$IntervieworSickSolsticeDist>90,],family = "binomial")
summary(sensitivitymodel1)

#removing Evidence of Rabbits
 newwildevidencecolumns
summary(matchedglmformulaadjage(newwildevidencecolumns))
autoglm_MVLR(newwildevidencecolumns,Agematchvector)
autoglm_newwildevidencecolumns[[8]]

#Running the preliminary models again
newbestpreliminarymodels<- list(autoglm_contactcolumnsnocattle,`autoglm_contactcolumnsnocattle + 1`,autoglm_glmactivitycolumns,autoglm_activitygroupcolumns, autoglm_newwildevidencecolumns, autoglm_petscolumns, autoglm_otheran, autoglm_allwaterexposure, autoglm_waterandtreatmentcolumns, autoglm_medicalconditionscols,autoglm_glmotherexposures)
newbestvariables<-as.list(rep(0, length(newbestpreliminarymodels)))

newbestlistvariables<-list(0)

#This loop will choose the model with the lowest AIC from each category
for (i in 1:length(newbestpreliminarymodels)) { 
  newbestvariables[[i]]<-paste(deparse(newbestpreliminarymodels[[i]][[match(min(sapply(newbestpreliminarymodels[[i]],function(x)(x$aic))),(sapply(newbestpreliminarymodels[[i]],function(x)(x$aic))))]]$terms[[3]]),collapse = "") }



for (i in 1:length(newbestvariables)) { 
  newbestlistvariables[[i]]<- unlist(strsplit(gsub(" ", "",newbestvariables[[i]]),split = "+", fixed=T))
  newbestlistvariables[[i]]<-newbestlistvariables[[i]][4:length(newbestlistvariables[[i]])]
  newbestlistvariables[[i]]<-as.numeric(sapply(newbestlistvariables[[i]],function(x)which(names(dat)==x)))
}

newbestlistvariables

#putting all the number of columns together:
newbestunlistvariables<-unlist(newbestlistvariables)
newbestunlistvariables

#now we run the function : 
autoglm_MVLR(newbestunlistvariables,Agematchvector) 
autoglm_newbestunlistvariables[[1]]
newmodelthroughstep<-stepAIC(matchedglmformulaadjage(newbestunlistvariables),direction = "backward", scope=list(lower=dichotomoustype ~ Sex+Gen.RuralityHomeaddr+Age))
summary(newmodelthroughstep)

manualmodel2<-glm(dichotomoustype~ Sex+Gen.RuralityHomeaddr+AgeOver16+ContactDairy+AnyActivityBeef +AnySlaughterActivity+EvidenceAnyRodent+ EvidenceWildDeer+WaterActivitySwimming+HealthCutandCover, data=dat,family = "binomial")
summary(manualmodel2)

sensitivitymodel2<-glm(dichotomoustype~ Sex+Gen.RuralityHomeaddr+AgeOver16+ContactDairy+AnyActivityBeef +AnySlaughterActivity+EvidenceAnyRodent+ EvidenceWildDeer+WaterActivitySwimming+HealthCutandCover, data=dat[dat$IntervieworSickSolsticeDist>90,],family = "binomial")
summary(sensitivitymodel1)

ctab(as.factor(ifelse(predict(manualmodel2)>0,"positive","negative")),dat$Type)

#The following is a formula to evaluate how many respondents are correctly predicted by the model
accuracy<-ctab(factor(ifelse(predict(eval(parse(text=paste("glm(dichotomoustype~",paste(deparse(manualmodel2$terms[[3]]),collapse = ""),",data = dat, family = \"binomial\")"))))>0,"Positive","Negative")),dat[as.numeric(rownames(data.frame(predict(eval(parse(text=paste("glm(dichotomoustype~",paste(deparse(manualmodel2$terms[[3]]),collapse = ""),",data = dat, family = \"binomial\")"))))))),7])

accuracy$table<-accuracy$table[c(2,1),]
accuracy$table

#Sensitivity of the model: 
accuracy$table[1,1]/sum(accuracy$table[,1])

#Especificity of the model: 
accuracy$table[2,2]/sum(accuracy$table[,2])


