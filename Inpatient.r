#Inpatient_orignal <- read.csv("C:/Users/praneetha/Desktop/Inpatient.csv",sep=",",header=TRUE)
Inpatient <- read.csv("C:/Users/praneetha/Desktop/Inpatient.csv",sep=",",header=TRUE)
#install.packages("tidyverse")
library(tidyverse)
library(data.table)
library(forcats) 
## To Remove The Dollar Symbol
Inpatient$Average.Covered.Charges <- as.numeric(gsub("[\\$,]", "", Inpatient$Average.Covered.Charges))
Inpatient$Average.Medicare.Payments <- as.numeric(gsub("[\\$,]", "", Inpatient$Average.Medicare.Payments))

Inpatient$Average.Total.Payments <- as.numeric(gsub("[\\$,]", "", Inpatient$Average.Total.Payments))
Inpatient$ï..DRG.Definition<-as.numeric(gsub("\\D", "", Inpatient$ï..DRG.Definition))
Inpatient$ï..DRG.Definition <- sub("^", "D", Inpatient$ï..DRG.Definition)


## To Rename the Column Names
setnames(Inpatient, old=c("ï..DRG.Definition","Provider.Id","Provider.Name","Provider.Street.Address","Provider.City","Provider.State"
    ,"Provider.Zip.Code","Hospital.Referral.Region.Description","Total.Discharges","Average.Covered.Charges","Average.Total.Payments","Average.Medicare.Payments")
     , new=c("DRG_Definition","Provider_Id", "Provider_Name","Provider_Street_Address","Provider_City",
    "Provider_State","Provider_Zip_Code","Hospital_Ref_Reg.Desc","Total_Discharges","Avg_Covered_Charges","Avg_Total_Payments","Avg_Medicare_Payments"))


## To View Column Names
View(Inpatient)
Inpatient<-select(Inpatient,
                       DRG_Definition, 
                       Provider_State, 
                       Provider_City,
                       Provider_Name,
                       Total_Discharges,
                       Avg_Covered_Charges,
                       Avg_Total_Payments,
                       Avg_Medicare_Payments)

View(Inpatient)

#View(Inpatient_orignal)
colnames(Inpatient)

ggplot(data = Inpatient) +
  geom_boxplot(mapping = aes(x = reorder(DRG_Definition, Total_Discharges, FUN = median), y = Total_Discharges),color="blue",alpha="0.05") +
  coord_flip()+
  xlab("DRG_Definition") +
  ylab("My y label") 
  

##To display results based on the drug "470 - MAJOR JOINT REPLACEMENT OR REATTACHMENT OF LOWER EXTREMITY W/O MCC"
Inpatient_Drug<-Inpatient%>%filter(DRG_Definition =="D470")%>%
  group_by(Provider_State)
  
## To display only few records.
Inpatient_Drug<-select(Inpatient_Drug,
                       DRG_Definition, 
                       Provider_State, 
                       Provider_City,
                       Provider_Name,
                       Total_Discharges,
                       Avg_Covered_Charges,
                       Avg_Total_Payments,
                       Avg_Medicare_Payments)

View(Inpatient_Drug)

ggplot(data = Inpatient_Drug, mapping = aes(x = Total_Discharges, y = Avg_Covered_Charges)) + 
  geom_point(mapping = aes(color = Provider_State,se=FALSE),position = "jitter",alpha=0.20) + 
  geom_smooth()


ggplot(Inpatient_Drug, aes(Provider_State, Total_Discharges)) + 
  geom_point()+
  geom_line()


ggplot(Inpatient_Drug, aes(Provider_State, Total_Discharges)) + 
  geom_point()+
  geom_line()

## To get the data of Newyork city for the drg D470


Inpatient_Drug_D470_NY<- Inpatient_Drug%>%filter(Provider_State=="NY")%>%
  arrange(Total_Discharges)


## the total number of dicharges are more at the provider_name=HOSPITAL FOR SPECIAL SURGERY.
Inpatient_Drug_D470_NY<-select(Inpatient_Drug_D470_NY,
                       DRG_Definition, 
                       Provider_State,
                       Provider_City,
                       Provider_Name,
                       Total_Discharges,
                       Avg_Covered_Charges)
View(Inpatient_Drug_D470_NY)
## but the average medicare payments are more in Ak.

## To find which state uses the medicare payments more for this drug
ggplot(data = Inpatient_Drug) + 
  stat_summary(
    mapping = aes(x = Provider_State, y = Avg_Medicare_Payments))

## To see why average medicare payments are more in AK
## this ccould be because the number of providers are less so avg_medicare is more
#The verage cases are more for NY and less for AK but still AK has more avg_medicare_payments
Inpatient_med_AK<-Inpatient_Drug%>%filter(Provider_State =="AK")%>%
  arrange(Avg_Medicare_Payments)

View(Inpatient_med_AK)

## To see why average medicare payments are more in AK

Inpatient_med_MD<-Inpatient_Drug%>%filter(Provider_State =="AL")%>%
  arrange(Avg_Medicare_Payments)

View(Inpatient_med_MD)

## To find the average total payments
ggplot(data = Inpatient_Drug) + 
  stat_summary(
    mapping = aes(x = Provider_State, y = Avg_Total_Payments))

