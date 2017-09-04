#Packages Used
require(readxl)
require(ggplot2)
require(stringr)
require(data.table)
require(maps)
require(reshape2)
require(corrplot)
require(caret)
require(C50)
require(rpart)
require(RColorBrewer)
require(rattle)
require(rpart.plot)
require(pROC)

#File- prescriber_detailed#
prescriber_detailed <- read_excel("prescriber.detailed.xlsx")
head(prescriber_detailed)
sapply(prescriber_detailed,class)
prescriber_detailed$bene_count <- as.integer(prescriber_detailed$bene_count)
colSums(is.na(prescriber_detailed)) #bene_count- 256128 NA's found 
#suppresed values are between 1 and 10 so taken mean
prescriber_detailed$bene_count[is.na(prescriber_detailed$bene_count)] <- as.integer(mean(1:10)) 
summary(prescriber_detailed)

#File - Prescriber_summary
prescriber_summary <- read_excel("prescriber.summary.xlsx")
summary(prescriber_summary)
sapply(prescriber_summary ,class)

#Datatype conversion
prescriber_summary$total_claim_count_ge65 <- as.numeric(prescriber_summary$total_claim_count_ge65)
prescriber_summary$total_drug_cost_ge65 <- as.numeric(prescriber_summary$total_drug_cost_ge65)
prescriber_summary$total_day_supply_ge65 <- as.numeric(prescriber_summary$total_day_supply_ge65)
prescriber_summary$brand_claim_count <- as.numeric(prescriber_summary$brand_claim_count)
prescriber_summary$brand_drug_cost <- as.numeric(prescriber_summary$brand_drug_cost)
prescriber_summary$generic_claim_count <- as.numeric(prescriber_summary$generic_claim_count)
prescriber_summary$generic_drug_cost <- as.numeric(prescriber_summary$generic_drug_cost)
prescriber_summary$other_claim_count <- as.numeric(prescriber_summary$other_claim_count)
prescriber_summary$other_drug_cost <- as.numeric(prescriber_summary$other_drug_cost)

prescriber_summary$ge65_suppress_flag <- as.factor(prescriber_summary$ge65_suppress_flag)
prescriber_summary$bene_count_ge65_suppress_flag <- as.factor(prescriber_summary$bene_count_ge65_suppress_flag)
prescriber_summary$other_suppress_flag<- as.factor(prescriber_summary$other_suppress_flag)
prescriber_summary$generic_suppress_flag <- as.factor(prescriber_summary$generic_suppress_flag)
prescriber_summary$brand_suppress_flag <- as.factor(prescriber_summary$brand_suppress_flag)

#--NA-Treatment--#
colSums(is.na(prescriber_summary))

# (*) Indicates total_claim_count_ge65  is between 1 and 10
prescriber_summary$total_claim_count_ge65[is.na(prescriber_summary$total_claim_count_ge65) & prescriber_summary$ge65_suppress_flag == "*"] <- as.integer(mean(1:10)) 
# (#) indicates less than 65 y.o group is total claim is between 1 and 10
prescriber_summary$total_claim_count_ge65[is.na(prescriber_summary$total_claim_count_ge65) & prescriber_summary$ge65_suppress_flag == "#"] <- 
  prescriber_summary$total_claim_count[is.na(prescriber_summary$total_claim_count_ge65) & prescriber_summary$ge65_suppress_flag == "#"] - 5

# (*) Indicates claim_counts is between 1 and 10
prescriber_summary$brand_claim_count[is.na(prescriber_summary$brand_claim_count) & prescriber_summary$brand_suppress_flag == "*"] <- as.integer(mean(1:10)) 
prescriber_summary$generic_claim_count[is.na(prescriber_summary$generic_claim_count) & prescriber_summary$generic_suppress_flag == "*"] <-as.integer(mean(1:10))
prescriber_summary$other_claim_count[is.na(prescriber_summary$other_claim_count) & prescriber_summary$other_suppress_flag == "*"] <- as.integer(mean(1:10))

# (#) Indicates remaining claim_counts is between 1 and 10
prescriber_summary$brand_claim_count[is.na(prescriber_summary$brand_claim_count) & prescriber_summary$brand_suppress_flag == "#"] <-  
  prescriber_summary$total_claim_count[is.na(prescriber_summary$brand_claim_count) & prescriber_summary$brand_suppress_flag == "#"] - 
  (prescriber_summary$generic_claim_count[is.na(prescriber_summary$brand_claim_count) & prescriber_summary$brand_suppress_flag == "#"] + 
     prescriber_summary$other_claim_count[is.na(prescriber_summary$brand_claim_count) & prescriber_summary$brand_suppress_flag == "#"])

prescriber_summary$generic_claim_count[is.na(prescriber_summary$generic_claim_count) & prescriber_summary$generic_suppress_flag == "#"] <- 
  prescriber_summary$total_claim_count[is.na(prescriber_summary$generic_claim_count) & prescriber_summary$generic_suppress_flag == "#"] - 
  (prescriber_summary$brand_claim_count[is.na(prescriber_summary$generic_claim_count) & prescriber_summary$generic_suppress_flag == "#"]+ 
     prescriber_summary$other_claim_count[is.na(prescriber_summary$generic_claim_count) & prescriber_summary$generic_suppress_flag == "#"])

prescriber_summary$other_claim_count[is.na(prescriber_summary$other_claim_count) & prescriber_summary$other_suppress_flag == "#"] <- 
  prescriber_summary$total_claim_count[is.na(prescriber_summary$other_claim_count) & prescriber_summary$other_suppress_flag == "#"] - 
  (prescriber_summary$brand_claim_count[is.na(prescriber_summary$other_claim_count) & prescriber_summary$other_suppress_flag == "#"]+ 
     prescriber_summary$generic_claim_count[is.na(prescriber_summary$other_claim_count) & prescriber_summary$other_suppress_flag == "#"])

#cost of drug value are still suppressed. what can be done?
colSums(is.na(prescriber_summary))

#Removing flag column
prescriber_summary<- prescriber_summary[,-c(9,11,15,18,21)] 
summary(prescriber_summary)

#write file
write.csv(prescriber_summary,"Prescriber_Summary_No_flag.csv",row.names = F)

#--File--PUF.Deatiled--#
puf_detailed <- read_excel("PUF.detailed.xlsx")
head(puf_detailed)
summary(puf_detailed)
colSums(is.na(puf_detailed)) # no null values

#--File--PUF.summary--#
puf_summary <- read_excel("PUF.summary.xlsx")
head(puf_summary)
sapply(puf_summary,class)
summary(puf_summary)
puf_summary$number_of_drug_hcpcs <- as.numeric(puf_summary$number_of_drug_hcpcs)
puf_summary$total_drug_services <- as.numeric(puf_summary$total_drug_services)
puf_summary$total_drug_medicare_allowed_amt <- as.numeric(puf_summary$total_drug_medicare_allowed_amt)
puf_summary$number_of_med_hcpcs <- as.numeric(puf_summary$number_of_med_hcpcs)
puf_summary$total_med_services <- as.numeric(puf_summary$total_med_services)
puf_summary$total_med_medicare_allowed_amt <- as.numeric(puf_summary$total_med_medicare_allowed_amt)

colSums(is.na(puf_summary)) #2140 doctors doesn't provide drug & med services?
