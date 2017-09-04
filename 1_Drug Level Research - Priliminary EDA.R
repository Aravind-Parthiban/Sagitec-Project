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

setwd("~/Desktop/SSN_SACE_2017_Jan-master")

#File- prescriber_detailed#
prescriber_detailed <- read_excel("prescriber.detailed.xlsx")
head(prescriber_detailed)
sapply(prescriber_detailed,class)
prescriber_detailed$bene_count <- as.integer(prescriber_detailed$bene_count)
colSums(is.na(prescriber_detailed)) #bene_count- 256128 NA's found 
#suppresed values are between 1 and 10 so taken mean
prescriber_detailed$bene_count[is.na(prescriber_detailed$bene_count)] <- as.integer(mean(1:10)) 
summary(prescriber_detailed)

#Drug research-preliminary EDA#
head(prescriber_detailed$drug_name) #special characters present in drug_names
head(prescriber_detailed$generic_name) 
sum(grepl('[^[:alnum:]]', prescriber_detailed$drug_name))

#Removing Special characters from Drug_name & Generic_name
prescriber_detailed$drug_name <- gsub("-"," ",prescriber_detailed$drug_name)
prescriber_detailed$drug_name <- gsub("/"," ",prescriber_detailed$drug_name)
prescriber_detailed$generic_name <- gsub("-"," ",prescriber_detailed$generic_name)
prescriber_detailed$generic_name <- gsub("/"," ",prescriber_detailed$generic_name)

#750 unique gereric drug names
length(unique(prescriber_detailed$generic_name)) 

#1119 unique drug name which includes both branded and generic
length(unique(prescriber_detailed$drug_name))

#429 unique gerneric drugs names used in drug names
length(intersect(prescriber_detailed$drug_name,prescriber_detailed$generic_name)) 

#(not in) function
'%!in%' <- function(x,y)!('%in%'(x,y)) 

#690 unique brand drug names in drug name
#---XX--length(unique(prescriber_detailed$drug_name)) - length(intersect(prescriber_detailed$drug_name,prescriber_detailed$generic_name)) 
length(unique(prescriber_detailed$drug_name)[unique(prescriber_detailed$drug_name) %!in% 
                                               intersect(prescriber_detailed$drug_name,prescriber_detailed$generic_name)])

#Store the unique_brand_name as vector
uniq_brand <- unique(prescriber_detailed$drug_name)[unique(prescriber_detailed$drug_name) %!in% 
                                                      intersect(prescriber_detailed$drug_name,prescriber_detailed$generic_name)]
uniq_generic <- unique(prescriber_detailed$generic_name)

#Create a new column brand_name
prescriber_detailed$brand_name[prescriber_detailed$drug_name %in% uniq_brand == T] <- 
  prescriber_detailed$drug_name[prescriber_detailed$drug_name %in% uniq_brand == T]

#64.3 % of Total Drug prescribed are Branded  & only 35.7 % are generic drug name being prescribed
mean(is.na(prescriber_detailed$brand_name))

#For Further analysis drug level details required.(out of scope)

#New Features (cost/drug)
prescriber_detailed$cos_per_drug <- round((prescriber_detailed$total_drug_cost/prescriber_detailed$total_day_supply),2)

#write the file
write.csv(prescriber_detailed,"prescriber_detailed_feature.csv",row.names = F)

##--Top--#
#Top 5 Prescribers
barplot(head(summary(as.factor(prescriber_detailed$doc_id)),n=5))

#Top 5 prescribed Generic Drug
barplot(head(sort(table(prescriber_detailed$generic_name),decreasing = T),n=5))

#Top 5 prescribed Brand Drug
barplot(head(sort(table(prescriber_detailed$brand_name),decreasing = T ),n=5))

#Top 5 Prescriber States
barplot(head(sort(table(prescriber_detailed$nppes_provider_state),decreasing = T),n=5))

#Top 5 Prescriber City
barplot(head(sort(table(prescriber_detailed$nppes_provider_city),decreasing = T),n=5))

#Top 5 benficiacry count
barplot(head(sort(prescriber_detailed$bene_count,decreasing = T),n=5))

#Top 5 Total claim count
barplot(head(sort(prescriber_detailed$total_claim_count,decreasing = T),n=5))

#Top 5 Total day supply
barplot(head(sort(prescriber_detailed$total_day_supply,decreasing = T),n=5))

#Top 5 costliest branded Drug-- most drug prices differs between a range of values
prescriber_detailed$brand_name[prescriber_detailed$cos_per_drug %in% 
                                 head(sort(prescriber_detailed$cos_per_drug[!is.na(prescriber_detailed$brand_name)],decreasing =T))]

#Top 5 costliest generic Drug
prescriber_detailed$generic_name[prescriber_detailed$cos_per_drug %in% 
                                   head(sort(prescriber_detailed$cos_per_drug[is.na(prescriber_detailed$brand_name)],decreasing = T))]
barplot(head(sort(prescriber_detailed$cos_per_drug[(is.na(prescriber_detailed$brand_name))],decreasing = T),n=5))

