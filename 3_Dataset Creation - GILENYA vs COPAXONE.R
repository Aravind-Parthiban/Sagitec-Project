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

#File Pres_Cons(e.g. for dataset creation)
pres_conso <- read_excel("PUF_pres_conso.xlsx")
summary(pres_conso)
pres_conso$diagnostic <- as.numeric(pres_conso$diagnostic)
pres_conso$other <- as.numeric(pres_conso$other)
pres_conso$diagnostic_services_percentage <- as.numeric(pres_conso$diagnostic_services_percentage)
pres_conso$other_services_percentage <- as.numeric(pres_conso$other_services_percentage)
pres_conso$diagnostic_services_rank_in_his_speciality <- as.numeric(pres_conso$diagnostic_services_rank_in_his_speciality)
pres_conso$other_services_rank_in_his_speciality <- as.numeric(pres_conso$other_services_rank_in_his_speciality)
pres_conso$top1_drug_cost_prescribed <- as.numeric(pres_conso$top1_drug_cost_prescribed)
pres_conso$top2_drug_cost_prescribed <- as.numeric(pres_conso$top2_drug_cost_prescribed)
pres_conso$top3_drug_cost_prescribed <- as.numeric(pres_conso$top3_drug_cost_prescribed)
pres_conso$top4_drug_cost_prescribed <- as.numeric(pres_conso$top4_drug_cost_prescribed)
pres_conso$top5_drug_cost_prescribed <- as.numeric(pres_conso$top5_drug_cost_prescribed)
pres_conso$branded_drugs_cost_percentage <- as.numeric(pres_conso$branded_drugs_cost_percentage)
pres_conso$generic_drugs_cost_percentage <- as.numeric(pres_conso$generic_drugs_cost_percentage)
pres_conso$bene_count <- as.numeric(pres_conso$bene_count)
pres_conso$total_claim_count <- as.numeric(pres_conso$total_claim_count)
pres_conso$total_drug_cost <-as.numeric(pres_conso$total_drug_cost)
pres_conso$total_day_supply <- as.numeric(pres_conso$total_day_supply)
pres_conso$brand_drug_cost <- as.numeric(pres_conso$brand_drug_cost)
pres_conso$brand_claim_count <- as.numeric(pres_conso$brand_claim_count)
pres_conso$generic_claim_count <- as.numeric(pres_conso$generic_claim_count)
pres_conso$generic_drug_cost <- as.numeric(pres_conso$generic_drug_cost)
pres_conso$other_claim_count <- as.numeric(pres_conso$other_claim_count)
pres_conso$other_drug_cost <- as.numeric(pres_conso$other_drug_cost)
colSums(is.na(pres_conso))#so many NA which can be treated only after define problem statement


#Classification "GILENYA" vs "COPAXONE"

#--dataset creation--#

#Merge
pres_features <- read.csv("prescriber_detailed_feature.csv")
summary(pres_features)

#Filter records with only "GILENYA" & "COPAXONE" as brand name in Prescriber detailed file
dt_pres_features <- data.table(pres_features)
dt_pres_brands <- dt_pres_features[dt_pres_features$brand_name == "GILENYA" | dt_pres_features$brand_name == "COPAXONE"]

#Merge it with consolidate file derived from (PUF & prescriber files)
m1_Puf_pres_conso_brand <- merge(dt_pres_brands,pres_conso,by = "doc_id")
write.csv(m1_Puf_pres_conso_brand,"Puf_pres_conso_brand.csv",row.names = F)
summary(m1_Puf_pres_conso_brand)
colSums(is.na(m1_Puf_pres_conso_brand))

#Remove city & state - redundant column
m1_Puf_pres_conso_brand <- m1_Puf_pres_conso_brand[,-c(12,13)]

#--missing value treatment--#
#Total service count is the sum of (diagnostic,therapeutic & others counts)
sum(is.na(m1_Puf_pres_conso_brand$total_services_count)) #no null values

m1_Puf_pres_conso_brand$therapeutic[is.na(m1_Puf_pres_conso_brand$therapeutic)] <- 0
m1_Puf_pres_conso_brand$diagnostic[is.na(m1_Puf_pres_conso_brand$diagnostic) & is.na(m1_Puf_pres_conso_brand$other)] <- 0
m1_Puf_pres_conso_brand$other[is.na(m1_Puf_pres_conso_brand$other) & m1_Puf_pres_conso_brand$diagnostic == 0] <- 0

m1_Puf_pres_conso_brand$diagnostic[is.na(m1_Puf_pres_conso_brand$diagnostic)] <- 
  (m1_Puf_pres_conso_brand$total_services_count[is.na(m1_Puf_pres_conso_brand$diagnostic)]) - 
  (m1_Puf_pres_conso_brand$therapeutic[is.na(m1_Puf_pres_conso_brand$diagnostic)] + 
     m1_Puf_pres_conso_brand$other[is.na(m1_Puf_pres_conso_brand$diagnostic)])

m1_Puf_pres_conso_brand$other[is.na(m1_Puf_pres_conso_brand$other)] <- 
  (m1_Puf_pres_conso_brand$total_services_count[is.na(m1_Puf_pres_conso_brand$other)]) - 
  (m1_Puf_pres_conso_brand$therapeutic[is.na(m1_Puf_pres_conso_brand$other)] + 
     m1_Puf_pres_conso_brand$diagnostic[is.na(m1_Puf_pres_conso_brand$other)])

#Imputing missing value for the precentage of various services
m1_Puf_pres_conso_brand$diagnostic_services_percentage[is.na(m1_Puf_pres_conso_brand$diagnostic_services_percentage)] <-
  (m1_Puf_pres_conso_brand$diagnostic[is.na(m1_Puf_pres_conso_brand$diagnostic_services_percentage)] / 
     m1_Puf_pres_conso_brand$total_services_count[is.na(m1_Puf_pres_conso_brand$diagnostic_services_percentage)]) * 100

m1_Puf_pres_conso_brand$other_services_percentage[is.na(m1_Puf_pres_conso_brand$other_services_percentage)] <- 
  (m1_Puf_pres_conso_brand$other[is.na(m1_Puf_pres_conso_brand$other_services_percentage)]/
     m1_Puf_pres_conso_brand$total_services_count[is.na(m1_Puf_pres_conso_brand$other_services_percentage)]) * 100

m1_Puf_pres_conso_brand$therapeutic_services_percentage[is.na(m1_Puf_pres_conso_brand$therapeutic_services_percentage)] <- 
  (m1_Puf_pres_conso_brand$therapeutic[is.na(m1_Puf_pres_conso_brand$therapeutic_services_percentage)]/
     m1_Puf_pres_conso_brand$total_services_count[is.na(m1_Puf_pres_conso_brand$therapeutic_services_percentage)]) * 100

#Brand,generic,others claim count (from pres_summary)
pres_sum<-read.csv("Prescriber_Summary_No_flag.csv")
m2<-merge(m1_Puf_pres_conso_brand,pres_sum, by= "doc_id")
colSums(is.na(m2))


#Again Remove redundant columns
dt_m2<- data.table(m2)
colSums(is.na(dt_m2))

dt_m3<-dt_m2[,c("city","state","therapeutic_services_rank_in_his_speciality","diagnostic_services_rank_in_his_speciality",
                "other_services_rank_in_his_speciality","top1_drug_prescribed","top2_drug_prescribed","top3_drug_prescribed",
                "top4_drug_prescribed","top5_drug_prescribed","top1_drug_cost_prescribed","top2_drug_cost_prescribed",
                "top3_drug_cost_prescribed","top4_drug_cost_prescribed","top5_drug_cost_prescribed","bene_count.y",
                "total_claim_count.y","total_drug_cost.y","total_day_supply.y","brand_claim_count.x","brand_drug_cost.x",
                "generic_claim_count.x","other_claim_count.x","other_drug_cost.x","nppes_provider_city.y",
                "nppes_provider_state.y","generic_drug_cost.x"):=NULL]

colSums(is.na(dt_m3))

df1<-dt_m3
df2<-df1[(!is.na(df1$brand_drug_cost.y)) & (!is.na(df1$generic_drug_cost.y)) & (!is.na(df1$total_drug_cost_ge65)) & 
           (!is.na(df1$other_drug_cost.y))] 
colSums(is.na(df2))#NO MISSING Values!!

#Write the 'FINAL' dataset for analysis
write.csv(df2,"GILENYA_COPAXONE_cons.csv",row.names = F)
gil_cop_conso <- read.csv("GILENYA_COPAXONE_cons.csv")
summary(gil_cop_conso)


