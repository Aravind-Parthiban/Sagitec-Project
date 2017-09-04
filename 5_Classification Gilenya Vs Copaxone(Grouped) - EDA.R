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
require(plotly)

#Read Dataset
dtg <- read.csv("GILENYA_COPAXONE_conso_grp.csv")

#Count of Drug prescribed
ggplotly(ggplot(dtg,aes(dtg$group,fill= brand_name)) + geom_bar(stat = "count") + labs(title = "Brand Drug Prescribed",x="Brand Drug",y="Count"),tooltip = "all")

#Drug Beneficiary count
ggplotly(ggplot(dtg,aes(x=dtg$group, y=dtg$bene_count,fill = brand_name))+ geom_bar(stat ="identity") + labs(title = "Brand Drug Bene_count",x="Brand Drug",y="Total Bene_Count"),tooltip = "all")

#Total Claim Count
ggplot(dtg,aes(x=dtg$group, y=dtg$total_claim_count,fill = brand_name))+ geom_bar(stat ="identity") + labs(title = "Brand Drug Total Claim Count",x="Brand Drug",y="Total Claim Count")

#Total Day Supply
ggplot(dtg,aes(x=dtg$group, y=dtg$total_day_supply,fill = brand_name))+ geom_bar(stat ="identity") + labs(title = "Brand Drug Total Day Supply",x="Brand Drug",y="Total Day Supply Count")

#Total Drug Cost
ggplot(dtg,aes(x=dtg$group, y=dtg$total_drug_cost,fill = brand_name))+ geom_bar(stat ="identity") + labs(title = "Total Drug Cost",x="Brand Drug",y="Total Drug Cost")

#Median cost per Drug
ggplotly(ggplot(dtg,aes(x=dtg$group,y=dtg$cos_per_drug,colour=brand_name)) + geom_boxplot(position = "dodge") +labs(title = "Median Cost per Drug",x="Brand Drug",y="Cost Per Drug"),tooltip = "all")

#New data created removing duplicate doc_id (only for EDA)
rep_doc_id<- dtg[duplicated(dtg$doc_id),1]
dtr1<-dtg[rep_doc_id,]
dtr2<-dtg[dtg$group == "COPAXONE",]
dtr3 <- dtg[dtg$group == "GILENYA",]
dtr<-rbind(dtr1,dtr2,dtr3)
#write.csv(dtr,"GILENYA_COPAXONE_conso_grp_duprm.csv",row.names = F)

#Overall No.of patients
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$number_of_patients,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Number of Patients",x="Brand Drug",y="Total No. of Patients"),tooltip = "all")

#Overall Total medicare amount allowed
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$total_medicare_allowed_amount,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Medicare Amount Allowed",x="Brand Drug",y="Total medicare amount allowed"),tooltip = "all")

#Overall Total Service performed
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$services_performed,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Service performed",x="Brand Drug",y="Total Service performed"),tooltip = "all")

#Avg. services perfermoed per patient
BOTH<- weighted.mean(dtr$avg_services_per_patient[dtr$group == "BOTH"],dtr$number_of_patients[dtr$group == "BOTH"])
COPAXONE<- weighted.mean(dtr$avg_services_per_patient[dtr$group == "COPAXONE"],dtr$number_of_patients[dtr$group == "COPAXONE"])
GILENYA<-weighted.mean(dtr$avg_services_per_patient[dtr$group== "GILENYA"],dtr$number_of_patients[dtr$group== "GILENYA"])
dtr_avg_service <- data.frame(BOTH,COPAXONE,GILENYA)
dtr_avg_service <- melt(dtr_avg_service)
ggplotly(ggplot(dtr_avg_service,aes(x=dtr_avg_service$variable,y=dtr_avg_service$value,fill = variable)) + geom_bar(stat = "identity")+ labs(title = "Weighted Avg Service performed / Patient",x="Brand Drug",y="Weighted Avg Service/ Patient"),tooltip = "all")
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$avg_services_per_patient,fill = group))+ geom_boxplot() + labs(title = "Avg Service performed / Patient",x="Brand Drug",y="Avg Service/ Patient"),tooltip = "all")


#Avg. amount allowed per patient
BOTH<- weighted.mean(dtr$avg_allowed_amt_per_patient[dtr$group[unique(dtr$doc_id)] == "BOTH"],dtr$number_of_patients[dtr$group[unique(dtr$doc_id)] == "BOTH"])
COPAXONE<- weighted.mean(dtr$avg_allowed_amt_per_patient[dtr$group == "COPAXONE"],dtr$number_of_patients[dtr$group == "COPAXONE"])
GILENYA<-weighted.mean(dtr$avg_allowed_amt_per_patient[dtr$group== "GILENYA"],dtr$number_of_patients[dtr$group== "GILENYA"])
dtr_avg_amt <- data.frame(BOTH,COPAXONE,GILENYA)
dtr_avg_amt <- melt(dtr_avg_amt)
ggplotly(ggplot(dtr_avg_amt,aes(x=dtr_avg_amt$variable,y=dtr_avg_amt$value,fill=variable)) + geom_bar(stat = "identity")+ labs(title = "Weighted Avg Medicare Amt Allowed/ Patient",x="Brand Drug",y="Weighted Avg Medicare Amt Allowed/ Patient"),tooltip = "all")
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$avg_allowed_amt_per_patient,fill = group))+ geom_boxplot() + labs(title = "Avg Amt Allowed performed / Patient",x="Brand Drug",y="Avg Service/ Patient"),tooltip = "all")

#Service types
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$diagnostic,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Diagonstic Service performed",x="Brand Drug",y="Total Service performed"),tooltip = "all")
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$therapeutic,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Therapeutic Service performed",x="Brand Drug",y="Total Service performed"),tooltip = "all")
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$other,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Other Service performed",x="Brand Drug",y="Total Service performed"),tooltip = "all")

DIAGNOSTIC <- mean(dtr$diagnostic_services_percentage)
THERAPEUTIC <- mean(dtr$therapeutic_services_percentage)
OTHER <- mean(dtr$other_services_percentage)
ser_percentage <- data.frame(THERAPEUTIC,DIAGNOSTIC,OTHER)
ser_percentage <- melt(ser_percentage)

ggplotly(ggplot(ser_percentage,aes(x=ser_percentage$variable,y=ser_percentage$value,fill = variable)) + geom_bar(stat = "identity")+ labs(title = "Overall Services Type Percentage",x="Brand Drug",y="Percentage of Service Performed"),tooltip = "all")


#Avg. HCC Risk Score of beneficiary across groups
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$average.hcc.risk.score.of.beneficiaries,fill = group))+ geom_boxplot() + labs(title = "Overall Avg HCC Risk Score",x="Brand Drug",y="Avg HCC Risk Score"),tooltip = "all")

#Overall Beneficary count
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_bene_count,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Beneficary count",x="Brand Drug",y="Total Bene Count"),tooltip = "all")

#Overall Total Claim Count
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_total_claim_count,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Claim Count",x="Brand Drug",y="Total Claim Count"),tooltip = "all")

#Overall Total Day Supply
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_total_day_supply,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Day Supply",x="Brand Drug",y="Total Day Supply"),tooltip = "all")

#Overall Total Drug Cost
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_total_drug_cost,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Drug Cost",x="Brand Drug",y="Total Drug Cost"),tooltip = "all")

#Overall Bene Count Age Above 65
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_bene_count_ge65,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Bene Count Age Above 65",x="Brand Drug",y="Bene Count"),tooltip = "all")

#Overall Total Claim Count Age  Above 65
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_total_claim_count_ge65,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Claim Count Age Above 65",x="Brand Drug",y="Total Claim Count"),tooltip = "all")

#Overall Total Day Supply Age Above 65
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_total_day_supply_ge65,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Day Supply Age Above 65",x="Brand Drug",y="Total Day Supply"),tooltip = "all")

#Overall Total Drug Cost Age Above 65
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_total_drug_cost_ge65,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Total Drug Cost Age Above 65",x="Brand Drug",y="Total Drug Cost"),tooltip = "all")

#Overall Brand Drug Claim Count
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_brand_claim_count,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Brand Drug Claim Count",x="Brand Drug",y="Claim Count"),tooltip = "all")

#Overall Generic Drug Claim Count
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_generic_claim_count,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Generic Drug Claim Count",x="Brand Drug",y="Claim Count"),tooltip = "all")

#Overall Other Claim Count
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_other_claim_count,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Other Claim Count",x="Brand Drug",y="Claim Count"),tooltip = "all")

#Overall Brand Drug Cost 
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_brand_drug_cost,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Brand Drug Cost",x="Brand Drug",y="Total Cost"),tooltip = "all")

#Overall Generic Drug Cost
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_bene_count_ge65,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overall Generic Drug Cost",x="Brand Drug",y="Total Cost"),tooltip = "all")

#Overll Other Drug Cost
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$overall_other_drug_cost,fill = group))+ geom_bar(stat ="identity") + labs(title = "Overll Other Drug Cost",x="Brand Drug",y="Total Cost"),tooltip = "all")

#Brand Drug cost percentage
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$branded_drugs_cost_percentage,fill = group))+ geom_bar(stat ="summary", fun.y = "mean") + labs(title = "Overall Brand Drug Cost Percentage",x="Brand Drug",y="Cost Percentage"),tooltip = "all")

#Generic Drug Cost Percentage
ggplotly(ggplot(dtr,aes(x=dtr$group, y=dtr$generic_drugs_cost_percentage,fill = group))+ geom_bar(stat ="summary", fun.y = "mean") + labs(title = "Overall Generic Drug Cost Percentage",x="Brand Drug",y="Cost Percentage"),tooltip = "all")

#Correlation 
summary(dtr)
dtc<- dtg[,-c(1:5,10,41)]
corrplot(cor(dtc),method = "circle")
