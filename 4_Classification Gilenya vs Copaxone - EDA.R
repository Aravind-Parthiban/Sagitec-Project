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

#Load Dataset
dt<- read.csv("GILENYA_COPAXONE_conso1.csv")

#Count of Drug Prescribed
ggplotly(ggplot(dt,aes(dt$brand_name,fill = brand_name)) + geom_bar(stat = "count") + labs(title = "Brand Drug Prescribed",x="Brand Drug",y="Count"),tooltip = "all")

# function to convert abrivatted state name into full state name
stateFromLower <-function(x) {
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

dt$state<-stateFromLower(dt$nppes_provider_state)

#State wise Prescribed 
ggplot(dt,aes(reorder(dt$state,dt$doc_id,length),fill=brand_name))+ geom_bar(stat = "count") + labs(title = "Brand Drug Prescribed",x="state",y="Count")

#Brand Drug Beneficiary count
ggplot(dt,aes(x=dt$brand_name, y=dt$bene_count,fill = brand_name))+ geom_bar(stat ="identity") + labs(title = "Brand Drug Bene count",x="Brand Drug",y="Total Bene Count")

#Total Claim Count
ggplot(dt,aes(x=dt$brand_name, y=dt$total_claim_count,fill = brand_name))+ geom_bar(stat ="identity") + labs(title = "Brand Drug Total Claim Count",x="Brand Drug",y="Total Claim Count")

#Total Day Supply
ggplot(dt,aes(x=dt$brand_name, y=dt$total_day_supply,fill = brand_name))+ geom_bar(stat ="identity") + labs(title = "Brand Drug Total Day Supply",x="Brand Drug",y="Total Day Supply Count")

#Total Drug Cost
ggplot(dt,aes(x=dt$brand_name, y=dt$total_drug_cost,fill = brand_name))+ geom_bar(stat ="identity") + labs(title = "Total Drug Cost",x="Brand Drug",y="Total Drug Cost")

#Median cost per Drug
ggplotly(ggplot(dt,aes(x=dt$brand_name,y=dt$cos_per_drug,colour= brand_name)) + geom_boxplot() +labs(title = "Median Cost per Drug",x="Brand Drug",y="Cost Per Drug"),tooltip = "all")

#State-wise Median cost per Drug 
#Refer Plots Folder  

#Create Groups based on drug prescription pattern  
#646 non-unique docs mean they prescribe both drugs
dim(dt[duplicated(dt$doc_id),])[1]
rep_doc_id<- dt[duplicated(dt$doc_id),1]
length(rep_doc_id)

#Docs who prescribes both GILENYA & COPAXONE
dt$group[dt$doc_id %in% rep_doc_id] <- "BOTH"
colSums(is.na(dt))

#Docs who only prescribes COPAXONE 
#dt$group[(is.na(dt$group)) & (dt$group[dt$brand_name[is.na(dt$group)] == "COPAXONE"])]

#Docs who only prescribes GILENYA
#dt$group[(is.na(dt$group)) & (dt$group[dt$brand_name[is.na(dt$group)] == "GILENYA"])]

#write.csv(dt,"GILENYA_COPAXONE_conso_grp.csv",row.names = F)

