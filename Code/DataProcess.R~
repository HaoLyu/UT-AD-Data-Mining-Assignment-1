# Read csv file 
Beneficiary2008 = read.csv2("./Data/DE1_0_2008_Beneficiary_Summary_File_Sample_1.csv",sep=",",dec =",")
Beneficiary2009 = read.csv2("./Data/DE1_0_2009_Beneficiary_Summary_File_Sample_1.csv",sep=",",dec =",")
Beneficiary2010 = read.csv2("./Data/DE1_0_2010_Beneficiary_Summary_File_Sample_1.csv",sep=",",dec =",")
Outpatient = read.csv2("./Data/DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.csv",sep=",",dec =",")
Inpatient = read.csv2("./Data/DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv",sep=",",dec =",")
PDE = read.csv2("./Data/DE1_0_2008_to_2010_Prescription_Drug_Events_Sample_1.csv",sep=",",dec =",")
Carrier1 = read.csv2("./Data/DE1_0_2008_to_2010_Carrier_Claims_Sample_1A.csv",sep=",",dec =",")
Carrier2 = read.csv2("./Data/DE1_0_2008_to_2010_Carrier_Claims_Sample_1B.csv",sep=",",dec =",")

# 1.a 
# For each of the 11 chronic conditions, use the chi-squared test to determine
# whether or not there is a significant relationship between it and the birth month.

# Select the subset of population who are from mid-west:
# Illinois, Indiana, Iowa, Kansas, Michigan, Minnesota, Missouri, Nebraska, North Dakota, Ohio, South Dakota, and Wisconsin.
# Abbreviation number: 14, 15, 16, 17, 23, 24, 26, 28, 35, 36, 43, 52
midwest = c(14, 15, 16, 17, 23, 24, 26, 28, 35, 36, 43, 52)
subBeneficiary2008 = subset(Beneficiary2008, SP_STATE_CODE %in% midwest)
subBeneficiary2009 = subset(Beneficiary2009, SP_STATE_CODE %in% midwest)
subBeneficiary2010 = subset(Beneficiary2010, SP_STATE_CODE %in% midwest)

# Add a new variable Birth_month 
month2008 = c(floor(((subBeneficiary2008$BENE_BIRTH_DT)%%10000)/100))
subBeneficiary2008$Birth_month  <- month2008

month2009 = c(floor(((subBeneficiary2009$BENE_BIRTH_DT)%%10000)/100))
subBeneficiary2009$Birth_month  <- month2009

month2010 = c(floor(((subBeneficiary2010$BENE_BIRTH_DT)%%10000)/100))
subBeneficiary2010$Birth_month  <- month2010

# Merge 2008, 2009 and 2010 subsets
subBeneficiary = rbind(subBeneficiary2008, subBeneficiary2009, subBeneficiary2010)

# Chi-squared Test of Independence on 11 chronic conditions(at 0.05 significance level)
tblist1 = c()
for(i in 1:11){
  tblist1 = c(tblist, table(subBeneficiary[,12+i], subBeneficiary[,33]))
}

# plist contains all p values of 11 chronic conditions
plist = NULL
for (i in 1:11){
  plist = c(plist, chisq.test(tblist[[i]])[3])
}

# Print the significant relationship between birth month and each of chronic conditions
for (i in 1:11){
  if(plist[i] > 0.05){
    cat("Chronic Condition", i,"does not have significant relationship with birth month\n")  
  }
  else{
    cat("Chronic Condition", i,"has significant relationship with birth month\n")  
  }
}

# Summury: In 11 Chronic Conditions, condition 3,4,5,7,8,9,10 has a significant relationship with birth month


#1-b. 
# adjust_plist is the new p values list after adjusting p values by Benjamini & Yekutieli (2001)
adjust_plist = p.adjust(plist, method="BY", n=length(plist))

# Print the significant relationship between birth month and each of chronic conditions after adjusting p values
for (i in 1:11){
  if(adjust_plist[i] > 0.05){
    cat("Chronic Condition", i,"does not have significant relationship with birth month\n")  
  }
  else{
    cat("Chronic Condition", i,"has significant relationship with birth month\n")  
  }
} 

# Summury: After the adjustment of p values, ondition 7, 8 has a significant relationship with birth month



##############################CUT-OFF##############################

# 2.a 
# Copy a new list a2Inpatient/a2Outpatient from Inpatient and Outpatient and characterize some variables
a2Inpatient = Inpatient
a2Inpatient[,c(21:30)] <- sapply(a2Inpatient[,c(21:30)], as.character) 
a2Outpatient = Outpatient
a2Outpatient[,c(13:22)] <- sapply(a2Outpatient[,c(13:22)], as.character) 

# a2Inpatient/Outpatient should only contain data in 2008
a2Inpatient = subset(a2Inpatient, a2Inpatient$CLM_ADMSN_DT<20090000)
a2Outpatient = subset(a2Outpatient, a2Outpatient$CLM_FROM_DT<20090000)

# Create a ICD-9 Code set to collect kidney disease codes
ICD = c("5851", "5852", "5853", "5854", "5855", "5856", "5859")

# Transfer codes in ICD9_DGNS_CD_1, ICD9_DGNS_CD_2, ..., ICD9_DGNS_CD_10 to simple interger in a2Inpatient and a2Outpatient
# 0 means not related to chronic kidney disease, 1 measn 5851, 2 means 5852, ..., 9 means 5859

for (i in 1:length(a2Inpatient$DESYNPUF_ID)){ 
  for(j in c(21:30)){    
    if(a2Inpatient[,j][i] %in% ICD){
      a2Inpatient[,j][i] = as.integer(a2Inpatient[,j][i])%%10
      #cat("in i row :", i, " j column", j, " New Admiting ICD-9 : ", a2Inpatient[,j][i], "\n" )
    }
    else {
      a2Inpatient[,j][i] = 0
    }  
  }
}

for (i in 1:length(a2Outpatient$DESYNPUF_ID)){ 
  for(j in c(13:22)){  
    if(a2Outpatient[,j][i] %in% ICD){
      a2Outpatient[,j][i] = as.integer(a2Outpatient[,j][i])%%10
      #cat("in i row :", i, " j column", j, " New Admiting ICD-9 : ", a2Outpatient[,j][i], "\n" )
    }
    else {
      a2Outpatient[,j][i] = 0
    }   
  }
}

# Make a statistics of kidney disease in a2Inpatient and a2Outpatient
# statkidney is a list containing the count of kidney patients and non-kidney patients

statkidney = list(rep(0, 10))

# Count the number of patients in stage 1,2, ..., 9 (7 and 8 are meaningless)
for (i in 1:length(a2Inpatient$DESYNPUF_ID)){
  for(j in c(21:30)){
    if(a2Inpatient[,j][i] %in% c(1, 2, 3, 4, 5, 6, 9)){
      count = as.integer(a2Inpatient[,j][i]) 
      statkidney[[1]][count+1] = statkidney[[1]][count+1] + 1
    }
  }
}

for (i in 1:length(a2Outpatient$DESYNPUF_ID)){
  for(j in c(13:22)){
    if(a2Outpatient[,j][i] %in% c(1, 2, 3, 4, 5, 6, 9)){
      count = as.integer(a2Outpatient[,j][i]) 
      statkidney[[1]][count+1] = statkidney[[1]][count+1] + 1
    }
  }
}

# Count the number of patients in stage 0 (people with no CKD)

for (i in 1:length(a2Inpatient$DESYNPUF_ID)){
  nonkidney = 0  
  for(j in c(21:30)){
    if(a2Inpatient[,j][i] %in% c(1, 2, 3, 4, 5, 6, 9)){
      nonkidney = 1
      break
    } 
  }     
  if(nonkidney == 0){
    statkidney[[1]][1] = statkidney[[1]][1] + 1
  }
}

for (i in 1:length(a2Outpatient$DESYNPUF_ID)){
  nonkidney = 0 
  for(j in c(21:30)){
    if(a2Outpatient[,j][i] %in% c(1, 2, 3, 4, 5, 6, 9)){
      nonkidney = 1
      break
    } 
  } 
  if(nonkidney == 0){
    statkidney[[1]][1] = statkidney[[1]][1] + 1
  }
}

# Remove the blank position in statkidney
statkidney = statkidney[[1]][-8:-9]

# Convert the population count number to population percentage 
statkidney = statkidney/(length(a2Inpatient$DESYNPUF_ID) + length(a2Outpatient$DESYNPUF_ID))

# Create a data frame plotdata to plot
namekidney = c("stage 0", "stage 1", "stage 2", "stage 3", "stage 4", "stage 5", "stage 6", "stage 9")
plotdata  = data.frame(statkidney, namekidney)
names(plotdata) = c("population", "diseasestage")

# Plotting 
library(scales)
library(ggplot2)

# The percent function helps get percentage form of the number
percent <- function(x, digits = 3, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

g = ggplot(plotdata, aes(y = plotdata$population, x = plotdata$diseasestage)) + 
    geom_point()+scale_y_continuous(labels=percent) +
    geom_text(aes(label=percent(population)),hjust=-0.2, vjust=0, size =2.3) +
    xlab("disease stage") +
    ylab("population percentage") +
    ggtitle("prevalence of different stages of CKD in the 2008 population")
g
# We regarded the people who were recorded as being in more than one stage in the same year as multiple samples
# This plotting is creted to show the prevalence of different stages, so we should focus more on stages.

# 2.b
# In this question, we need to get the population percentage of CKD in 2009. 
# Copy a new list a2Inpatient/a2Outpatient from Inpatient and Outpatient and characterize some variables
a2Inpatient = Inpatient
a2Inpatient[,c(21:30)] <- sapply(a2Inpatient[,c(21:30)], as.character) 
a2Outpatient = Outpatient
a2Outpatient[,c(13:22)] <- sapply(a2Outpatient[,c(13:22)], as.character) 

# a2Inpatient/Outpatient should only contain data in 2009
a2Inpatient = subset(subset(a2Inpatient, a2Inpatient$CLM_ADMSN_DT>20090000),a2Inpatient$CLM_ADMSN_DT<20100000)
a2Outpatient = subset(subset(a2Outpatient, a2Outpatient$CLM_ADMSN_DT>20090000),a2Outpatient$CLM_ADMSN_DT<20100000)

# Transfer codes in ICD9_DGNS_CD_1, ICD9_DGNS_CD_2, ..., ICD9_DGNS_CD_10 to simple interger in a2Inpatient and a2Outpatient
# 0 means not related to chronic kidney disease, 1 measn 5851, 2 means 5852, ..., 9 means 5859

for (i in 1:length(a2Inpatient$DESYNPUF_ID)){ 
  for(j in c(21:30)){    
    if(a2Inpatient[,j][i] %in% ICD){
      a2Inpatient[,j][i] = as.integer(a2Inpatient[,j][i])%%10
      cat("in i row :", i, " j column", j, " New Admiting ICD-9 : ", a2Inpatient[,j][i], "\n" )
    }
    else {
      a2Inpatient[,j][i] = 0
    }  
  }
}

for (i in 1:length(a2Outpatient$DESYNPUF_ID)){ 
  for(j in c(13:22)){  
    if(a2Outpatient[,j][i] %in% ICD){
      a2Outpatient[,j][i] = as.integer(a2Outpatient[,j][i])%%10
      cat("in i row :", i, " j column", j, " New Admiting ICD-9 : ", a2Outpatient[,j][i], "\n" )
    }
    else {
      a2Outpatient[,j][i] = 0
    }   
  }
}

# Make a statistics of kidney disease in a2Inpatient and a2Outpatient
# statkidney is a list containing the count of kidney patients and non-kidney patients

statkidney = list(rep(0, 10))

# Count the number of patients in stage 1,2, ..., 9
for (i in 1:length(a2Inpatient$DESYNPUF_ID)){
  for(j in c(21:30)){
    if(a2Inpatient[,j][i] %in% c(1, 2, 3, 4, 5, 6, 9)){
      count = as.integer(a2Inpatient[,j][i]) 
      statkidney[[1]][count+1] = statkidney[[1]][count+1] + 1
    }
  }
}

for (i in 1:length(a2Outpatient$DESYNPUF_ID)){
  for(j in c(13:22)){
    if(a2Outpatient[,j][i] %in% c(1, 2, 3, 4, 5, 6, 9)){
      count = as.integer(a2Outpatient[,j][i]) 
      statkidney[[1]][count+1] = statkidney[[1]][count+1] + 1
    }
  }
}

# Count the number of patients in stage 0 (people with no CKD)

for (i in 1:length(a2Inpatient$DESYNPUF_ID)){
  nonkidney = 0  
  for(j in c(21:30)){
    if(a2Inpatient[,j][i] %in% c(1, 2, 3, 4, 5, 6, 9)){
      nonkidney = 1
      break
    } 
  }   
  if(nonkidney == 0){
    statkidney[[1]][1] = statkidney[[1]][1] + 1
  }
}

for (i in 1:length(a2Outpatient$DESYNPUF_ID)){
  nonkidney = 0 
  for(j in c(21:30)){
    if(a2Outpatient[,j][i] %in% c(1, 2, 3, 4, 5, 6, 9)){
      nonkidney = 1
      break
    } 
  } 
  if(nonkidney == 0){
    statkidney[[1]][1] = statkidney[[1]][1] + 1
  }
}

# Remove the blank position in statkidney
statkidney = statkidney[[1]][-8:-9]

# Convert the population count number to population percentage 
statkidney = statkidney/(length(a2Inpatient$DESYNPUF_ID) + length(a2Outpatient$DESYNPUF_ID))

# Create a data frame plotdata2009 to plot
namekidney = c("stage 0", "stage 1", "stage 2", "stage 3", "stage 4", "stage 5", "stage 6", "stage 9")
plotdata2009  = data.frame(statkidney, namekidney)
names(plotdata2009) = c("population", "diseasestage")

# Plot the population percentage indicating transitions between stages from 2008 to 2009
plotdata$year = "2008"
plotdata2009$year = "2009"
plot2b = rbind(plotdata, plotdata2009)
ggplot(plot2b, aes(x = diseasestage, y = population)) +  geom_line(aes(group=year, colour=year)) + 
       stat_smooth(se=FALSE, span=0.5)  

# Create the table of 2008 and 2009 population of CKD
plot2b = cbind(plotdata$population, plotdata2009$population)
table2b = as.matrix(plot2b)
colnames(table2b) = c("2008","2009")
rownames(table2b) = plotdata$diseasestage
table2b <- as.table(table2b)
table2b


##############################CUT-OFF##############################
# 3.b
# Copy Beneficiary2008 and Beneficiary2009
Beneficiary2008_3 = Beneficiary2008
Beneficiary2009_3 = Beneficiary2009

# Characterize variables which are related to reimbursement
Beneficiary2008_3[,c(23:32)] = lapply(lapply(Beneficiary2008_3[,c(23:32)], as.character), as.integer)
Beneficiary2009_3[,c(23:32)] = lapply(lapply(Beneficiary2009_3[,c(23:32)], as.character), as.integer)

# Add a new variable reimbursement_amount into Beneficiary2008_3 and Beneficiary2009_3
Beneficiary2008_3$reimbursement_amount = Beneficiary2008_3$MEDREIMB_IP + Beneficiary2008_3$BENRES_IP + Beneficiary2008_3$PPPYMT_IP + 
                                         Beneficiary2008_3$MEDREIMB_OP + Beneficiary2008_3$BENRES_OP + Beneficiary2008_3$PPPYMT_OP +
                                         Beneficiary2008_3$MEDREIMB_CAR + Beneficiary2008_3$BENRES_CAR + Beneficiary2008_3$PPPYMT_CAR



Beneficiary2009_3$reimbursement_amount = Beneficiary2009_3$MEDREIMB_IP + Beneficiary2009_3$BENRES_IP + Beneficiary2009_3$PPPYMT_IP + 
                                         Beneficiary2009_3$MEDREIMB_OP + Beneficiary2009_3$BENRES_OP + Beneficiary2009_3$PPPYMT_OP +
                                         Beneficiary2009_3$MEDREIMB_CAR + Beneficiary2009_3$BENRES_CAR + Beneficiary2009_3$PPPYMT_CAR

# Distribution of reimbursement in 2008 and 2009
library(ggplot2)
Beneficiary2008_3$year = "2008"
Beneficiary2009_3$year = "2009"
Beneficiary0809_3 <- rbind(Beneficiary2008_3, Beneficiary2009_3)
ggplot(Beneficiary0809_3, aes(reimbursement_amount, fill = year )) + geom_histogram(alpha = 1) + 
  scale_x_continuous(limits = c(0, 100000))

# Randomly pick 1000 samples from 2008 and 2009 data. 
Beneficiary2008_3_sample = Beneficiary2008_3[sample(nrow(Beneficiary2008_3),1000),]
Beneficiary2009_3_sample = Beneficiary2009_3[sample(nrow(Beneficiary2009_3),1000),]
Beneficiary0809_3_sample <- rbind(Beneficiary2008_3_sample, Beneficiary2009_3_sample)

# Caluculate the mean of each group
library(plyr)
mu <- ddply(Beneficiary0809_3_sample, "year", summarise, grp.mean=mean(reimbursement_amount))
head(mu)

# Fit the mean line and plot it.
ggplot(Beneficiary0809_3_sample, aes(reimbursement_amount, fill = year )) + geom_histogram(alpha = 1) + 
       scale_x_continuous(limits = c(0, 30000)) + 
       geom_vline(data=mu, aes(xintercept=grp.mean, color=year),linetype="dashed")


# 3.d
# Ultilize beneficiary data in  2009
# Convert some variables and add a new variable reimbursement_class


for(i in 1:length(Beneficiary2009_3$DESYNPUF_ID)){
  if(Beneficiary2009_3$reimbursement_amount[i]<5000){
    Beneficiary2009_3$reimbursement_class[i] = "low"
  }else if(Beneficiary2009_3$reimbursement_amount[i]<15000){
    Beneficiary2009_3$reimbursement_class[i] = "medium"
  }else if(Beneficiary2009_3$reimbursement_amount[i]<25000){
    Beneficiary2009_3$reimbursement_class[i] = "medium high"
  }else {
    Beneficiary2009_3$reimbursement_class[i] = "high"
  }
}
#

# The getdate function helps get the date of a numeric 
getdate <- function(x) {
  date = as.integer(x)
  year = as.character(floor(date/10000))
  day = as.character(date%%100)
  month = as.character(floor((date%%10000)/100))
  date = paste(year, month, day, sep="-")
}

# Create an age variable from birth date and death date 
for(i in 1:length(Beneficiary2009_3$DESYNPUF_ID)){  
  if( is.na(Beneficiary2009_3$BENE_DEATH_DT[i])){
    Beneficiary2009_3$BENE_DEATH_DT[i] = paste(Beneficiary2009_3$year[i], "1231", sep="")
  }
}  

for(i in 1:length(Beneficiary2009_3$DESYNPUF_ID)){
  Beneficiary2009_3$age[i] = floor(as.numeric(as.Date(getdate(Beneficiary2009_3$BENE_DEATH_DT[i])) - as.Date(getdate(Beneficiary2009_3$BENE_BIRTH_DT[i])))/365)  
}

# Ultilize age/gender/state/ and all the chronic disease flags as the features in the cluster model.
# Create a new set clusterBeneficiary0809_3
clusterBeneficiary2009_3 = Beneficiary2009_3[,c(4,5,7,13:23,36)]

# Conver the SP_STATE_CODE into area types, like midwest, west, north east, south
midwest = c(14, 15, 16, 17, 23, 24, 26, 28, 35, 36, 43, 52)
west = c(3, 5, 6, 13, 27, 29, 32, 38, 46, 50, 53)
northeast = c(7, 20, 22, 30, 31, 33, 39, 41, 47)
south = c(1, 4, 8, 10, 11, 18, 19, 21, 25, 34, 37, 42, 44, 45, 49, 51)
for(i in 1:length(clusterBeneficiary2009_3$BENE_SEX_IDENT_CD)){
  if(clusterBeneficiary2009_3$SP_STATE_CODE[i] %in% midwest){
    clusterBeneficiary2009_3$SP_STATE_CODE[i] = 1
  }else if(clusterBeneficiary2009_3$SP_STATE_CODE[i] %in% west){
    clusterBeneficiary2009_3$SP_STATE_CODE[i] = 2
  }else if(clusterBeneficiary2009_3$SP_STATE_CODE[i] %in% northeast){
    clusterBeneficiary2009_3$SP_STATE_CODE[i] = 3
  }else if(clusterBeneficiary2009_3$SP_STATE_CODE[i] %in% south){
    clusterBeneficiary2009_3$SP_STATE_CODE[i] = 4
  }else{
    clusterBeneficiary2009_3$SP_STATE_CODE[i] = 5
  }
}

# K-means cluster to clusterBeneficiary0809_3, we create 4 clusters
kc <- kmeans(clusterBeneficiary2009_3, 4)
table(Beneficiary2009_3$reimbursement_class, kc$cluster)

# Plot the clusters and their centres.
plot(clusterBeneficiary2009_3[c("age", "SP_STATE_CODE")], col=kc$cluster)

# Build model to predict reimbursement amounts using clusters
clusterBeneficiary2009_3$cluster = kc$cluster
clusterBeneficiary2009_3$reimbursement = log(Beneficiary2009_3$reimbursement_amount+1)

# Divide the data in 2009 into 4 clusters
cluster1Beneficiary2009_3 = subset(clusterBeneficiary2009_3, cluster == 1)
cluster2Beneficiary2009_3 = subset(clusterBeneficiary2009_3, cluster == 2)
cluster3Beneficiary2009_3 = subset(clusterBeneficiary2009_3, cluster == 3)
cluster4Beneficiary2009_3 = subset(clusterBeneficiary2009_3, cluster == 4)

cluster1Beneficiary2009_3$cluster = NULL 
cluster2Beneficiary2009_3$cluster = NULL 
cluster3Beneficiary2009_3$cluster = NULL 
cluster4Beneficiary2009_3$cluster = NULL 

# Build SVM model and evaluate the prediction by SME
# Divide cluster 1 into a 70/30 train/test split
# Function MSE calculatet the MSE on log error
RMSE <- function(error)
{
  sqrt(mean(error^2))
}

# GetClusterRMSE get the RMSE of prediction of dataset with svm
GetClusterRMSE = function(dataset){
  trainrows = sample(nrow(dataset),floor(0.7*length(dataset[,1])))
  cluster_train = dataset[trainrows,]
  cluster_test = dataset[-trainrows,]
  
  # Build the svm model
  library(e1071)
  model = svm(cluster_train$reimbursement~., data = cluster_train)
  predicted <- predict(model, cluster_test)
  
  # Evaluate the model with log2(MSE)
  error = cluster_test$reimbursement - predicted
  RMSEcluster = RMSE(error)
  
  return(RMSEcluster)
}


# Get the whole RMSEclusterError
RMSEclusterError = GetClusterRMSE(cluster1Beneficiary2009_3) + GetClusterRMSE(cluster2Beneficiary2009_3) +
                   GetClusterRMSE(cluster3Beneficiary2009_3) + GetClusterRMSE(cluster4Beneficiary2009_3)

cat(RMSEclusterError)