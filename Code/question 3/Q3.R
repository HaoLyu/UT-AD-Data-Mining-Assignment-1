# Qestion 3
# Read csv file 
Beneficiary2008 = read.csv2("./Data/DE1_0_2008_Beneficiary_Summary_File_Sample_1.csv",sep=",",dec =",")
Beneficiary2009 = read.csv2("./Data/DE1_0_2009_Beneficiary_Summary_File_Sample_1.csv",sep=",",dec =",")
Beneficiary2010 = read.csv2("./Data/DE1_0_2010_Beneficiary_Summary_File_Sample_1.csv",sep=",",dec =",")
summary <- read.csv("DE1_0_2009_Beneficiary_Summary_File_Sample_1.csv")

# 3.a
library(glmnet)

reimb <- summary$MEDREIMB_IP + summary$BENRES_IP + summary$PPPYMT_IP + summary$MEDREIMB_OP + 
  summary$BENRES_OP + summary$PPPYMT_OP + summary$MEDREIMB_CAR + summary$BENRES_CAR + 
  summary$PPPYMT_CAR
reimb[reimb < 0 ] <- 0
log_reimb <- log(reimb + 1)
hist(log_reimb, main="Log Reimbursments in 2008")


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

# 3.c

cond <- summary[,c("BENE_ESRD_IND", "SP_ALZHDMTA", "SP_CHF", "SP_CHRNKIDN", "SP_CNCR", "SP_COPD",
                   "SP_DEPRESSN", "SP_DIABETES", "SP_ISCHMCHT", "SP_OSTEOPRS", "SP_RA_OA",
                   "SP_STRKETIA")]
cond$BENE_ESRD_IND <- ifelse(cond$BENE_ESRD_IND == "Y", 1, 2)
cond <- -1 * cond + 2
age <- 2009 - summary$BENE_BIRTH_DT %/% 10000
gender <- -1 * summary$BENE_SEX_IDENT_CD + 2
expensivestate <- ifelse(summary$SP_STATE_CODE %in% c(47, 41, 22, 38, 31, 5, 2, 33, 7, 12),  1, 0)
data = cbind(age, gender, expensivestate, cond)
sub <- sample(nrow(data), floor(nrow(data) * 0.7))
Xtrain <- data[sub,]
Xtest <- data[-sub,]
Ytrain <- log_reimb[sub]
Ytest <- log_reimb[-sub]

input <- cbind(Xtrain, Ytrain)
linear_model <- lm(Ytrain ~ age + gender + expensivestate + BENE_ESRD_IND + SP_ALZHDMTA + SP_CHF + 
                     SP_CHRNKIDN + SP_CNCR + SP_COPD + SP_DEPRESSN + SP_DIABETES + SP_ISCHMCHT +
                     SP_OSTEOPRS + SP_RA_OA + SP_STRKETIA, data=input)
lm_results <- predict(linear_model, newdata=Xtest)
lm_rmse <- sqrt(mean((lm_results - Ytest)^2))

ridge_model <- glmnet(as.matrix(Xtrain), as.matrix(Ytrain))
min_lambda <- ridge_model$lambda.min
ridge_results <- predict(ridge_model, as.matrix(Xtest), s=min_lambda)
ridge_rmse <- sqrt(mean((ridge_results - Ytest)^2))

baseline_rmse <- sqrt(mean((mean(log_reimb) - log_reimb)^2))


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

# K-means cluster to clusterBeneficiary2009_3, we create k clusters and calculate the RMSE of each clusters
KMcluster = function(x){
  kc = kmeans(clusterBeneficiary2009_3, x)
  #table(Beneficiary2009_3$reimbursement_class, kc$cluster)
  # Plot the clusters and their centres.
  #plot(clusterBeneficiary2009_3[c("age", "SP_STATE_CODE")], col=kc$cluster)
  # Build model to predict reimbursement amounts using clusters
  clusterBeneficiary2009_3$cluster = kc$cluster
  clusterBeneficiary2009_3$reimbursement = log(Beneficiary2009_3$reimbursement_amount+1)
  
  # Divide the data in Beneficiary2009 into k clusters
  Clusterlist = list()
  for(i in 1:x){
    kcluster = subset(clusterBeneficiary2009_3, cluster == i)
    kcluster$cluster = NULL
    ClusterRMSE = GetClusterRMSE(kcluster)
    Clusterlist = c(Clusterlist, ClusterRMSE)
  }
  return(Clusterlist)
}
KMcluster(5)

# Get the whole RMSEclusterError
RMSEclusterError = sum(KMcluster(5))
cat(RMSEclusterError)