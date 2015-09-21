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


# 1. Chronic Condition: Alzheimer or related disorders or senile
tb1 = table(subBeneficiary$SP_ALZHDMTA, subBeneficiary$Birth_month)

# 2. Chronic Condition: Chronic Condition: Heart Failure
tb2 = table(subBeneficiary$SP_CHF, subBeneficiary$Birth_month)

# 3. Chronic Condition: Chronic Kidney Disease
tb3 = table(subBeneficiary$SP_CHRNKIDN, subBeneficiary$Birth_month)

# 4. Chronic Condition: Cancer
tb4 = table(subBeneficiary$SP_CNCR, subBeneficiary$Birth_month)

# 5. Chronic Condition: Chronic Obstructive Pulmonary Disease
tb5 = table(subBeneficiary$SP_COPD, subBeneficiary$Birth_month)

# 6. Chronic Condition: Chronic Condition: Depression
tb6 = table(subBeneficiary$SP_DEPRESSN, subBeneficiary$Birth_month)

# 7. Chronic Condition: Chronic Condition: Diabetes
tb7 = table(subBeneficiary$SP_DIABETES, subBeneficiary$Birth_month)

# 8. Chronic Condition: Chronic Condition:  Ischemic Heart Disease
tb8 = table(subBeneficiary$SP_ISCHMCHT, subBeneficiary$Birth_month)

# 9. Chronic Condition: Chronic Condition: Osteoporosis
tb9 = table(subBeneficiary$SP_OSTEOPRS, subBeneficiary$Birth_month)

# 10. Chronic Condition: Chronic Condition: rheumatoid arthritis and osteoarthritis (RA/OA)
tb10 = table(subBeneficiary$SP_RA_OA, subBeneficiary$Birth_month)

# 11. Chronic Condition: Chronic Condition: Chronic Condition: Stroke/transient Ischemic Attack
tb11 = table(subBeneficiary$SP_STRKETIA, subBeneficiary$Birth_month)

# tblist contains all chronic condition tables 
tblist = list(tb1, tb2, tb3, tb4, tb5, tb6, tb7, tb8, tb9, tb10, tb11)

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
    geom_text(aes(label=percent(population)),hjust=-0.3, vjust=0.3, size =4) +
    xlab("disease stage") +
    ylab("population percentage") +
    ggtitle("prevalence of different stages of CKD in the 2008 population")

# We regarded the people who were recorded as being in more than one stage in the same year as multiple samples
# Because this plotting is creted to show the prevalence of different stages, so we should focus more on stages.
# Even though some people may be recorded in more than one stage, they are representing samples in different stages. 
