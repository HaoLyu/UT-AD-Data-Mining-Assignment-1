# Qestion 2
# Read csv file 
Beneficiary2008 = read.csv2("./Data/DE1_0_2008_Beneficiary_Summary_File_Sample_1.csv",sep=",",dec =",")
Beneficiary2009 = read.csv2("./Data/DE1_0_2009_Beneficiary_Summary_File_Sample_1.csv",sep=",",dec =",")
Beneficiary2010 = read.csv2("./Data/DE1_0_2010_Beneficiary_Summary_File_Sample_1.csv",sep=",",dec =",")
Outpatient = read.csv2("./Data/DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.csv",sep=",",dec =",")
Inpatient = read.csv2("./Data/DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv",sep=",",dec =",")

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
