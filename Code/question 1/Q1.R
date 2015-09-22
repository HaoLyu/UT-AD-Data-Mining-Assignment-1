# Question 1
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
