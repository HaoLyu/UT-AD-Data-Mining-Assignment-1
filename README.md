# UT-AD-Data-Mining-Assignment-1


All problems in this homework are based on the first sample of the DE-SynPuf data. A codebook describing these data can be found here.

1.	(Total 15 points) Birth month and chronic diseases

A recent paper shows that lifetime risks of certain diseases are related to birth month. We will try to see if some of these effects can be observed from DE-SynPuf  data .  Specifically, the Beneficiary file has information regarding 11 chronic conditions, and we will see if these seem to have any significant dependence on the birth month. We shall only look at patients from the mid-west, so you have to select only this subset of the population, but use the data for all 3 years for them.

a.	(5 points) For each of the 11 chronic conditions, use the chi-squared test to determine whether or not there is a significant relationship between it and the birth month.
b.	(3) Adjust the p-values obtained in parts (a) using the Benjamini & Yekutieli method. In R this can be done using the p.adjust function. Briefly explain why this step is important.
c.	(7) Compare your results with the aforementioned recent paper by Boland et al, and briefly comment on differences and similarities in your approach and results. (2-3 paragraphs only).


2.	(Total 25 points) Progression of Chronic Kidney Disease (CKD). (use any three classification models of your choice, e.g. logistic regression, SVMs, gradient boosting, random forests,……… and any language you like.  For python fans scikit-learn is a great resource). 

Using the beneficiary summary file, the inpatient claims file, and the outpatient claims file, we will be exploring chronic kidney disease. This is a disease that progresses in severity until it reaches end stage renal disease if not managed properly.

a.	(5) Visualize the prevalence of different stages (consider people with no CKD as stage 0) of CKD in the 2008 population by plotting percentage of population vs. stage, using the relevant ICD9 codes. Note: the data omits the ‘.’ for ICD codes. Thus, 585.1 = 5851. Please make a note on how you dealt with people who were recorded as being in more than one stage in the same year.  (you will not be considering 2009 or later data).
b.	(5) Obtain the contingency table indicating transitions between stages from 2008 to 2009 by tabulating the different percentages that occur in the “stage in 2009” vs. “stage in 2008” table.
c.	(15) Using demographic and ICD9 variables, develop a model to predict whether a person with stage V CDK will progress to end stage renal disease. In order to do this you need to first figure out how to obtain a feature vector (values of your independent variables) for each beneficiary.  Note that there can be multiple entries for the same person in the same year. How will you combine such information such that you are in the same feature space for each person? Another key issue is the resolution of the features.  Since there are many ICD9 codes , you are better off using the CCS grouping of ICD9 instead (see icd2singleccs.json on Canvas). This file provides a map from ICD9 codes to CCS codes. Use area under an ROC curve on the test set as the evaluation criterion with a 70/30 train/test split. Also, please review this paper predicting progression of chronic kidney disease to get ideas on how to approach the problem. There are in fact many papers on the subject, so your own literature search could be very valuable.

3.	 (Total 25 points) Cost Prediction 

This problem uses the beneficiary summary file. The goal is to predict total reimbursement amount (sum all reimbursement amounts) for patients by year. This problem is very similar to a problem on edX. Feel free to use this edX example as a guide.

a.	(2) Plot the distribution of log(reimbursements).    The log scale is being used because there is a longish tail of high reimbursement values.  If you observe some negative reimbursements, think about what they might mean and how you will deal with them.

For the rest of this problem you will use log_ reimbursements as the dependent variable. 

b.	(3) Do a random subsample of 1,000 patients, and plot cost in 2009 vs. cost in 2008. Fit the mean line. Briefly discuss what you conclude from this exercise.

On your own (do not report): Do some exploratory data analysis using plots to look at the distribution of reimbursement for various chronic diseases.
c.	(10) Using the beneficiary summary file data for year 2009 only and any of the variables you like in it other than the financial/cost ones,  learn a model that predicts the total reimbursement amount for each person. You decide on which model to build (regularized linear regression? SVR? Neural nets? ). 
Which variables seem important in your prediction? Use mean squared error (on log- reimbursements) as the evaluation criterion  and  a 70/30 train/test split.
d.	(10) Use K-means clustering to cluster people into groups using variables other than the dependent variable or financial variables. Justify your value of k. Now for each cluster, build a model to predict reimbursement amounts (as in part (c)). Discuss how your results compare to the global model  you created in step (c).



