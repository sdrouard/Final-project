Team: me, myself and I

Format: analyzing a dataset using wrangling and modeling 

Dataset(s): National Health and Nutrition Examination Survey 2017-March2020 (Body Measures + demographic data)

Research question: What is the distribution of obesity in the US and what are the socio-economic risk factors associated with obesity?

Data analysis plan:

Step 0- Initiation 
Create repertory for final project
Upload the data and the documentation 

Step 1- Data management, cleaning and standardization 
Create outcome/ covariate datasets with only the outcomes and covariates of interest
Verify individual identifiers are unique
Merge body measures and demographic data using individual identifiers
Report missingness in outcomes of interest
Verify data are in a reasonable range (no outliers) 
Replace by na if outlier detected
Reformat all column name in lower case
Create clean data excluding pregnant women 

Step 2- Data procession 
Calculate individual bmi and compare it to the given bmi
Create income quintiles
Create an obesity index (coherent measurements between bmi, arm and waist and hips circumferences)

Step 3 - Graphical analysis
Graph the distribution of the obesity index by age, gender and income quintile (or other relevant covariates)
Graph the distribution of the obesity index by gender and income quintile (or other relevant covariates)

Step 4- Data analysis
Evaluate if missingness of outcomes of interest is correlated with the socioeconomic covariates of interest
Regress the obesity index on the covariates of interest and report the results 
Use interpolation to predict the value of the obesity index when missing
Compare the original and interpolated distribution stratified by the relevant covariates

Step 5- Upload final report  
