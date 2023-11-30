Team: me, myself and I

Format: analyzing a dataset using wrangling and modeling 

Dataset(s): National Health and Nutrition Examination Survey 2017-2019 (Body Measures + demographic data)

Research question: What is the distribution of obesity in the US and what are the socio-economic risk factors associated with obesity?

Data analysis plan:

Step 0- Initiation 
Create repertory for final project
Upload the data and the documentation 

Step 1- Data preparation: cleaning and standardization 
-Create the dataset for analysis with body measures (outcomes) and demographic data (covariates).  
    *Restrict data to non-pregnant individuals for whom at least some body measures are available.
-Reformat all column name in lower case.
-Create covariates categrories (age-groups, income quintile) and outcomes (obesity binary)
-Create an alternative outcome variable excluding extreme values
    
Step 2- Descritpive statistics
-Report covariates for the entire sample, and by obesity status
-Report missingness and extreme values in outcomes of interest
-Report graphs of the body measure distributions and discuss if extreme values are outliers
-Analysis the determinants of outliers and missing values

Step 3 - Graphical analysis
-Graph the distribution of the body measures by age group, gender and income quintile (or other relevant covariates)
-Graph the distribution of the body measures by interactions of the relevant covariates

Step 4- Data analysis 
Use interpolation to predict the value of the obesity index when missing/extreme values 
Compare the original and interpolated distribution stratified by the relevant covariates
Analysis the determinants of body measures using a linear regression
Analysis the determinants of obesity with a logistic model

Step 5- Upload final report  
