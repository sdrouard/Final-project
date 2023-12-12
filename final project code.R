# Final project
# Author: Salome Drouard
# Date: 11 December 2023
rm(list = ls())

### Libraries
library("dplyr")
library("ggplot2")
library("tidyr")
library("data.table")
require(SASxport)
library(foreign)
library("rigr")
library(lme4)
library(Matrix)


### Data
demo <- read.xport("C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\DEMO_J.XPT")
demo <- as.data.table(demo)

bm <- read.xport("C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\BMX_J.XPT")
bm <- as.data.table(bm)


##Step 1- Data preparation: cleaning and standardization 

# 1.1 Create the dataset for analysis with body measures (outcomes) and demographic data (covariates). 
  # 1.1.1 Select relevant columns and rows 
  dt_bm <- bm[, c("SEQN","BMXWT","BMXHT")] 
  #Keep only observations with some body measurements + select height and weight
  
  dt_demo <- demo[(RIDEXPRG!=1|is.na(RIDEXPRG)) & RIDAGEYR>=20, c("SEQN","RIAGENDR","RIDAGEYR", "RIDRETH3", "DMDEDUC2", "INDHHIN2")]  
  #Exclude pregnant women + select covariate columns

# 1.2 Reformat all column name in lower case 
  #(It is not essential but I HATE caps for variable names)
  
    lower <- function(x) {
      cleaned_names <- sapply(names(x), function(name) {
        return(tolower(gsub("\\.", "_", name, fixed = TRUE)))
      })
      names(x) <- cleaned_names
      return(x)
    }
    
    dt_bm <- lower( dt_bm)
    dt_demo <- lower( dt_demo)

# 1.3 Merge body measures and demographic data using individual identifiers
    dt_merged <- merge (dt_demo,dt_bm,by="seqn") #keep only the inner merge
    setDT(dt_merged)

# 1.4 Create covariate categories 
    
  # 1.4.1 Age category  
    dt_merged$age_group <- cut(dt_merged$ridageyr, breaks = seq(from = min(dt_merged$ridageyr, na.rm = TRUE), to = max(dt_merged$ridageyr, na.rm = TRUE) + 10, by = 10), include.lowest = TRUE, labels = FALSE)
 
  #1.4.2 Income quintitle 
    dt_merged <- dt_merged[, indhhin2 := ifelse((indhhin2 == 77|indhhin2 == 99), NA, indhhin2)][, income_quintile:= cut(indhhin2, breaks = 5, labels = c(1, 2, 3, 4, 5))]
      # clean for refuse and don't know
    
  #1.4.3 clean the other covariates
    dt_merged <- dt_merged[,  `:=` (education = ifelse(dmdeduc2 %in% c(7, 9), NA, ifelse(dmdeduc2==1, 2 ,dmdeduc2)), race = ifelse(ridreth3 == 7, NA, ridreth3))][, setnames(dt_merged, c("riagendr", "ridageyr"), c("sex", "age"))]    
       # clean for refuse and don't know, group less than high school level together, clean column names
    
# 1.5 Create an alternative outcome variable excluding extreme values
    
    # 1.5.1 create our own BMI measurement and obesity binary
    dt_merged <- dt_merged[,`:=` (height =  bmxht/100)][, `:=`( bmi = bmxwt/(height^2))][, obesity := ifelse(bmi >= 30, 1, 0)][, obesity := ifelse((is.na(height)|is.na(bmxwt)), NA, obesity)]
                               #convert height in cm

    # 1.5.2 save outcome and covariate names
    out_names <-  names(dt_merged[, c("bmi", "obesity")])
    covariate_names <-  names(dt_merged[, c("age_group", "income_quintile", "race","sex", "education")])
    
    # 1.5.3 Final dataset
    dt_merged <- dt_merged[, c("seqn",out_names,covariate_names, "height", "bmxwt", "age"), with = FALSE] 
    
##Step 2- Descritpive statistics

# 2.2 Extreme values analysis
    
    # 2.2.1 Extreme value detection 
    dt_merged <- dt_merged[, `:=`(mean =  mean(bmi, na.rm = TRUE), sd=sd(bmi, na.rm = TRUE) ), ][, `:=`(outliers = ifelse(abs(bmi-mean)/sd >3, 1, 0))]
      ev_frq_table <- table(dt_merged$outliers )
        print(ev_frq_table)
    
    # 2.2.2 Report the 10 biggest extreme values.
    dt_outliers <- dt_merged[dt_merged$outliers == 1, .SD[order(-bmi)]]
      top_10_outliers <- dt_outliers[1:10, .(height, bmxwt, bmi)]
       print(top_10_outliers)
      # NB: not actual outliers but for the exercise on interpolation will be replaced by mv 

    # 2.2.3 Report graphs of the body measure distributions and discuss if extreme values are outliers
    dt_merged$index <- seq_len(nrow(dt_merged))
    my_labeller <- function(variable,value){
      return(paste(variable, value, sep=": "))
    }
    ggplot(dt_merged, aes(x = index, y = bmi, color = as.factor(outliers))) +
      geom_point() +
      theme_minimal() +
      labs(x = "Index", y = "BMI", color = "Outliers") +
      scale_color_manual(values = c("blue", "red")) +
      ggtitle("Distribution of extreme values")
    ggsave(filename = "C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\outliers.png", width = 10, height = 10)
  
    # 2.2.4 Create an alternative outcome variables excluding extreme values 
    dt_merged <- dt_merged[, `:=`(bmi_alt = bmi, obesity_alt = obesity)][outliers == 1, `:=`(bmi_alt = NA, obesity_alt = NA)]
    
# 2.3 Report missingness 
    
    # 2.3.1 Evaluation of missingness 
    dt_merged <- dt_merged[, missing_values := ifelse(is.na(bmi_alt)>0, 1, 0)]
     mv_frq_table <- table(dt_merged$missing_values )
     print(mv_frq_table)
    
    # 2.3.2 look at the distribution of missing values by covariate 
    mv_frq_tables <- list()
    
    for (var in covariate_names) {
      mv_frq_tables[[var]] <- dt_merged[, .N, by = .(missing_values, get(var))]
    }
    
    for (var in covariate_names) {
      print(paste("Missing values by", var))
      print(mv_frq_tables[[var]])
    }
    
    mm <- glm(missing_values ~ sex + income_quintile + as.factor(race) + as.factor(age_group)  , data = dt_merged, family = binomial)
    summary(mm)
    
      #no concentration of missing values  
    
# 2.1 Report covariates for the entire sample, and by obesity status
 
    # 2.1.1 create Dstat table for all sample
    percentage_list <- lapply(covariate_names, function(covariate) {
      dt <- dt_merged[, .N, by = covariate]
      dt[, Percentage := N / sum(N) * 100]
      return(dt)
    })
    
    print(percentage_list)
    #pretty balanced covariates (expect income because the initial income variable was already from 1 to 10)
    
    # 2.2.1 calculate the mean bmi and prevalence of obesity by levels
    dt_prevalence <- data.table(
      mean_bmi = round(mean(dt_merged$bmi_alt, na.rm = TRUE),1),
      median_bmi = round(median(dt_merged$bmi_alt, na.rm = TRUE),1), 
      p25 = round(quantile(dt_merged$bmi_alt, 0.25, na.rm = TRUE),1), 
      p75 = round(quantile(dt_merged$bmi_alt, 0.75, na.rm = TRUE),1),
      obesity_prevalence_overall = round((mean(dt_merged$obesity_alt, na.rm = TRUE)*100),1),
      underweight = round(mean(dt_merged$bmi_alt < 18.5, na.rm = TRUE) * 100,1),
      normal = round(mean(dt_merged$bmi_alt >= 18.5 & dt_merged$bmi_alt < 24.9, na.rm = TRUE) * 100,1),
      overweight = round(mean(dt_merged$bmi_alt >= 25 & dt_merged$bmi_alt <= 29.9, na.rm = TRUE) * 100,1),
      obesity1 = round(mean(dt_merged$bmi_alt >= 30 & dt_merged$bmi_alt <= 34.9, na.rm = TRUE) * 100,1),
      obesity2 = round(mean(dt_merged$bmi_alt >= 35 & dt_merged$bmi_alt <= 39.9, na.rm = TRUE) * 100,1),
      obesity3 = round(mean(dt_merged$bmi_alt >= 40, na.rm = TRUE) * 100,1)
    )
    
    
##Step 3- Graphical analysis
    
 # 3.1 Overall
    
      ggplot(dt_merged, aes_string(x = "bmi_alt")) +
        geom_histogram(color = "black", fill = "lightgrey")  +
        theme_minimal() +
        labs(x = "BMI", y = "count") +
        geom_vline(aes(xintercept = 18.5), color = "lightblue", linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = 25), color = "green", linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = 30), color = "gold", linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = 35), color = "darkorange", linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = 40), color = "red", linetype = "dashed", size = 1) +
        ggtitle("Overall distribution of bmi")
        
        ggsave(filename = paste0("C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\BMI_Distribution.png"), width = 10, height = 10)
        

# 3.2 by covariates
        # 3.2.1 by sex
        obesity_by_sex <- dt_merged %>%
          group_by(sex) %>%
          summarise(obesity_prevalence = mean(obesity_alt, na.rm = TRUE) * 100)
        
        obesity_by_sex$sex <- factor(obesity_by_sex$sex, levels = c(1, 2), labels = c("Male", "Female"))
        
        ggplot(obesity_by_sex, aes(x = sex, y = obesity_prevalence)) +
          geom_bar(stat = "identity", fill = "purple") +
          geom_text(aes(label = paste0(round(obesity_prevalence, 1), "%")), vjust = -0.3) +
          labs(x = "Sex", y = "Prevalence of Obesity (%)") +
          ggtitle("Prevalence of obesity by sex")
        
        ggsave(filename = paste0("C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\obesity_Distribution_sex.png"), width = 10, height = 10)
        
        # 3.2.2 by age group
        
        obesity_by_age <- dt_merged %>%
          group_by(age_group) %>%
          summarise(obesity_prevalence = mean(obesity_alt, na.rm = TRUE) * 100)
        
        ggplot(obesity_by_age, aes(x = age_group, y = obesity_prevalence)) +
          geom_bar(stat = "identity", fill = "blue") +
          geom_text(aes(label = paste0(round(obesity_prevalence, 1), "%")), vjust = -0.3) +
          labs(x = "Age Group", y = "Prevalence of Obesity (%)") +
          ggtitle("Prevalence of obesity by age group")
        
        ggsave(filename = paste0("C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\obesity_Distribution_age_group.png"), width = 10, height = 10)
        
        # 3.2.3 by race
        
        obesity_by_race <- dt_merged %>%
          group_by(race) %>%
          summarise(obesity_prevalence = mean(obesity_alt, na.rm = TRUE) * 100)
        
        obesity_by_race$race <- factor(obesity_by_race$race, levels = c(1, 2, 3, 4, 6, 7), labels = c("Mexican American", "Other Hispanic", "White", "Black" , "Asian", "Other" ))
        
        ggplot(obesity_by_race, aes(x = race, y = obesity_prevalence)) +
          geom_bar(stat = "identity", fill = "lightblue") +
          geom_text(aes(label = paste0(round(obesity_prevalence, 1), "%")), vjust = -0.3) +
          labs(x = "Race", y = "Prevalence of Obesity (%)") +
          ggtitle("Prevalence of obesity by race")
        ggsave(filename = paste0("C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\obesity_Distribution_race.png"), width = 10, height = 10)
        
        
        # 3.2.4 by income
        
        obesity_by_inc <- dt_merged %>%
          group_by(income_quintile) %>%
          summarise(obesity_prevalence = mean(obesity_alt, na.rm = TRUE) * 100)
        
        ggplot(obesity_by_inc, aes(x = income_quintile, y = obesity_prevalence)) +
          geom_bar(stat = "identity", fill = "green") +
          geom_text(aes(label = paste0(round(obesity_prevalence, 1), "%")), vjust = -0.3) +
          labs(x = "Income quntile", y = "Prevalence of Obesity (%)") +
          ggtitle("Prevalence of obesity by income quintile")
        ggsave(filename = paste0("C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\obesity_Distribution_income.png"), width = 10, height = 10)
        
        
        # 3.2.5 by education
        
        obesity_by_ed <- dt_merged %>%
          group_by(education) %>%
          summarise(obesity_prevalence = mean(obesity_alt, na.rm = TRUE) * 100)
        
        obesity_by_ed$education <- factor(obesity_by_ed$education, levels = c( 2, 3, 4, 5), labels = c("Less than high school", "High school", "Some college", "Graduate or above" ))
        
        ggplot(obesity_by_ed, aes(x = education, y = obesity_prevalence)) +
          geom_bar(stat = "identity", fill = "gold") +
          geom_text(aes(label = paste0(round(obesity_prevalence, 1), "%")), vjust = -0.3) +
          labs(x = "Education level", y = "Prevalence of Obesity (%)") +
          ggtitle("Prevalence of obesity by education level")
        ggsave(filename = paste0("C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\obesity_Distribution_education.png"), width = 10, height = 10)
        
    
 # 3.3 Graph the distribution of the body measures by interactions of the relevant covariates
        
        # 3.3.1 by income x sex
        
        obesity_by_inc_sex <- dt_merged %>%
          group_by(income_quintile,sex) %>%
          summarise(obesity_prevalence = mean(obesity_alt, na.rm = TRUE) * 100)
        
        obesity_by_inc_sex$sex <- factor(obesity_by_inc_sex$sex, levels = c(1, 2), labels = c("Male", "Female"))
        
        ggplot(obesity_by_inc_sex, aes(x = income_quintile, y = obesity_prevalence, fill = sex)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste0(round(obesity_prevalence, 1), "%")), vjust = -0.3) +
          scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
          labs(x = "Income Quintile", y = "Prevalence of Obesity (%)", fill = "Sex") +
          ggtitle("Prevalence of Obesity by Income Quintile") +
          facet_wrap(~sex)
        ggsave(filename = paste0("C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\obesity_Distribution_inc_sex.png"), width = 10, height = 10)
        
      
## Step 4- Data analysis   
      
 # 4.1 linear model on the determinants of bmi
      dt_merged$age2 <- dt_merged$age^2
      model1 <- lm(bmi_alt ~ sex*income_quintile  + as.factor(race) + age + age2 , data = dt_merged)
      summary(model1)
      
 # 4.2 logistic model on the determinants of obesity
      
      model2 <- glm(obesity_alt ~ sex*income_quintile + as.factor(race) + age + age2 , data = dt_merged, family = binomial)
      summary(model2)
      odds_ratios <- exp(coef(model2))
      conf_int <- exp(confint(model2))
      print(odds_ratios)
      print(conf_int)
      
## Step 5- Interpolation of missing values 
      
  # 5.1 Predictions 
      # 5.1.1 For bmi
        dt_merged$predicted_bmi <- predict(model1, newdata = dt_merged)
        
      # 5.1.2 For obesity 
          #from model
        dt_merged$predicted_obesity_model <- predict(model2, newdata = dt_merged, type="response")
        dt_merged$predicted_obesity_model <- dt_merged[, ifelse(predicted_obesity_model > 0.5, 1 , 0)]
        
          #from predicted bmi
        dt_merged$predicted_obesity_bmi <- dt_merged[, ifelse(predicted_bmi >=30,1,0)]
        
      
  # 5.2 Summary of the results
        
      # 5.2.1 Graph predictions for bmi
        
        # 5.2.1.1 save predicted observations
         predicted_sample <- dt_merged[dt_merged$missing == 1, ] 
         
        # 5.2.1.2 Random sample of complete data
         temp <- dt_merged[dt_merged$missing == 0, ]
         
         set.seed(22) 
         sampled_rows <- sample(nrow(temp), size = 500, replace = FALSE)
         random_sample <- temp[sampled_rows, ]
         
         # 5.2.1.3 Append data and replace by predicted values
         interpolation_data <- rbind(predicted_sample, random_sample)
         
         interpolation_data$bmi_alt <- interpolation_data[ , ifelse(is.na(bmi_alt), predicted_bmi, bmi_alt) ]
         
         # 5.2.1.4 Overlay interpolated and original bmi
         
         ggplot(interpolation_data, aes(x = index, y = bmi_alt, color = factor(missing_values))) +
           geom_point() +
         labs(x = "Index", y = "BMI") +
           ggtitle("Interpolated and original bmi") 
         
         ggsave(filename = paste0("C:\\Users\\sdrouard\\Documents\\coursework\\Final_project\\interpolated.png"), width = 10, height = 10)
       
      
      # 5.2.2 Obesity: Concordance of both methods
         table(predicted_sample$predicted_obesity_bmi,predicted_sample$predicted_obesity_model)
           
         

  
         
