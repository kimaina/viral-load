
# increase memory
options(java.parameters = "-Xmx15g")
# knitting options
knitr::opts_chunk$set(
	fig.path='results/figure-vl2/', cache.path='results/cache-vl2/', # sets folder for saving graphics
    fig.align='center',external=TRUE, warning=F, # alligns figures to center, no print warnings
    fig.pos="h", table.placement="h", #fig.height=4,  # makes tables and graphs in correct order with text
	echo=FALSE, message=FALSE,  comment=" ", #don't print code or messages, saves in cache
tidy=TRUE,tidy.opts=list(width.cutoff=20)
) # makes printing code not run off page
a4width<- 8.3
a4height<- 9


# function to install missing packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
  sapply(pkg, require, character.only = TRUE)
}

#install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')

packages =c( "dplyr","car","sjPlot","sjmisc","sjlabelled","ggpubr","ggpmisc","gridExtra","stargazer","e1071","jtools",
             "effects","multcompView","ggplot2","ggrepel","MASS","broom","ggcorrplot","leaps","relaimpo","olsrr","reshape2",
             "ROCR","arm","foreign","nnet","VGAM","ordinal","ModelGood","InformationValue","rms","texreg","knitr","pastecs",
             "xtable","easyGgplot2","tidyverse","ISLR","randomForest","caret","rpart","rpart.plot","naniar","caret","C50", "plotROC", "skimr", "lattice", "ggthemes", "caretEnsemble", "Hmisc", "DMwR",
             "doParallel","bartMachine","xgboost","PerformanceAnalytics", "knitr", "kableExtra")

ipak(packages)
#options(allow_html_in_all_outputs=TRUE)

# packages not pubished
#devtools::install_github("kassambara/easyGgplot2")
library(easyGgplot2)

# packages not pubished
#devtools::install_github('cttobin/ggthemr')
library(ggthemr)


# custom functions
source("roc/unbalanced_functions.R")


select = dplyr::select; summarize = dplyr::summarize; rename = dplyr::rename; mutate = dplyr::mutate;

ggthemr("flat")
predictName="virologic_failure2"





# Data Exploration 

#In this section we will explore our data before using it to create 3 models for the binary classification task of predicting whether or not a patient is suppressed using classical binary classification models. 


## Import Wrangled Data
#This is a 1 row per patient dataset created in the previous section with the following features. Note, not all of these features wil be used for prediction. We will only be using baseline features.

##```{r}
# import data
hiv_first_vl <- read.csv("hiv_first_vl.csv/hiv_first_vl.csv")

# calculate proportion
hiv_first_vl = hiv_first_vl%>%
  mutate(
    num_days_on_arvs=max_days_on_arvs,
    num_days_in_care=max_days_on_treatment,
    num_days_on_tb_meds= max_tb_tx_days,
    num_days_on_tb_prop=max_tb_prop_days,
    prop_days_on_arvs =ifelse(max_days_on_arvs>=max_days_on_treatment,0,max_days_on_arvs/max_days_on_treatment),
    prop_days_on_tb_meds =ifelse(max_tb_tx_days>=max_days_on_treatment,0,max_tb_tx_days/max_days_on_treatment),
    prop_days_on_tb_prop =ifelse(max_tb_prop_days>=max_days_on_treatment,0,max_tb_prop_days/max_days_on_treatment),
    prop_defaulted_apptmts=num_defaulted_apptmt/num_encounters,
    vl_count_1_log=log(vl_count_1+.00001),
    prop_bad_adherence=num_bad_adherence/num_encounters
    
    
  )


response_var= c('virologic_failure1',"virologic_failure2")

# binary variables -has_used_contraceptive
binary_var = c(
  'is_male', 'is_status_disclosed',
  
  # boolean                                                           
  'is_on_contraceptive','is_on_health_cover','is_on_cryptococcus_tx','is_on_tb_prophy_regimen','has_sti_symptoms','has_tb_symptoms',
  'has_drug_tox_efcts','has_toxic_drug','has_referral_order','has_phdp_referral','needs_fam_tx_support','has_changed_pcp',
  'has_changed_tb_tx','has_restarted_tb_tx','has_been_hospitalized','has_sulf_peni_rxns',
  
  # physical exam
  'is_general_pexam', 'is_skin_pexam','is_lymph_nodes_pexam','is_respiratory_pexam','is_heent_pexam','is_cardiac_pexam',
  'is_abdominal_pexam','is_urogenital_pexam','is_extremies_pexam','is_psychiatric_pexam','is_neurologic_pexam',
  'is_musculoskeletal_pexam', 'is_cxr_code_labs',
  
  # vitals
  'is_underweight', 'has_high_bp', 'has_low_bp', 'has_abnormal_oxy_sat', 'has_fever'
)

# categorical variables
categorical_var =c(
  'health_cover', 'cryptococcus_tx','contraceptive', 'poor_adherence_rsn',
  'cur_pcp_prophylaxis', 'tb_tx_phase', 'tb_tx_regimen', 'tb_prophy_regimen',
  'sti_symptoms', 'tb_assmt_status', 'tb_symptoms', 'drug_toxicity_effects',
  'sulf_peni_other_reactions', 'toxic_drug', 'drug_toxicity_severity',
  'referral_ordered','drug_toxicity_cause', 'phdp_referral', 'family_tx_support',
  'bmi_status', 'pulse_status'
)

# numerical variables
numerical_var =c(
  'first_age', 'bmi', #'avg_weight','height',
  #"num_days_on_arvs","num_days_in_care","num_days_on_tb_meds","num_days_on_tb_prop",
  "prop_days_on_arvs","prop_days_on_tb_meds","prop_days_on_tb_prop",
  "prop_defaulted_apptmts","prop_bad_adherence", "num_encounters", "num_days_in_care", "vl_count_1_log"
  #"changed_location","changed_who_stages","arv_lines_changed", "arv_meds_changed",
  #"adherence_changes","num_pcs_changes"
  #'max_temp','min_oxy_sat','max_sbp','avg_dbp','avg_pulse'
  #'max_days_on_arvs','max_tb_tx_days','max_tb_prop_days','max_days_on_treatment',
  #'max_days_btwn_apptmts','num_defaulted_apptmt','num_bad_adherence'
)

# ordinal variables
ordinal_var =c(
  'first_who_stage','first_arv_line'
)

# save for reporting purposes
dt=hiv_first_vl%>%select(c(numerical_var,ordinal_var,binary_var))
saveRDS(dt, file = "results/dataset.rds")

#```

#```{r eval=FALSE}

# Histogram plots with mean lines
vl1 <- hiv_first_vl %>%
  filter(!is.na(vl_count_1))

seq=c('is_general_pexam', 'is_skin_pexam', 'is_lymph_nodes_pexam', 'is_respiratory_pexam', 'is_heent_pexam',
      'is_cardiac_pexam','is_abdominal_pexam','is_urogenital_pexam','is_extremies_pexam','is_psychiatric_pexam',
      'is_neurologic_pexam','is_musculoskeletal_pexam', 'is_cxr_code_labs'  )

for (value in numerical_var ) { 
  p1 <- ggplot2.histogram(vl1, xName=value, backgroundColor="white",  bins=10,
                          #brewerPalette="Paired",
                          groupName="suppressed", legendPosition="top", xScale="log2",
                          mainTitle=paste(value, " | Suppressed = 1"),
                          alpha=0.5, addDensity=TRUE,  removePanelGrid=TRUE ,removePanelBorder=TRUE,
                          addMeanLine=TRUE, meanLineSize=1)
  p2<- ggboxplot(vl1, x = "suppressed", y = value, color = "suppressed", palette = "jco", add = "jitter")+ stat_compare_means()	
  
  print(grid.arrange( p1,p2, ncol = 2.) )
  # print(p2 )
}

#```

##```{r}

# convert categorical data into factor
hiv_first_vl[categorical_var] <- lapply(hiv_first_vl[categorical_var], factor)


# create data 

hiv_first_vl$vl2_suppression <- ifelse(hiv_first_vl$vl_count_2<=1000, 1,0)
hiv_first_vl$vl3_suppression <- ifelse(hiv_first_vl$vl_count_3<=1000, 1,0)
hiv_first_vl$vl4_suppression <- ifelse(hiv_first_vl$vl_count_4<=1000, 1,0)
hiv_first_vl$vl5_suppression <- ifelse(hiv_first_vl$vl_count_5<=1000, 1,0)


# re-order levels
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

#xtable(head(hiv_first_vl ), comment=FALSE)

#```

##```{r}
hiv_dfl=hiv_first_vl%>%filter(!is.na(vl_count_1))

hiv_dfl$vl2_suppression <- ifelse(hiv_dfl$vl_count_2<=1000, "No","Yes")

a =hiv_dfl%>% mutate(VL_Failure=ifelse(vl_count_1<=1000, "No","Yes")) %>%
  ggboxplot( palette = "jco",  ylab = "VL Count",
             y = c("vl_count_1"),  
             x = c("VL_Failure"), 
             shape = "VL_Failure",	
             yscale = c("log2"),
             add = "jitter", 
             color = "VL_Failure",  add.params = list(size = 0.5, jitter = 0.2) )+
  scale_color_manual(values= c("#00AFBB", "#E7B800"))+ scale_fill_manual(values= c("#00AFBB", "#E7B800"))


b =hiv_dfl%>% mutate(VL_Failure=ifelse(vl_count_2<=1000, "No","Yes")) %>%
  ggboxplot( palette = "jco",  ylab = "VL Count 2",
             y = c("vl_count_2"),  
             x = c("VL_Failure"), 
             shape = "VL_Failure",	
             yscale = c("log2"),
             add = "jitter", 
             color = "VL_Failure",  add.params = list(size = 0.5, jitter = 0.2) )	+
  scale_color_manual(values= c("#00AFBB", "#E7B800"))+ scale_fill_manual(values= c("#00AFBB", "#E7B800"))


c = hiv_dfl%>% mutate(VL_Failure=ifelse(vl_count_1<=1000, "No","Yes")) %>%
  ggplot2.histogram( xName="vl_count_1", backgroundColor="white",  bins=25,
                     #brewerPalette="Paired",
                     groupName="VL_Failure", xScale="log10",rug = TRUE ,    
                     alpha=0.5, addDensity=TRUE,  removePanelGrid=TRUE ,removePanelBorder=TRUE,
                     addMeanLine=TRUE, meanLineSize=1) + xlab("VL Count 1")+
  scale_color_manual(values= c("#00AFBB", "#E7B800")) + scale_fill_manual(values= c("#00AFBB", "#E7B800"))

d = hiv_dfl%>% mutate(VL_Failure=ifelse(vl_count_2<=1000, "No","Yes")) %>%
  ggplot2.histogram( xName="vl_count_2", backgroundColor="white",  bins=25,
                     #brewerPalette="Paired",
                     groupName="VL_Failure", xScale="log10", xlab = "VL Count 2",rug = TRUE ,
                     alpha=0.5, addDensity=TRUE,  removePanelGrid=TRUE ,removePanelBorder=TRUE,
                     addMeanLine=TRUE, meanLineSize=1) + xlab("VL Count 2")+
  scale_color_manual(values= c("#00AFBB", "#E7B800"))+ scale_fill_manual(values= c("#00AFBB", "#E7B800"))

ggarrange(a,b,c,d,
          common.legend = TRUE, legend="bottom",	
          ncol = 2, nrow=2)	

#```





## Viral Load Flow Chat
### First Viral Load
#Out of the 30,063 patients, only 10,270 patients had a baseline viral load. Out of 10,270, 78% of them were virally suppressed

##```{r}
df =hiv_first_vl %>%
  summarise(
    patients_missing_vl= sum(is.na(vl_count_1)),
    patients_with_vl = sum(!is.na(vl_count_1)), 
    suppressed =  sum(suppressed),
    not_suppressed = sum(!is.na(vl_count_1)) - sum(suppressed),
    suppression_prevalence =  sum(suppressed)/sum(!is.na(vl_count_1))
    
  )
kable(df)
#```

### Second Viral Load

#```{r }
df =hiv_first_vl %>%
  summarise(
    patients_missing_vl= sum(is.na(vl_count_2)),
    patients_with_vl = sum(!is.na(vl_count_2)), 
    suppressed =  sum(vl2_suppression, na.rm = TRUE),
    not_suppressed = sum(!is.na(vl_count_2)) - sum(vl2_suppression, na.rm = TRUE),
    suppression_prevalence =  sum(vl2_suppression, na.rm = TRUE)/sum(!is.na(vl_count_2))
    
  )
kable(df)
#```

### Third Viral Load

##```{r}
df =hiv_first_vl %>%
  summarise(
    patients_missing_vl= sum(is.na(vl_count_3)),
    patients_with_vl = sum(!is.na(vl_count_3)), 
    suppressed =  sum(vl3_suppression, na.rm = TRUE),
    not_suppressed = sum(!is.na(vl_count_3)) - sum(vl3_suppression, na.rm = TRUE),
    suppression_prevalence =  sum(vl3_suppression, na.rm = TRUE)/sum(!is.na(vl_count_3))
    
  )
kable(df)
#```

### Forth Viral Load

##```{r}
df =hiv_first_vl %>%
  summarise(
    patients_missing_vl= sum(is.na(vl_count_4)),
    patients_with_vl = sum(!is.na(vl_count_4)), 
    suppressed =  sum(vl4_suppression, na.rm = TRUE),
    not_suppressed = sum(!is.na(vl_count_4)) - sum(vl4_suppression, na.rm = TRUE),
    suppression_prevalence =  sum(vl4_suppression, na.rm = TRUE)/sum(!is.na(vl_count_4))
    
  )
kable(df)
#```


### Fifth Viral Load

##```{r}
df =hiv_first_vl %>%
  summarise(
    patients_missing_vl= sum(is.na(vl_count_5)),
    patients_with_vl = sum(!is.na(vl_count_5)), 
    suppressed =  sum(vl5_suppression, na.rm = TRUE),
    not_suppressed = sum(!is.na(vl_count_5)) - sum(vl5_suppression, na.rm = TRUE),
    suppression_prevalence =  sum(vl5_suppression, na.rm = TRUE)/sum(!is.na(vl_count_5))
    
  )
kable(df)
#```



##```{r}
DF <- read.table(text="VL Not_Suppressed Suppressed    
1   2198    8072
2   1185    4942 
3   653     2656 
4   283     1100
5   108     256 
", header=TRUE)


DF1 <- melt(DF, id.var="VL")


ggplot(DF1, aes(x = VL, y = value, fill = variable, label = value)) + 
  geom_bar(stat = "identity")+  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  labs(x='VL Index', y='# Patients', title = "Stratification by Suppression Status")+
  theme_classic() + theme(legend.position="bottom")

#```





# Covariates

#```{r }
library(skimr)
skimmed <- skim_to_wide(hiv_first_vl%>%filter(!is.na(bmi) ))
kable(skimmed)
#```
## Training and Validation Sets


#The dataset was split into training and External Validation set by ratio .9:.1 i.e 4615 (90%) and 512 (10%)

##```{r}
# https://www.machinelearningplus.com/machine-learning/caret-package/
#ML preProcess(iris[,1:4], method=c("scale"))





library(Hmisc)
set.seed(123)
training_test_set = hiv_first_vl%>%filter(!is.na(bmi) )%>%filter(!is.na(vl_count_1))
#training_test_set$first_weight = impute(training_test_set$first_weight, median)  # replace with mean
#training_test_set$height = impute(training_test_set$height, median)  # replac



# virologic_failure 1
training_test_set$virologic_failure1 <- ifelse(training_test_set$suppressed==0,1,0)


#virologic_failure 2
training_test_set$virologic_failure2 <- ifelse(training_test_set$vl2_suppression==0,"Yes", "No")
training_test_set$virologic_failure2 <-as.factor(training_test_set$virologic_failure2)
training_test_set$virologic_failure2 <- relevel(training_test_set$virologic_failure2, ref = "Yes")

# impute missing
ml_dataframe = training_test_set%>%
  filter(!is.na(vl_count_1))%>%
  filter(!is.na(vl_count_2))%>%
  dplyr::select( c(numerical_var,ordinal_var,binary_var, response_var
                   #,categorical_var
  ))


#ml_dataframe <- oneHotNoMulti(ml_dataframe, categorical_var)


# pre-process
#ml_dataframe.preprocessParams=preProcess(ml_dataframe[1:100,], method=c("scale","center"))
#ml_dataframe.transformed <- predict(ml_dataframe.preprocessParams,ml_dataframe[1:100,])

# create one hot model
train_onehot_fit=dummyVars(virologic_failure2 ~ ., data=ml_dataframe)

# create train/test sets
train_index <- createDataPartition(ml_dataframe$virologic_failure2, p=.8,
                                   list=FALSE,
                                   times = 1)

train_DF <- ml_dataframe[train_index,]
validate_DF <- ml_dataframe[-train_index,]

# one hot encoding - please comment out
train_onehot=  data.frame(predict(train_onehot_fit, newdata = train_DF))
validate_onehot=  data.frame(predict(train_onehot_fit, newdata = validate_DF))

train_y <- train_DF$virologic_failure2
train_x <- train_onehot

validate_y <- validate_DF$virologic_failure2
validate_x <- validate_onehot


gg_miss_var(ml_dataframe )+ labs(title = "Frequency of Missingness")

#chart.Correlation(train_DF, histogram=TRUE, pch=19)	
#```

#```{r  cache=F}
# define training control
train_control <- caret::trainControl(
  index = createFolds(train_DF$virologic_failure2, 10), # have each folds balance vl class
  #sampling = "smote",
  verboseIter = F, # no training log
  allowParallel = TRUE, # FALSE for reproducible results 
  summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE
  # ,preProc = c("center", "scale")
)



#```

# Logistic Regression Model 

#We used logistic regression estimates the probability of an outcome. Events are coded as binary variables with a value of 1 representing suppression, and a value of zero representing treatment failure


#```{r warning=TRUE,  cache=TRUE}
#https://rstudio-pubs-static.s3.amazonaws.com/43302_2d242dbea93b46c98ed60f6ac8c62edf.html
#http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html
#set.seed(7)


virologic_failure2.logit.cv <- train(virologic_failure2 ~.,data=train_DF, 
                                     method = "glm",#method = "glmStepAIC",  direction ="both",
                                     trControl = train_control,metric = "Sens")

# print cv scores
summary(virologic_failure2.logit.cv$finalModel)

#```





### Model Discrimination Analyisis

##```{r}
source("roc/unbalanced_functions.R")
md2.ols = generateClassificationStats(train_DF, virologic_failure2.logit.cv, cutoff=.4, modelName="Logistic Model", predictName=predictName)
#plot Sens
md2.ols$roc
md2.ols$calibration
# plot dist
md2.ols$distribution
# Sensitivity Specificty plots
##ggthemr("light")
md2.ols$accuracy_info$plot
#md$accuracy_info$data
# confusion matrix  
#ggthemr("light")
md2.ols$cm_info$plot
#```


### Confusion Matrix

##```{r}
md2.ols$cm_info$confMatrix

#```

### External Validation


#```{r  echo=F, eval=T, results="hide", fig.show='hide'}

mdt2.ols = generateClassificationStats(validate_DF, virologic_failure2.logit.cv, cutoff=.4, modelName="virologic_failure2.logit.cv", predictName=predictName)
#```

#```{r  echo=F, eval=T }
# plot dist
mdt2.ols$distribution
#ggthemr("light")
mdt2.ols$cm_info$plot
mdt2.ols$cm_info$confMatrix


#```



# Penalised Logistic Regression Model 


#```{r cache=TRUE}
#https://rstudio-pubs-static.s3.amazonaws.com/43302_2d242dbea93b46c98ed60f6ac8c62edf.html
#http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html
#set.seed(7)
tune_grid.pls=expand.grid(alpha = 0.3:.5,
                          lambda = seq(0.03,0.05,by = 0.01) )

virologic_failure2.logit.rl.cv <-train(virologic_failure2 ~.,data=train_DF, method = "glmnet", 
                                       trControl = train_control,metric = "Sens",
                                       tuneGrid = tune_grid.pls)


# print cv scores

ggplot(virologic_failure2.logit.rl.cv)
#print(virologic_failure2.logit.cv)

plot(virologic_failure2.logit.rl.cv$finalModel, xvar = "lambda")
abline(v = log(virologic_failure2.logit.rl.cv$bestTune$lambda), col = "red", lty = "dashed")

#```





### Model summary

#```{r cache=T}

#print(virologic_failure2.logit.rl.cv$resample)
coef(virologic_failure2.logit.rl.cv$finalModel, s =virologic_failure2.logit.rl.cv$bestTune$lambda
)
#ggthemr("light")
coef(virologic_failure2.logit.rl.cv$finalModel, s =virologic_failure2.logit.rl.cv$bestTune$lambda
) %>%
  broom::tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(50, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("GLMNET Variable Shrinkage") +
  xlab("Coefficient") +
  theme_economist()+
  ylab(NULL)



#```

### Model Discrimination Analyisis

#```{r eval=T}

#source("roc/unbalanced_functions.R")

md2.glmnet = generateClassificationStats(train_DF, virologic_failure2.logit.rl.cv, cutoff=.4, modelName="Penalised Logistic Model",
                                         predictName=predictName)
#plot Sens
md2.glmnet$roc
md2.glmnet$calibration
# plot dist
md2.glmnet$distribution
# Sensitivity Specificty plots
#ggthemr("light")
md2.glmnet$accuracy_info$plot
#md2.glmnet$accuracy_info$data
# confusion matrix  
#ggthemr("light")
md2.glmnet$cm_info$plot


#```


### Confusion Matrix

##```{r}
md2.glmnet$cm_info$confMatrix

#```



### External Validation

#```{r  echo=F, eval=T, results="hide", fig.show='hide'}

mdt2.glmnet = generateClassificationStats(validate_DF, virologic_failure2.logit.rl.cv, cutoff=.4, modelName="virologic_failure2.logit.rl.cv", predictName=predictName)
#```

#```{r  echo=F, eval=T}
# plot dist
mdt2.glmnet$distribution
#ggthemr("light")
mdt2.glmnet$cm_info$plot
mdt2.glmnet$cm_info$confMatrix


#```



# KNN
##In pattern and class recognition, the k-nearest neighbors algorithm (k-NN) is a non-parametric method used for classification and regression. k-NN is a type of instance-based learning, or lazy learning, where the function is only approximated locally and all computation is deferred until classification. 

#```{r cache=TRUE}
#set.seed(825)
# do parallel
cluster <- makePSOCKcluster(detectCores() ) # convention to leave 1 core for OS
registerDoParallel(cluster)

tune_grid.knn = expand.grid(k = (44:45))
# train the model 
system.time(virologic_failure2.knn.cv<- train(virologic_failure2~., data=train_DF,  trControl=train_control,  method="knn", 
                                              tuneGrid = tune_grid.knn,
                                              metric = "Sens",maximize = TRUE))

# shut down cluster
stopCluster(cluster)
registerDoSEQ()

# print cv scores
print(virologic_failure2.knn.cv)


#```


#```{r cache=T}


ggplot(virologic_failure2.knn.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 

#```





### Model Discrimination Analyisis

##```{r}
#source("roc/unbalanced_functions.R")

md2.knn = generateClassificationStats(train_DF, virologic_failure2.knn.cv, cutoff=.4, modelName="KNN Model", predictName=predictName)
#plot Sens
md2.knn$roc
md2.knn$calibration
# plot dist
md2.knn$distribution
# Sensitivity Specificty plots
#ggthemr("light")
md2.knn$accuracy_info$plot
#md2.knn$accuracy_info$data
# confusion matrix  
#ggthemr("light")
md2.knn$cm_info$plot

#```


### Confusion Matrix

##```{r}

md2.knn$cm_info$confMatrix
#```


### External Validation

#```{r  echo=F, eval=T, results="hide", fig.show='hide'}

mdt2.knn = generateClassificationStats(validate_DF, virologic_failure2.knn.cv, cutoff=.4, modelName="knn", predictName=predictName)
#```

#```{r  echo=F, eval=T}
# plot dist
mdt2.knn$distribution
#ggthemr("light")
mdt2.knn$cm_info$plot
mdt2.knn$cm_info$confMatrix


#```



# Classification and Regression Trees (CART)

#Classification and Regression Trees (CART) were first introducted in 1984 by a group led by Leo Briemann (Brieman et al. 1984). The CART algorithm provided a means to sequentially conduct binary splits on variables provided to the algorithm, resulting in a decision structure that resembles its namesake, a tree. 

#```{r cache=TRUE}

#set.seed(825)
tune_grid.cart = expand.grid(cp = seq(0,0.001,by = 0.0001))
# train the model 
virologic_failure2.dt.cv<- train(virologic_failure2~., data=train_DF,  trControl=train_control,  method="rpart", 
                                 tuneGrid =tune_grid.cart, parms = list(split = "gini"),
                                 metric = "Sens",maximize = TRUE)

# print cv scores
print(virologic_failure2.dt.cv)




#```


#```{r cache=T}


ggplot(virologic_failure2.dt.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 

#```



### Tree Diagram

##```{r}

#printcp(d_tree) # display the results 
#plotcp(d_tree) # visualize cross-validation results 
#summary(d_tree) # detailed summary of splits

rpart.plot(virologic_failure2.dt.cv$finalModel, main="Decsion Tree", fallen.leaves=F, extra=104, box.palette="GnBu")


#```

### Model Discrimination Analyisis

##```{r}
#source("roc/unbalanced_functions.R")

md2.cart = generateClassificationStats(train_DF, virologic_failure2.dt.cv, cutoff=.4, modelName="CART Model", predictName=predictName)
#plot Sens
md2.cart$roc
md2.cart$calibration
# plot dist
md2.cart$distribution
# Sensitivity Specificty plots
#ggthemr("light")
md2.cart$accuracy_info$plot
#md2.cart$accuracy_info$data
# confusion matrix  
#ggthemr("light")
md2.cart$cm_info$plot

#```


### Confusion Matrix

##```{r}

md2.cart$cm_info$confMatrix
#```



### External Validation

#```{r  echo=F, eval=T, results="hide", fig.show='hide'}

mdt2.cart = generateClassificationStats(validate_DF, virologic_failure2.dt.cv, cutoff=.4, modelName="CART", predictName=predictName)
#```

#```{r  echo=F, eval=T }
# plot dist
mdt2.cart$distribution
#ggthemr("light")
mdt2.cart$cm_info$plot
mdt2.cart$cm_info$confMatrix

#```



# XGBOOST
#https://xgboost.readthedocs.io/en/latest/parameter.html

#```{r cache=TRUE}

#set.seed(825)
nrounds <- 1000

# note to start nrounds from 200, as smaller learning rates result in errors so
# big with lower starting points that they'll mess the scales
tune_grid.xgb <- expand.grid(
  nrounds = seq(from = 100, to = nrounds, by = 50),
  eta =c( .2,.1#,.4#,.8,.9, .98,1.0
  ), # learning rate default=0.3 range = [0,1], prevents overfitting
  max_depth = c( 2,3#,4#, 5#,6 
  ), # default 6, incr this value will make the model overfitt and complex
  gamma = c(0), # min loss reduction required to make a further partition on a leaf. range: [0,inf] def 0
  colsample_bytree =c(1), # 1 means all columns are used in each decision tree. range [0,1]
  min_child_weight = c(1), # default 1, range: [0,inf]
  subsample = 1 # range: (0,1] default 1; 0.5 means xgb will randomly sample half of the training data prior to growing trees. and this will prevent overfitting. 
)




# train the model # try neg_log_loss,error
virologic_failure2.xgb.cv<- train(virologic_failure2 ~ ., data=train_DF, tuneGrid=tune_grid.xgb, trControl=train_control, 
                                  metric = "Sens" ,method="xgbTree",  verbose = T,  maximize = TRUE)


# save model
#xgb.save(virologic_failure2.xgb.cv$finalModel, 'models/virologic_failure2.cv.xgb.model')

# print cv scores
#print(virologic_failure2.xgb.cv$finalModel)


# resample

#```



#```{r cache=T}


ggplot(virologic_failure2.xgb.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 

#```


### Relative Importance

##```{r}
# get the trained model
model = xgb.dump(virologic_failure2.xgb.cv$finalModel, with.stats=TRUE)

importance_matrix = xgb.importance( model=virologic_failure2.xgb.cv$finalModel)

# plot
gp = xgb.plot.importance(importance_matrix, top_n = 20 )
print(gp) 


#```

### Model Discrimination Analyisis

##```{r}
#source("roc/unbalanced_functions.R")

md2.xgb = generateClassificationStats(train_DF, virologic_failure2.xgb.cv, cutoff=.4, modelName="XGBoost Model", predictName=predictName)
#plot Sens
md2.xgb$roc
md2.xgb$calibration
# plot dist
md2.xgb$distribution
# Sensitivity Specificty plots
#ggthemr("light")
md2.xgb$accuracy_info$plot
#md2.xgb$accuracy_info$data
# confusion matrix  
#ggthemr("light")
md2.xgb$cm_info$plot



#```

### Confusion Matrix

##```{r}
md2.xgb$cm_info$confMatrix
#```






### External Validation

#```{r  echo=F, eval=T, results="hide", fig.show='hide'}

mdt2.xgb = generateClassificationStats(validate_DF, virologic_failure2.xgb.cv, cutoff=.4, modelName="XGBoost", predictName=predictName)
#```

#```{r  echo=F, eval=T }
# plot dist
mdt2.xgb$distribution
#ggthemr("light")
mdt2.xgb$cm_info$plot
mdt2.xgb$cm_info$confMatrix

#```




# GBM
#https://xgboost.readthedocs.io/en/latest/parameter.html

#```{r cache=TRUE}

#set.seed(825)

# do parallel
cluster <- makePSOCKcluster(detectCores() ) # convention to leave 1 core for OS
registerDoParallel(cluster)

#define tunegrid
tune_grid.gbm <- expand.grid(
  interaction.depth = c(9,10), 
  n.trees = c(100, 300, 400), 
  shrinkage = 0.1,
  n.minobsinnode = 20)

# train the model # try neg_log_loss,error
system.time(virologic_failure2.gbm.cv<- train(virologic_failure2 ~ ., data=train_DF, tuneGrid=tune_grid.gbm, trControl=train_control,#metric = "Sens" ,
                                              method="gbm",  verbose = F,  maximize = TRUE))
# shut down cluster
stopCluster(cluster)
registerDoSEQ()
virologic_failure2.gbm.cv

#```

#```{r cache=T}


ggplot(virologic_failure2.gbm.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 


#```

### Relative Importance

##```{r}
library(gbm)

ggplot(varImp(virologic_failure2.gbm.cv), top=20) 

#```


### Model Discrimination Analyisis

##```{r}
#source("roc/unbalanced_functions.R")

md2.gbm = generateClassificationStats(train_DF, virologic_failure2.gbm.cv, cutoff=.4, modelName="GBM Model", predictName=predictName)
#plot Sens
md2.gbm$roc
md2.gbm$calibration
# plot dist
md2.gbm$distribution
# Sensitivity Specificty plots
#ggthemr("light")
md2.gbm$accuracy_info$plot
#md2.gbm$accuracy_info$data
# confusion matrix  
#ggthemr("light")
md2.gbm$cm_info$plot



#```

### Confusion Matrix

##```{r}
md2.gbm$cm_info$confMatrix
#```



### External Validation

#```{r  echo=F, results="hide", fig.show='hide', eval=T}

mdt2.gbm = generateClassificationStats(validate_DF, virologic_failure2.gbm.cv, cutoff=.4, modelName="GBM Model", predictName=predictName)
#```

#```{r  echo=F, eval=T }
# plot dist
mdt2.gbm$distribution
#ggthemr("light")
mdt2.gbm$cm_info$plot
mdt2.gbm$cm_info$confMatrix

#```






# Random Forest
#Random forests improve predictive accuracy by generating a large number of bootstrapped trees (based on random samples of variables), classifying a case using each tree in this new "forest", and deciding a final predicted outcome by combining the results across all of the trees (an average in regression, a majority vote in classification). Breiman and Cutler's random forest approach is implimented via the randomForest package.

#```{r cache=TRUE}

#set.seed(825)
# do parallel
cluster <- makePSOCKcluster(detectCores() ) # convention to leave 1 core for OS
registerDoParallel(cluster)

#create tunegrid with 15 values from 1:15 for mtry to tunning model. Our train function will change number of entry variable at each split according to tunegrid. 
tunegrid.rf <- expand.grid(.mtry=c(3,5))
# train the model 
system.time(
  virologic_failure2.rf.cv<- train(virologic_failure2 ~ ., data=train_DF, tuneGrid=tunegrid.rf, ntrees=500,
                            trControl=train_control, metric = "Sens", method="rf",  verbose = F,  maximize = TRUE)
)

# shut down cluster
stopCluster(cluster)
registerDoSEQ()

# print cv scores
#print(virologic_failure2.rf.cv)

#```


#```{r cache=T}


ggplot(virologic_failure2.rf.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 
#```

### Model Discrimination Analyisis

##```{r}
#source("roc/unbalanced_functions.R")
md2.rf = generateClassificationStats(train_DF, virologic_failure2.rf.cv, cutoff=.4, modelName="RF Model", predictName=predictName)
#plot Sens
md2.rf$roc
md2.rf$calibration
# plot dist
md2.rf$distribution
# Sensitivity Specificty plots
#ggthemr("light")
md2.rf$accuracy_info$plot
#md2.rf$accuracy_info$data
# confusion matrix  
#ggthemr("light")
md2.rf$cm_info$plot
#```


### Confusion Matrix

##```{r}
md2.rf$cm_info$confMatrix

#```



### Relative Importance of Variables
#According to https://dinsdalelab.sdsu.edu/metag.stats/code/randomforest.html, “the mean decrease in Gini coefficient is a measure of how each variable contributes to the homogeneity of the nodes and leaves in the resulting random forest

##```{r}

varImpPlot(virologic_failure2.rf.cv$finalModel, pch = 20, main = "Importance of Variables")
impvar <-round(randomForest::importance(virologic_failure2.rf.cv$finalModel),2)
kable(impvar)

#```



### Error Rate

#This plot shows the class error rates of the random forest model. As the number of trees increases, the error rate approaches zero.

#```{r }


plot(virologic_failure2.rf.cv$finalModel, main = "Error rate of random forest")

#```

### External Validation

#```{r  echo=F, eval=T, results="hide", fig.show='hide'}

mdt2.rf = generateClassificationStats(validate_DF, virologic_failure2.rf.cv, cutoff=.4, modelName="RF Model", predictName=predictName)
#```

#```{r  echo=F, eval=T}
# plot dist
mdt2.rf$distribution
#ggthemr("light")
mdt2.rf$cm_info$plot
mdt2.rf$cm_info$confMatrix
#```





#  BART (Bayesian Additive Regression Trees)
#```{r cache=FALSE, eval=FALSE}
#devtools::install_github("yizhenxu/GcompBART",  dependencies = TRUE)
library(GcompBART) #BART for G computation
### probit BART 

nd = 1000 #number of posterior draws
nb = 100 #number of burn-in
nt = 200 #number of trees

Prior_binary = list(ntrees = nt,
                    kfac = 2,
                    pswap = 0.1, pbd = 0.5, pb = 0.25,
                    alpha = 0.95, beta = 2.0,
                    nc = 100, minobsnode = 10)
Mcmc_binary = list(burn=nb, ndraws = nd)


# formula
fml = "virologic_failure2 ~."

#set.seed(99)
Bmod2 = model_bart(as.formula(fml), data = train_DF, type = "binary",
                   Prior = Prior_binary,
                   Mcmc = Mcmc_binary)

### Diagnostics
# Convergence plots
DiagPlot(Bmod2,1)
# Variable importance plots
DiagPlot(Bmod2,0)

### Training set prediction
TM = Bmod2$samp_y
yM = matrix(rep(if_else(train_DF$virologic_failure2=="Yes",1,0), nd),ncol = nd)

## Training set accuracies
# overall accuracy
mean(TM == yM)

# posterior mode accuracy
yhat = function(vec){
  names(which.max(table(vec))) 
}
TMY = apply(TM, 1, yhat)
mean(TMY == if_else(train_DF$virologic_failure2=="Yes",1,0))

### Test set prediction

PM = predict_bart(Bmod2, test_DF)$samp_y
yM = matrix(rep(if_else(test_DF$virologic_failure2=="Yes",1,0), nd),ncol = nd)

## test set accuracies
# overall accuracy
mean(PM == yM)

# posterior mode accuracy
PMY = apply(PM, 1, yhat)
mean(PMY == test_DF$virologic_failure2)


#```






# SVM

#```{r cache=TRUE}

#set.seed(825)
# parallel processing
cluster <- makePSOCKcluster(detectCores() ) # convention to leave 1 core for OS
registerDoParallel(cluster)


#create tunegrid 
tune_grid.svm <- expand.grid(sigma = c( .02),C = c(0.5))
# train the model 
system.time(virologic_failure2.svmr.cv<- train(virologic_failure2 ~ ., data=train_DF, trControl=train_control, metric = "ROC", method="svmRadial",  verbose = F,  maximize = TRUE))

# shut down cluster
stopCluster(cluster)
registerDoSEQ()

print(virologic_failure2.svmr.cv)



#```

#```{r cache=T}

ggplot(virologic_failure2.svmr.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 

#```

### Model Discrimination Analyisis

##```{r}
#source("roc/unbalanced_functions.R")
md2.svm = generateClassificationStats(train_DF, virologic_failure2.svmr.cv, cutoff=.3, modelName="SVM Model", predictName=predictName)
#plot Sens
md2.svm$roc
md2.svm$calibration
# plot dist
md2.svm$distribution
# Sensitivity Specificty plots
#ggthemr("light")
md2.svm$accuracy_info$plot
#md2.svm$accuracy_info$data
# confusion matrix  
#ggthemr("light")
md2.svm$cm_info$plot
#```

### Confusion Matrix

##```{r}
md2.svm$cm_info$confMatrix
#```

### External Validation

#```{r  echo=F, eval=T, results="hide", fig.show='hide'}

mdt2.svm = generateClassificationStats(validate_DF, virologic_failure2.svmr.cv, cutoff=.3, modelName="SVM Model", predictName=predictName)
#```

#```{r  echo=F, eval=T }
# plot dist
mdt2.svm$distribution
#ggthemr("light")
mdt2.svm$cm_info$plot
mdt2.svm$cm_info$confMatrix


#```


# Comparative Analysis


