
# increase memory
options(java.parameters = "-Xmx15g")
# seed 
SEED=7
set.seed(SEED) 

# Install Packages
packages =c( "dplyr","car","sjPlot","sjmisc","sjlabelled","ggpubr","ggpmisc","gridExtra","stargazer","e1071","jtools",
             "effects","multcompView","ggplot2","ggrepel","MASS","broom","ggcorrplot","leaps","relaimpo","olsrr","reshape2",
             "ROCR","arm","foreign","nnet","VGAM","ordinal","ModelGood","InformationValue","rms","texreg","knitr","pastecs",
             "xtable","easyGgplot2","tidyverse","ISLR","randomForest","caret","rpart","rpart.plot","naniar","caret","C50", "plotROC", 
	     "skimr", "lattice", "ggthemes", "caretEnsemble", "Hmisc", "DMwR",
             "doParallel","bartMachine","xgboost","PerformanceAnalytics", "knitr", "kableExtra")

install.packages(packages, dependencies = TRUE, repos='http://cran.rstudio.com/')
sapply(packages, require, character.only = TRUE)

# Prevent Override
select = dplyr::select; summarize = dplyr::summarize; rename = dplyr::rename; mutate = dplyr::mutate;

# function to generate model evaluation metrics: roc, auc, sens, spec, calibration
# also prints model giscrimination analyisis and calibration plots
generateClassificationStats = function(dataset, carretModel, cutoff=.5, modelName, predictName="virologic_failure"){
  
  dataset$prediction <- predict(carretModel, newdata = dataset, type = "prob" )$Yes
  # convert yes virologic_failure to 1,0
  dataset$actual <- relevel( as.factor( ifelse(dataset[[predictName]]=="Yes","1","0")), "1")
  
  # distribution of the prediction score grouped by known outcome
  distribution = ggplot( dataset, aes( prediction, ..density.., color = as.factor(actual), linetype = as.factor(actual) ) ) + 
    geom_density( size = 1) +
    ggtitle( "Prediction Distribution" ) + xlab("Predicted Pr(Y=1|V)") + ylab("Density")+
    guides(colour = guide_legend(override.aes = list(linetype = c(1,2))))+
    scale_linetype(guide = FALSE) +
    scale_color_economist( name = "Virologic Failure", labels = c(  "Yes", "No" ) ) + 
    theme_economist()
  
  accuracy_info <- TrainAccuracyCutoffInfo( train = dataset, test = dataset, cutoff=seq( 0, 1, by = .1 ),
                                            predict = "prediction", actual = "actual" )
  
  # visualize .6 cutoff (lowest point of the previous plot)
  cm_info <- ConfusionMatrixInfo( data = dataset, predict = "prediction", 
                                  actual = "actual", cutoff = cutoff)
  
  # roc
  roc <- pROC::roc( relevel(as.factor(dataset$actual), "1" ) ~ dataset$prediction, plot = TRUE, print.auc = TRUE,
                    thresholds="best", print.thres="best", print.auc.y=4,	conf.level=0.95, boot.n=1000,
                    main=modelName, percent=TRUE, ci=F, of="thresholds"	
  )	
  
  # calibration curve
  cal_plot_data = calibration(actual ~ prediction, 
                              data = dataset, class = 1)$data
  
  calibration =ggplot() + xlab("Bin Midpoint") +
    geom_line(data = cal_plot_data, aes(midpoint, Percent),
              color = "#F8766D") +
    geom_point(data = cal_plot_data, aes(midpoint, Percent),
               color = "#F8766D", size = 3) +
    geom_line(aes(c(0, 100), c(0, 100)), linetype = 2, 
              color = 'grey50') + 
    ggtitle( "Calibration Curve" )+
    theme_economist()
  
  
  return( list( cm_info  = cm_info, 
                accuracy_info 	  = accuracy_info, 
                distribution   = distribution, 
                roc         = roc,
                calibration=calibration
  ) 
  )
}


response_var= c('virologic_failure1',"virologic_failure2")

# binary variables
binary_var = c(
  'male', 'status_disclosed','on_contraceptive','on_health_cover','on_tb_ipt_regimen','sti_symptoms_status','tb_symptoms_status',
  'referral_ordered','referred_to_phdp','needs_fam_tx_support','been_hospitalized','alcohol_consumer', 'cig_smoker','cxr_code_labs_status',
  'general_clinical_exam', 'skin_clinical_exam','lymph_nodes_clinical_exam','respiratory_clinical_exam', 'virologic_failure1',
  'abdominal_clinical_exam','urogenital_clinical_exam','underweight_status', 'high_bp', 'low_bp', 'abnormal_oxy_sat', 'fever', 'mtrh_clinic'
)

# categorical variables
categorical_var =c('baseline_arv_line')

# numerical variables
numerical_var =c('baseline_age', 'bmi', 'days_since_last_vl',"prop_days_on_arvs","prop_defaulted_visits", "num_encounters","vl_count_1_log")

# ordinal variables
ordinal_var =c('baseline_who_stage')

# Data Prep
vl.df =readRDS(file = "/results-2020/dataset.rds") 

# Response and predictors
features = c(numerical_var,ordinal_var,binary_var,categorical_var)
target = "virologic_failure2"


# Missing Data
gg_miss_var(vl.df )+ labs(title = "Frequency of Missingness")


# create train/validation sets
clean.df =vl.df %>%filter(!is.na(vl_count_1))%>%filter(!is.na(vl_count_2)) %>%select(features,target)#%>%filter(!is.na(bmi))
train_index <- createDataPartition(clean.df$virologic_failure2, p=.8,
                                   list=FALSE,
                                   times = 1)

train_DF <- clean.df[train_index,]
validate_DF <- clean.df[-train_index,]

# folds
nfolds = 10
fold = createFolds(train_DF$virologic_failure2, nfolds) # have each folds balance vl class
# define training control
train_control <- caret::trainControl(
  index = fold,
  #sampling = "smote",
  verboseIter = F, # no training log
  allowParallel = TRUE, # FALSE for reproducible results 
  summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE
  # ,preProc = c("center", "scale")
)



# Logistic Regression Model 
trainOLSL = function(train_DF,validate_DF,features,target,train_control, cutoff=.25){
          virologic_failure2.logit.cv <- train(target ~features,data=train_DF, 
                                               method = "glm",#method = "glmStepAIC",  direction ="both",
                                               trControl = train_control,metric = "Sens")
          summary(virologic_failure2.logit.cv$finalModel) # print cv scores
          
          ### External Validation
          generateClassificationStats(validate_DF, virologic_failure2.logit.cv, cutoff=.25, 
                                      modelName="virologic_failure2.logit.cv", predictName=target)
}


# ElasticNET
trainENET = function(train_DF,validate_DF,features,target,train_control, cutoff=.25){
          grid.pls=expand.grid(alpha = 0.1:.5,
                                    lambda = seq(0.01,0.1,by = 0.01) )
          
          virologic_failure2.logit.rl.cv <-train(target ~features,data=train_DF, method = "glmnet", 
                                                 trControl = train_control,metric = "Sens",
                                                 tuneGrid = tune_grid.pls)
          
          
          # print cv scores
          ggplot(virologic_failure2.logit.rl.cv)
          plot(virologic_failure2.logit.rl.cv$finalModel, xvar = "lambda")
          abline(v = log(virologic_failure2.logit.rl.cv$bestTune$lambda), col = "red", lty = "dashed")

          ### External Validation
          generateClassificationStats(validate_DF, virologic_failure2.logit.rl.cv, cutoff=.25, 
                                      modelName="virologic_failure2.logit.rl.cv", predictName=target)
}

# KNN
trainKNN = function(train_DF,validate_DF,features,target,train_control, cutoff=.25){
        cluster <- makePSOCKcluster(detectCores() ) # convention to leave 1 core for OS
        registerDoParallel(cluster)
        
        tune_grid.knn = expand.grid(k = (20:40))
        system.time(virologic_failure2.knn.cv<- train(target~features, data=train_DF,  trControl=train_control,  method="knn", 
                                                      tuneGrid = tune_grid.knn,
                                                      metric = "Sens",maximize = TRUE))
        
        # shut down cluster
        stopCluster(cluster)
        registerDoSEQ()
        
        # print cv scores
        print(virologic_failure2.knn.cv)
        ggplot(virologic_failure2.knn.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 
        
        
        ### External Validation
        generateClassificationStats(validate_DF, virologic_failure2.knn.cv, cutoff=.25, modelName="knn", predictName=target)
}

# Classification and Regression Trees (CART)
trainCART = function(train_DF,validate_DF,features,target,train_control, cutoff=.25){
          tune_grid.cart = expand.grid(cp = seq(0,0.1,by = 0.01))
          virologic_failure2.dt.cv<- train(target~features, data=train_DF,  trControl=train_control,  method="rpart", 
                                           tuneGrid =tune_grid.cart, parms = list(split = "gini"),
                                           metric = "Sens",maximize = TRUE)
          
          # print cv scores & Tuning and Tree Diagram
          print(virologic_failure2.dt.cv)
          ggplot(virologic_failure2.dt.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 
          rpart.plot(virologic_failure2.dt.cv$finalModel, main="Decsion Tree", fallen.leaves=F, extra=104, box.palette="GnBu")
          
          
          ### External Validation
          generateClassificationStats(validate_DF, virologic_failure2.dt.cv, cutoff=.25, modelName="CART", predictName=target)
}

# XGBOOST
trainXGB = function(train_DF,validate_DF,features,target,train_control, cutoff=.25){
          #https://xgboost.readthedocs.io/en/latest/parameter.html
          # note to start nrounds from 200, as smaller learning rates result in errors so
          # big with lower starting points that they'll mess the scales
          nrounds <- 1000
          tune_grid.xgb <- expand.grid(
            nrounds = seq(from = 100, to = nrounds, by = 50),
            eta =c( .2,.1,.4,.8,.9, .98,1.0
            ), # learning rate default=0.3 range = [0,1], prevents overfitting
            max_depth = c( 2,3,4, 5,6 
            ), # default 6, incr this value will make the model overfitt and complex
            gamma = c(0), # min loss reduction required to make a further partition on a leaf. range: [0,inf] def 0
            colsample_bytree =c(1,0), # 1 means all columns are used in each decision tree. range [0,1]
            min_child_weight = c(1,2,4,6,8), # default 1, range: [0,inf]
            subsample = c(0,1,.5) # range: (0,1] default 1; 0.5 means xgb will randomly sample half of the training data - prevents overfitting. 
          )
          
          
          # train the model # try neg_log_loss,error
          virologic_failure2.xgb.cv<- train(target~features, data=train_DF, tuneGrid=tune_grid.xgb, trControl=train_control, 
                                            metric = "Sens" ,method="xgbTree",  verbose = T,  maximize = TRUE)
          ggplot(virologic_failure2.xgb.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 
          
          
          ### Relative Importance
          model = xgb.dump(virologic_failure2.xgb.cv$finalModel, with.stats=TRUE)
          importance_matrix = xgb.importance( model=virologic_failure2.xgb.cv$finalModel)
          print(xgb.plot.importance(importance_matrix, top_n = 20 )) 
          
          
          ### External Validation
          generateClassificationStats(validate_DF, virologic_failure2.xgb.cv, cutoff=.25, modelName="XGBoost", predictName=target)
}

# GBM
trainGBM = function(train_DF,validate_DF,features,target,train_control, cutoff=.25){
        cluster <- makePSOCKcluster(detectCores() ) # convention to leave 1 core for OS
        registerDoParallel(cluster)
        #define tunegrid
        tune_grid.gbm <- expand.grid(
          interaction.depth = c(1,3,5,7,9,10), 
          n.trees = c(100, 300, 400, 1000), 
          shrinkage = 0.1, # prevents overfitting
          n.minobsinnode = 20)
        
        # train the model # try neg_log_loss,error
        system.time(virologic_failure2.gbm.cv<- train(target~features, data=train_DF, tuneGrid=tune_grid.gbm, trControl=train_control,
                                                      metric = "Sens" ,method="gbm",  verbose = F,  maximize = TRUE))
        
        stopCluster(cluster) # shut down cluster
        registerDoSEQ()
        
        # print cv
        virologic_failure2.gbm.cv
        ggplot(virologic_failure2.gbm.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 
        ggplot(varImp(virologic_failure2.gbm.cv), top=20) # Relative Importance
        
        ### External Validation
        generateClassificationStats(validate_DF, virologic_failure2.gbm.cv, cutoff=cutoff, modelName="GBM Model", predictName=target)
}


# Random Forest
trainRF = function(train_DF,validate_DF,features,target,train_control, cutoff=.25){
        cluster <- makePSOCKcluster(detectCores() ) # convention to leave 1 core for OS
        registerDoParallel(cluster)
        
        #create tunegrid with 15 values from 1:15 for mtry to tunning model. 
        #Our train function will change number of entry variable at each split according to tunegrid. 
        tunegrid.rf <- expand.grid(.mtry=5:15)
        system.time(
          virologic_failure2.rf.cv<- train(target~features, data=train_DF, tuneGrid=tunegrid.rf, ntrees=500,
                                    trControl=train_control, metric = "Sens", method="rf",  verbose = F,  maximize = TRUE)
        )
        
        # shut down cluster
        stopCluster(cluster)
        registerDoSEQ()
        
        plot(virologic_failure2.rf.cv$finalModel, main = "Error rate of random forest")
        varImpPlot(virologic_failure2.rf.cv$finalModel, pch = 20, main = "Importance of Variables")
        impvar <-round(randomForest::importance(virologic_failure2.rf.cv$finalModel),2)
        kable(impvar)
        
        ### External Validation
        generateClassificationStats(validate_DF, virologic_failure2.rf.cv, cutoff=cutoff, modelName="RF Model", predictName=target)
}

#  BART (Bayesian Additive Regression Trees)
trainBART = function(train_DF,validate_DF,features,target,train_control, cutoff=.25){

            devtools::install_github("yizhenxu/GcompBART")
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
            fml = "target ~features"
            
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
}


# SVM
trainSVM = function(train_DF,validate_DF,features,target,train_control, cutoff=.25){
          cluster <- makePSOCKcluster(detectCores() ) # convention to leave 1 core for OS
          registerDoParallel(cluster)
          
          tune_grid.svm <- expand.grid(sigma = c( .01,.02,.03,.04,.05),C = c(.1,.3,.5,.7,.9)) #create tunegrid 
          system.time(virologic_failure2.svmr.cv<- train(target~features, data=train_DF, trControl=train_control, 
                                                         metric = "ROC", method="svmRadial",  verbose = F,  maximize = TRUE))
          
          # shut down cluster
          stopCluster(cluster)
          registerDoSEQ()
          
          print(virologic_failure2.svmr.cv)
          ggplot(virologic_failure2.svmr.cv)+ ggtitle("Tuning Graph")+theme_minimal()+ theme(plot.title = element_text(hjust = 0.5)) 
          
          ### External Validation
          generateClassificationStats(validate_DF, virologic_failure2.svmr.cv, cutoff=.25, modelName="SVM Model", predictName=target)
}

# SL
trainSL = function(train_DF,validate_DF,features,target,train_control, cutoff=.25){
        library(h2o)
        h2o.init()
        train_DF$fold = NA
        train_DF$virologic_failure2= as.factor(train_DF$virologic_failure2)
        validate_DF$virologic_failure2= as.factor(validate_DF$virologic_failure2)
        
        # create h2o k-fold columns consistent with base learner
        for (f in seq_along(fold)){
          for (i in fold[[f]]){
            train_DF[i,]$fold=f
          }
        }
        
        # convert to h2o object
        train.h2o= as.h2o(train_DF)
        validate.h2o= as.h2o(validate_DF)
        
        ## Train & Cross-validate a GBM
        base_gbm <- h2o.gbm(x =  features,
                          y = target,
                          training_frame = train.h2o,
                          fold_column = 'fold',
                          distribution = "bernoulli",
                          ntrees = 50,
                          max_depth = 3,
                          min_rows = 2,
                          learn_rate = 0.1,
                          keep_cross_validation_predictions = TRUE,
                          keep_cross_validation_fold_assignment = TRUE,
                          seed = SEED)
        
        ## Train & Cross-validate RF
        base_rf <- h2o.randomForest(x = features,
                                  y = target,
                                  training_frame = train.h2o,
                                  fold_column = 'fold',
                                  ntrees = 30,
                                  keep_cross_validation_predictions = TRUE,
                                  keep_cross_validation_fold_assignment = TRUE,
                                  seed = SEED)
        
        ## Train & Cross-validate ENET
        base_glm <- h2o.glm(x = features,
                          y = target,
                          training_frame = train.h2o,
                          fold_column = 'fold',
                          family = "binomial",
                          alpha = 0, #0.0 produces ridge regression
                          # lambda = 0,
                          keep_cross_validation_predictions = TRUE,
                          keep_cross_validation_fold_assignment = TRUE,
                          seed = SEED)
        
        ## Train SL
        SL <- h2o.stackedEnsemble(x = features,
                                        y = target,
                                        training_frame = train.h2o,
                                        model_id = "SL",
                                        base_models = list(base_gbm@model_id,
                                                           base_rf@model_id,
                                                           base_glm@model_id,
                                                           base_xgb@model_id
                                    ))
        ## External Validation
        perf <- h2o.performance(SL, newdata = validate.h2o)
        ensemble_auc_test <- h2o.auc(perf)
        print(sprintf("Ensemble Test AUC: %s", ensemble_auc_test))
}


## Train Models
trainRF(train_DF,validate_DF,features,target,train_control, .25)
trainRF(train_DF,validate_DF,features,target,train_control,.25)
trainXGB(train_DF,validate_DF,features,target,train_control,.25)
trainOLSL(train_DF,validate_DF,features,target,train_control,.25)
trainENET(train_DF,validate_DF,features,target,train_control,.25)
trainKNN(train_DF,validate_DF,features,target,train_control,.25)
trainCART(train_DF,validate_DF,features,target,train_control,.25)
trainSVM(train_DF,validate_DF,features,target,train_control,.25)
trainSL(train_DF,validate_DF,features,target,train_control,.25)

## Comparative Analysis


