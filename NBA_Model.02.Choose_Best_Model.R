#
# Heath Gilham
# NBA predictions for teams
#

# To do
# Figure out what values cause the svm models to stop working
# Track model accuracy through run. For each set of predictors.


# Big To Do
# vary response variable
# Predict continuous variable
# Ensemble methods - voting, stacking, bootstrapping -> voting, boosting - adaboost or others 
# Adaboost, ensemble all models 
# Collect predictions for each model with name of model including parameters
# Generalise function to take in different response variable
# How many regular players/ starters are missing this game?
# More years
# Output probabilities instead and compare the probabilites from both teams playing in the next game
# 3 game averages
# Other variables





#### Functions and source ####

imports = function(){
require(xgboost)
require(readr)
require(stringr)
require(caret)
require(car)
require(randomForest)
require(e1071)
require(extraTrees)
require(class)
require(gbm)
require(fastAdaboost)
require(MASS)
require(neuralnet)
require(rpart)
require(qcc)
  
}


XGB_cont = function(){
  
  
  for(eta in seq(0.06,0.25,0.01)){
    for(max_depth in seq(5,25,2)){
      for(nthread in 2:10){
        for(nround in seq(60,100,10)){
          xgb <- xgboost(data = data.matrix(train[,-1]), 
                         label = train[,1], 
                         eta = eta,
                         max_depth = max_depth, 
                         nround=nround, 
                         subsample = 0.5,
                         colsample_bytree = 0.5,
                         seed = 1,
                         eval_metric = "rmse",
                         objective = "reg:linear",
                         nthread = nthread,
                         verbose=0
          )
          
          pred <- predict(xgb, data.matrix(test[,-1]))
          xgb1 = rmse(test[,1],pred)
          
          if(xgb1<rmse){
            rmse = xgb1
            model = xgb
            bestpred = pred
          }
          
        }
      }
    }
  }
  
  print(cbind(bestpred,test[,1]))
  print(rmse)
  return(model)
}

accuracy = function(name, params,model,prediction,cutoff=FALSE){
  
  if(cutoff){
   prediction = prediction[,2]>cutoff
  }
  
  acc = mean(test[,1]==as.numeric(prediction))
  
  if(acc>rmse){
    rmse <<- acc
    best_model <<- model
    print(rmse)
  }
  
  model_name = name
  if(length(params)!=0){
    for(param in params){
      model_name = paste0(model_name,"_",param)
    }
  }
  
  mod_names <<- c(mod_names,model_name)
  predictions <<- rbind(predictions,c(acc,pred))  
  
  return(acc)
}

timing = function(model_name){
  current_model_name <<- model_name
  print(model_name)
  model_list <<- c(model_list,model_name)
  times <<- c(times,Sys.time())  
  }


  
Best_binary_model = function(){
    rmse = 0
    times <<- c()
    model_list <<- c()
      
    # Logistic regression
    timing("Logistic Regression")
      
    mymodel = glm(WBin~.,data=train,family='binomial')
    pred = predict(mymodel,newdata=test[,-1])
      
    # Decision tree
    timing("Decision Tree")
      
    mymodel = rpart(WBin~.,data=train, method = "class")
    pred = predict(mymodel,test[,-1])
    
    for(i in seq(0.3,0.7,0.01)){
        accuracy(current_model_name,i,mymodel,pred,i)
    }
    
      
    # Random Forest
    if(pca){ 
        timing("Random Forest")
       
        for(j in c(TRUE,FALSE)){
            for( k in 2:5){
                mymodel = randomForest(WBin ~ .,data=train, importance=j,ntree=200,mtry=k)
                pred = predict(mymodel,newdata=test[,-1])
                
                accuracy(current_model_name,c(j,k),mymodel,pred)
            }
        }
       
    }
      
    
      
    # Naive Bayes
    timing("Naive Bayes")
          
    mymodel <- naiveBayes(WBin ~ ., data = train) 
    pred = predict(mymodel,newdata=test[,-1])
          
    accuracy(current_model_name,"",mymodel,pred)
          
    # SVM
      
    # Linear Kernel
    timing("SVM Linear Kernel")
    for( j in c(TRUE,FALSE)){
        for(k in seq(5,12,3)){
            mymodel <- svm(WBin ~ ., data = train, kernel = "linear", cost = k, scale = j)
            pred = predict(svmfit,newdata=test[,-1])
            
            accuracy(current_model_name,c(j,k),mymodel,pred)
      }
    }
    
    # Radial kernel
    timing("SVM Radial Kernel")
    
    for(i in c(TRUE,FALSE)){
        for(cost in 10^seq(1,2,0.2)){
            for(gam in 10^seq(-5,-1,0.25)){
                mymodel <- svm(WBin ~ ., data = train, kernel = "radial", cost = cost, gamma=gam, scale = i) # linear svm, scaling turned OFF
                pred = predict(svmfit,test[,-1])
                
                accuracy(current_model_name,c(i,cost,gam),mymodel,pred)
            }
        }
    }
    # Polynomial kernel
    timing("SVM Polynomial Kernel")
    for(i in c(TRUE,FALSE)){
          for(gam in 10^seq(-5,-4,0.5)){
              for(degree in 2:3){
                  for(coef in 10^seq(-3,0,1)){
                      svmfit = svm(WBin ~ ., data = train, kernel = "polynomial", cost = 0.2, gamma=gam, degree= degree, coef0 = coef, scale = i) # linear svm, scaling turned OFF
                      pred = predict(svmfit,test[,-1])
                      
                      accuracy(current_model_name,c(i, cost, gam, degree),mymodel,pred)
                      
            }
        }}
    }
    
    
    # Sigmoid kernel - to do
    timing("SVM Sigmoid Kernel")
    for(i in c(TRUE,FALSE)){
        for(cost in 10^seq(0.2,0.6,0.4)){
            for(gam in 10^seq(-5,-3,1)){
                for(coef in 10^seq(-3,-1,1)){
                    mymodel <- svm(WBin ~ ., data = train, kernel = "sigmoid", cost = cost, gamma=gam, coef0 = coef, scale = i) # linear svm, scaling turned OFF
                    pred = predict(svmfit,test[,-1])
                    
                    accuracy(current_model_name, c(i, cost, gam, coef), mymodel, pred)
                }
            }
        }
    }
    
      
      
      
      
      
    # KNN 
    timing("KNN")
    
    for(k in 2:15){
        pred = knn(train = train[,-1],test = test[,-1],cl = train[,1],k=k)
        accuracy(current_model_name,k,"KNN",pred)
    }
      
    
      
    # Linear Discriminant Analysis      
    timing("LDA")
    
    mymodel <- lda(WBin ~ ., data = train) 
    pred = predict(ldafit,test[,-1])
    
    for(i in seq(0.3,0.7,0.01)){
        accuracy(current_model_name,i,mymodel,pred$posterior[,2],i)
    }
    
    times <<- c(times,Sys.time())
    model_list <<- c(model_list,"End")
          
    print(rmse)
    return(model)
}

Best_binary_model_caret = function(){
  rmse = 0
  #cvcontrol <- trainControl(method='cv', number=4)
  tuning_LogitBoost   <-  expand.grid(nIter = seq(20,60,10))
  tuning_rf           <-  expand.grid(mtry = seq(20,ceiling(ncol(train)/10),5))
  tuning_extraTrees   <-  expand.grid(mtry = seq(20,ceiling(ncol(train)/10),5), 
                                      numRandomCuts = seq(20,ceiling(ncol(train)/10),5)
                                      )
  tuning_nb           <-  expand.grid(fL = 10^seq(-2,2,1), 
                                      usekernel = c('linear'),
                                      adjust = 10^seq(-2,2,1)
                                      )
    
  tuning_nnet         <-  expand.grid(size = seq(1,5,1), 
                                      decay = seq(0.1,0.5,0.1)
                                      )
  tuning_neuralnet    <-  expand.grid(layer1 = seq(2,5,1), 
                                      layer2 = seq(2,5,1),
                                      layer3 = seq(2,5,1)
                                      )
  
  tuning_knn          <-  expand.grid(k = 3:15)
  tuning_lda          <-  expand.grid()
  svm_costs = 10^seq(-3,2,1)
  tuning_svmLinear  <-  expand.grid(C = svm_costs)
  tuning_svmPoly    <-  expand.grid(C = svm_costs, 
                                      scale = 10^seq(-2,2,1),
                                      degree = 2:5
                                      ) 
  tuning_svmRadial  <-  expand.grid(C = svm_costs, 
                                     sigma = 10^seq(-2,2,1)
                                     ) 
  
  tuning_xgbtree      <-  expand.grid(nrounds = 100,
                                      max_depth = seq(7,25,2),
                                      eta = seq(0.02,0.25,0.05),
                                      gamma = 10^seq(-2,1,1),
                                      colsample_bytree = c(0.5,0.55),
                                      min_child_weight = c(0.5,0.55),
                                      subsample = c(0.5,0.55)
                                      )
  #tuning_adaboost     <-  expand.grid(nIter = seq(60,100,10),
  #                                    method = 
  #                                    )
  
  
  the_models = c("LogitBoost","rf", "extraTrees","nb","nnet","neuralnet",
                 "knn","lda","lssvmLinear","lssvmPoly","lssvmRadial","xgbtree") #,"adaboost")
  
  for(method in the_models){
      timing(method)
        
      mymodel <- train(WBin~.,data = train, method=method, metric = "Accuracy", tuneGrid = get(paste0("tuning_",method)))
      pred = predict(mod1,newdata=test[,-1])
      
      accuracy(current_model_name,"",mymodel,pred)
  
  }
  

  

  # SVM
  
  # Linear Kernel
  for( j in c(TRUE,FALSE)){
    for(k in seq(5,15,5)){
      svmfit <- svm(WBin ~ ., data = train, kernel = "linear", cost = k, scale = j) # linear svm, scaling turned OFF
      pred = predict(svmfit,newdata=test[,-1])
      svm = mean(test[,1]==pred)
      
      
  }}
  
  # Radial kernel
  for(i in c(TRUE,FALSE)){
    for(cost in 10^seq(1,2,0.2)){
      for(gam in 10^seq(-5,-1,0.25)){
        svmfit <- svm(WBin ~ ., data = train, kernel = "radial", cost = cost, gamma=gam, scale = i) # linear svm, scaling turned OFF
        pred = predict(svmfit,test[,-1])
        svm = mean(test[,1]==pred)
        
       
    }}}
  # Polynomial kernel - to do
  for(i in c(TRUE,FALSE)){
    for(cost in 10^seq(0.2,0.8,0.2)){
      for(gam in 10^seq(-5,-2,0.5)){
        for(degree in 3:4){
          for(coef in 10^seq(-3,1,1)){
            svmfit <- svm(WBin ~ ., data = train, kernel = "polynomial", cost = cost, gamma=gam, degree= degree, coef0 = coef, scale = i) # linear svm, scaling turned OFF
            pred = predict(svmfit,test[,-1])
            svm = mean(test[,1]==pred)
            
            
        }}
    }}}
  
  
  # Sigmoid kernel - to do
  for(i in c(TRUE,FALSE)){
    for(cost in 10^seq(0.2,1.2,0.4)){
      for(gam in 10^seq(-5,-3,1)){
        for(coef in 10^seq(-3,1,1)){
          svmfit <- svm(WBin ~ ., data = train, kernel = "sigmoid", cost = cost, gamma=gam, coef0 = coef, scale = i) # linear svm, scaling turned OFF
          pred = predict(svmfit,test[,-1])
          svm = mean(test[,1]==pred)
          
          
      }}
  }
  }

  
  print(rmse)
  return(model)
  }
    
    

XGB_binary = function(){
  timing("XGBoost")
  
  train$WBin = as.numeric(train$WBin)-1
  for(eta in seq(0.02,0.25,0.02)){
    for(max_depth in seq(5,25,2)){
      for(nthread in 4:10){
          mymodel <- xgboost(data = data.matrix(train[,-1]), 
                         label = train[,1], 
                         eta = eta,
                         max_depth = max_depth, 
                         nround=100, 
                         subsample = 0.5,
                         colsample_bytree = 0.5,
                         seed = 1,
                         eval_metric = "error",
                         objective = "binary:logistic",
                         nthread = nthread,
                         verbose=0
          )
          
          pred <- predict(xgb, data.matrix(test[,-1]))
          accuracy(current_model_name,"",mymodel,pred,0.5)
          
          
      }
  }
  }
  
  
# Adaboost  
  timing("AdaBoost")
  k = 100
  adab <- adaboost(WBin ~., data = train, nIter = k)
  pred <- predict(adab, test[,-1])
  
  for(cut in seq(0.3,0.7,0.01)){
    accuracy(current_model_name,"",mymodel,pred$prob[,2],cut)
  }
  
  mod_perf = cbind(mod_names,predictions)
      
  times <<- c(times,Sys.time())
  model_list <<- c(model_list,"End")
  print(rmse)
  return(model)
  
  }
  


####  The code ####
imports()
source("C:/Users/Heathyboy/OneDrive/Data/NBA Team Analysis/NBA analysis.R")
source("C:/Users/Heathyboy/OneDrive/Data/General Functions.R")
#source("~/Dropbox/My R Projects/Code/NBA analysis.R"); Rstudioserveron = TRUE

run_NBA = function(runpca=FALSE){
all <- games_w_averages[c(39,42:ncol(games_w_averages))]
# First column is response variable
all[,1] <- as.factor(all[,1])
set.seed(100)


s = 1:(round(0.5*nrow(all)))
t = ((round(0.5*nrow(all)))+1):(round(0.6*nrow(all)))
train <- all[s,]
test <- all[t,]
train_ans <- train[,1]
test_ans <- test[,1]


# Adjustment
predictorcols = setdiff(names(train),"WBin")
pcaDone <- prcomp(train[,predictorcols]) # PCA
cumprop = summary(pcaDone)$importance[3,]
cols = 1:(length(cumprop[cumprop<=0.99])+1)

# http://rstatistics.net/principal-component-analysis/#at_pco=smlwn-1.0&at_si=5892c12138c93ea4&at_ab=per-2&at_pos=0&at_tot=1
#Deciding on the number of principal components
#screeplot(pcaDone, type="lines") # screeplot
#pareto.chart(pcaDone$sdev^2, ylab="Variances")  # plot pareto chart

PCAdatasettrain = predict(pcaDone, train[,predictorcols])
PCAdatasettrain = as.data.table(PCAdatasettrain)
PCAdatasettrain = PCAdatasettrain[,cols,with=FALSE]
PCAdatasettrain = cbind(train,PCAdatasettrain)

PCAdatasettest = predict(pcaDone, test[,predictorcols])
PCAdatasettest = as.data.table(PCAdatasettest)
PCAdatasettest = PCAdatasettest[,cols,with=FALSE]
PCAdatasettest = cbind(train,PCAdatasettest)

impdatasettrain = train
impdatasettest = test

rfmodel = randomForest(WBin ~ .,impdatasettrain,importance=TRUE, ntree = 500); rfmodel
varImpPlot(rfmodel)
imp = varImp(rfmodel)
pred = predict(rfmodel,impdatasettest)
accuracy = mean(pred==test[,"WBin"])

usefulcols = rownames(imp)[imp[,1] > quantile(imp[,1], 0.9)]

impdatasettrain = impdatasettrain[,c("WBin",usefulcols)]
impdatasettest = impdatasettest[,c("WBin",usefulcols)]

rfmodel = randomForest(WBin ~ .,impdatasettrain,importance=TRUE, ntree = 500); rfmodel
varImpPlot(rfmodel)
imp = varImp(rfmodel)
pred = predict(rfmodel,impdatasettest)
accuracy = mean(pred==test[,"WBin"])

impdatasettrain = impdatasettrain[,c(ncol(impdatasettrain),1:(ncol(impdatasettrain)-1))]
impdatasettest = impdatasettest[,c(ncol(impdatasettest),1:(ncol(impdatasettest)-1))]

train = impdatasettrain
test = impdatasettest

# Models
mod_perf = data.frame()
predictions = data.frame()
mod_names = c()

Best_binary_model()
XGB_binary()

}

run_NBA()































xgboost_model = xgboost(data = my_dataset, 
                        label = my_labels, 
                        eta = 0.01,
                        max_depth = 4, 
                        nround=100, 
                        subsample = 0.5,
                        colsample_bytree = 0.5,
                        seed = 1,
                        eval_metric = "error",
                        objective = "binary:logistic",
                        nthread = 4,
                        verbose=0
                        )

my_predictions = predict(xgboost_model,my_new_dataset)


mod1 = rpart(WBin~.,data=train, method = "class")

pred = predict(mod1,test[,-1])

for(i in seq(0.3,0.7,0.01)){
svm = mean(test[,1]==as.numeric(pred[,2]>i)); print(svm)
}



# Neural Network
'''
require(neuralnet)
#train$WBin = as.numeric(train$WBin)
abc = as.formula(paste("WBin ~ ",paste(names(train)[-1],collapse = "+"))) # Need to setup own formula here

for( j in c(4)){
for(k in c(4)){
nn = train(abc,train,method = "nnet",trace=FALSE, tuneGrid = expand.grid(.size=c(j,k),.decay = c(0,0.001,0.1)))
pred = predict(nn,test[,-1]) ; pred = as.numeric(pred)-1
svm = mean(test[,1]==pred)

if(svm>rmse){
rmse = svm
model = svmfit
print(rmse) 
}
mod_names <<- c(mod_names,c(paste0("Neural_Network_",j,"_",k)))
predictions <<- rbind(predictions,c(svm,pred))
}
}
train$WBin = as.factor(train$WBin)
'''

# Extreme Random Trees
for(ntree in c(200)){
  for( cuts in 2:4){
    for( k in 4:6){
      rf = extraTrees(train[,-1],train[,1], numRandomCuts = cuts,ntree=ntree,mtry=k)
      pred = predict(rf,newdata=test[,-1])
      rf1 = mean(test[,1]==(as.numeric(pred)-1))
      
      if(rf1>rmse){
        rmse = rf1
        model = rf
        print(rmse)
      }
      mod_names <<- c(mod_names,c(paste0("extreme_forest_",ntree,"_",cuts,"_",k)))
      predictions <<- rbind(predictions,c(rf1,pred))
    }
  }
}