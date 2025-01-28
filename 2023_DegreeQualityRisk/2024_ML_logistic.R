library(caret)
library(mice)

#### Prepare your data:
    #Factor to predict ("VD"). Two classes
    #Predictors to use.
    #Dummy factors

    #dataBachelor <- x
    #dataMasters  <- y

#### Train parameters
    trControlup = trainControl(
      method = "repeatedcv",             
      number = 5,                   
      repeats = 15,                      
      savePredictions = "final",      
      returnResamp = "final",     
      classProbs = TRUE,        
      summaryFunction = twoClassSummary,      
      sampling="up"
      )
  
#### Training: bachelor model

    #Create sample for split train set
    set.seed(321)
    training.samples <- createDataPartition(y = dataBachelor$VD, times=1, p = 0.75, list = FALSE)
    
    #Missing imputation
    ignore_mice <- rep(FALSE,nrow(dataBachelor))
    ignore_mice[-training.samples] <- TRUE
    set.seed(321)
    mice_mod <- mice(dataBachelor[, !names(dataBachelor) %in% c('VD')], m=10, method='rf', ignore=ignore_mice) 
    mice_output <- complete(mice_mod)
    str(mice_output)
    for (i in names(mice_output)){
      dataBachelor[,i] <- mice_output[,i]
    }
    
    #Split train set
    train.dataB <- dataBachelor[training.samples, ]
    test.dataB <- dataBachelor[-training.samples, ]
    
    #Train
    logmodelupBcontrol <-caret::train(
      VD ~ .,               
      data = train.dataB,         
      method = "glm",  
      family = "binomial",
      preProcess="scale",
      metric = "Sens",   
      trControl = trControlup)



#### Training: master model
    
    #Create sample for split train set
    set.seed(321)
    training.samples<- createDataPartition(y = dataMasters$VD, times=1, p = 0.75, list = FALSE)
    
    #Missing imputation
    ignore_mice <- rep(FALSE,nrow(dataMasters))
    ignore_mice[-training.samples] <- TRUE
    set.seed(321)
    mice_mod <- mice(dataMasters[, !names(dataMasters) %in% c('VD')], m=10, method='rf', ignore=ignore_mice) 
    mice_output <- complete(mice_mod)
    str(mice_output)
    for (i in names(mice_output)){
      dataMasters[,i] <- mice_output[,i]
    }
    
    #Split train set
    train.dataM <- dataMasters[training.samples, ]
    test.dataM <- dataMasters[-training.samples, ]
    
    #Train
    logmodelupMcontrol <-caret::train(
      VD ~ .,               
      data = train.dataM,               
      method = "glm",  
      family = "binomial",
      preProcess="scale",
      metric = "Sens",   
      trControl = trControlup)
    

#### Evaluation
    resamples <- resamples(list("Bachelor" = logmodelupBcontrol,
                                "Master" = logmodelupMcontrol))
    summary(resamples)
    bwplot(resamples)

#### Confusion matrix for test set and threshold change
    predict_thresholdB <- ifelse(predict(logmodelupBcontrol, newdata =test.dataB , type = "prob")[1] > 0.40, "Class1", "Class2")
    cmBachelor <- caret::confusionMatrix(data = as.factor(predict_thresholdB), reference = test.dataB$VD, mode="everything")
    cmBachelor
    
    predict_thresholdM <- ifelse(predict(logmodelupMcontrol, newdata =test.dataM , type = "prob")[1] > 0.40, "Class1", "Class2")
    cmMaster <- caret::confusionMatrix(data = as.factor(predict_thresholdM), reference = test.dataM$VD, mode="everything")
    cmMaster





