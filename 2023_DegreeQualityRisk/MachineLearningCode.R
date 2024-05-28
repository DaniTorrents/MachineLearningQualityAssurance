

# Packages
library(dplyr)
library(ggplot2)  
library(plotly)
library(visdat)
library(naniar)
library(kableExtra)
library(tidymodels)
library(tidyflow)
library(rsample)
library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)
library(vip)
library(pdp)
library(baguette)
library(e1071)
library(imbalance)
library(mlr)
library(ROSE)

# Oversampling on the data based on acreditation results (Success)
set.seed(333)

task <- makeClassifTask(data = Data, target = "AcredResults", positive = "Success")
r <- round(table(Data$AcredResults)[2]/table(Data$AcredResults)[1]/2) + 1
task_over <- mlr::oversample(task, rate = r)
data_oversample <- getTaskData(task_over)

#--------------
filestrain <- sample(1:nrow(Data), round(nrow(Data)* 0.8), FALSE) ## Create a train set with 80% the data
dftrain <- Data[filestrain, ]
dftest <- Data[-filestrain, ]

dftrain_over <- ovun.sample(AcredResults ~ ., data = Data, method = "over")$data

#### NTREE: 5, 10, 50, 100, 500, 1000
#### MTRY: 1, 3, 5, 9
#### Bootstrap TRUE, FALSE

trees <- c(5, 10, 50, 100, 500, 1000)
variables <- c(1, 3, 5, 9)
boot <- c(TRUE, FALSE)

## Use expand.grid to create a table with all possible combinations

parametres <- expand.grid(arbres = arbres, variables = variables, boot = boot)

parametres$resultats <- NA 

## Model with dftrain without oversampling balancejar

for (i in 1:nrow(parametres)) {
  mod <- randomForest(AcredResults ~ ., data = dftrain[,-ncol(dftrain)],
                      ntree = parametres$arbres[i], # number of trees
                      mtry = parametres$variables[i], # Number of variables
                      replace = parametres$boot[i]) # Bootstrap
  prediction <- predict(mod, dftest, type = "class")
  parametres$resultats[i] <- round(mean(dftest$AcredResults == prediction),2) ## Accuracy
}

parametres_over <- expand.grid(arbres = arbres, variables = variables, boot = boot)

parametres_over$resultats <- NA 


## Model with dftrain with oversampling

for (i in 1:nrow(parametres)) {
  mod_over <- randomForest(AcredResults ~ ., data = dftrain_over[,-ncol(dftrain_over)],
                           ntree = parametres_over$arbres[i],
                           mtry = parametres_over$variables[i],
                           replace = parametres_over$boot[i])
  prediction_over <- predict(mod_over, dftest, type = "class")
  parametres_over$resultats[i] <- round(mean(dftest$AcredResults == prediction_over),5)
}


## Which combinations has better accuracy rate?

mesprecis <- parametres[which.max(parametres$resultats),]

## Idem (oversampling)

mesprecis_over <- parametres_over[which.max(parametres_over$resultats),]

row.names(mesprecis) <- NULL

row.names(mesprecis_over) <- NULL

cat("**Without oversampling** \n")

formattable(mesprecis)

cat("**With oversampling** \n")

formattable(mesprecis_over)

if (mesprecis$resultats>mesprecis_over$resultats) {
  cat("In this case, the model with a better accuracy rate is the one that uses a data matrix without oversampling. **Therefore, we select the model without oversampling**.")
} else if (mesprecis$resultats==mesprecis_over$resultats) {
  cat("In this case, both models (with oversampled data and without) have the same performance. **Therefore, we select the model without oversampling**.")
} else {
  cat("In this case, the model with a better accuracy rate is the one that uses an oversampled data matrix. **Therefore, we select the model with oversampling**.")
  
  parametres <- parametres_over
}

## Explore accuracy rates for parametres

nomparametres <- c(colnames(parametres[,1:ncol(parametres)-1]))

for (i in unique(nomparametres)) {
  nomtaula <- paste0("table_", i)
  taula <- formattable(aggregate(parametres$resultats, list(parametres[,c(i)]), mean), list(
    formattable::area(col=2) ~ color_tile("transparent", "#2dccb9")))
  assign(nomtaula, taula)
}

names(taula_arbres) <- c("Number of trees (ntree)", "Accuracy Rate")
names(taula_variables) <- c("Number of variables (mtry)", "Accuracy Rate")
names(taula_boot) <- c("Bootstrap (replacement)", "Accuracy Rate")

cat("**Mean Accuracy Rate for each possible value of the parameters** \n")

taula_arbres
taula_variables
taula_boot

millor <- parametres[which.max(parametres$resultats),]

modelfinal <- randomForest(AcredResults ~.,
                           data = data_oversample[,-ncol(data_oversample)],
                           ntree = millor$arbres,
                           mtry = millor$variables,
                           replace = millor$boot,
                           importance = TRUE) ## Aquí et treu més mètriques d'importancia

modelfinal

varImpPlot(modelfinal)


matriuconf <- modelfinal$confusion[,c(1,2)]

matriuconf <- round(prop.table(matriuconf, 1),2)

row.names(matriuconf) <- paste(row.names(matriuconf),"real")

colnames(matriuconf) <- paste(colnames(matriuconf),"predit")

test <- melt(matriuconf)

names(test) <- c("target", "prediction", "proportion")

test$prediction <- factor(test$prediction, levels=rev(levels(test$prediction)))

ggplot(test, aes(target, prediction))+
  geom_tile(aes(fill=proportion))+
  theme_minimal()+
  aqu_theme+
  geom_label(aes(label=proportion, fontface=2), size=10)+
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")+
  coord_flip()


# # Prediccions
X <- data.frame(id = names(modelfinal$predicted), preds = modelfinal$predicted)
names(X) <- c("id", "preds_Model")
prediccions <- merge(data_oversample, X, by = 'row.names', all = TRUE)
prediccions$dupl <- duplicated(prediccions$Identifier)
prediccions <- subset(prediccions, prediccions$dupl == 'FALSE')

# Carregar prediccions a la bbdd original

DadesG <- left_join(DadesG, prediccions[,c('Identifier', 'preds_Model')], by = 'Identifier')
write.xlsx(DadesG, file = "./path/to/file/preds.xlsx", colNames = TRUE)