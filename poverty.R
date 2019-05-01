library(randomForest)
library(dplyr)
library(tidyverse)
library(xgboost)
library(glmnet)
library(caret)


povertyData<-read.csv('D:\\bin\\OR568\\Project\\costa-rican-household-poverty-prediction\\train.csv')
povertyDataTest<-read.csv('D:\\bin\\OR568\\Project\\costa-rican-household-poverty-prediction\\test.csv')
samp<-povertyDataTest$Id
###################################################################################################

###########################################################################################
#Train
colnames(povertyData)[colSums(is.na(povertyData)) > 0]
sapply(povertyData, function(x) sum(is.na(x)))[colSums(is.na(povertyData)) > 0]
nullValueCols<-sapply(training, function(x) sum(is.na(x)))


###########################Don't need to run code below- This was for proposal submission#######################################################33
soy$ID <- rownames(soy) # Adding an ID variable
Plots2 <- melt(soy,id="Id") # melting to long format
counts <- ddply(Plots2, .(Plots2$variable, Plots2$value), nrow)
names(counts) <- c("y", "m", "Freq")
#Frequency plot using ggplot.
ggplot(counts, aes(y = Freq, x = m)) + geom_histogram(stat = "identity") + facet_wrap(~y)

str(povertyData)
nums <- unlist(lapply(x, is.numeric))  
is.fact <- sapply(povertyData, is.factor)

barplot(table(povertyData$Target),xlab="Poverty level",ylab = "Frequency")

#povertyData=povertyData[,-c('idhogar')]
#povertyData = subset(povertyData, select = -c('idhogar') )
povertyData = select(povertyData, -c('idhogar','Id') )
#povertyData <- povertyData[ ,-c('idhogar','Id')]
str(povertyData)

rfModel<-randomForest(Target~., data=povertyData,importance=TRUE, na.action=na.omit)
plot(rfModel)
plot(importance(rfModel))

varImpPlot(rfModel,main="Important Predictors")

##########################################Run from here######################################################

#povertyData


#colnames(povertyData)
#row<-nrow(povertyData)
#set.seed(12345)
#trainindex <- sample(row, row*.7, replace=FALSE)
#training <- povertyData[trainindex, ]
#validation <- povertyData[-trainindex, ]



#rfModel<-randomForest(as.factor(Target)~., data=training,importance=TRUE, na.action=na.omit)
#rfPredicted=predict(rfModel,validation,type="response")

#table(rfPredicted,validation$Target)
###############################Treating Null values####################################
######House -Rent#####
povertyDataV2Na <- povertyData[rowSums(is.na(povertyData['v2a1'])) > 0,]

houseOwnership=c(table(povertyDataV2Na$tipovivi1)[2],table(povertyDataV2Na$tipovivi2)[2],table(povertyDataV2Na$tipovivi3)[2],table(povertyDataV2Na$tipovivi4)[2])

barplot(houseOwnership,names.arg=c("Owned and paid","Owned-paying","Rented","Precarious"),col=c("light green","red","green","yellow"),las=0.5)


######Number of tablets#######################################
povertyDataV18Na <- povertyData[rowSums(is.na(povertyData['v18q1'])) > 0,]

tabletOwnership=c(table(povertyDataV18Na$v18q))

######Years behind in school#######################################

povertyDataVrez <- povertyData[rowSums(is.na(povertyData['rez_esc'])) == 0,]
summary(povertyDataVrez$age)
############################Convet all NAs to zeors##################################
povertyData[is.na(povertyData)] <- 0
#train
#Check if all nulls are trated
colnames(povertyData)[colSums(is.na(povertyData)) > 0]

#test
povertyDataTest[is.na(povertyDataTest)] <- 0
############################Feature Engineering###################################
#Removing all columns which are squared
#Train
povertyData <- povertyData[,-(134:140),drop=FALSE]

#Test
povertyDataTest <- povertyDataTest[,-(134:140),drop=FALSE]
#povertyData <- povertyData[ , -which(names(povertyData) %in% c("female"))]



has_many_values <- function(x) n_distinct(x) > 1
dup_var <- function(x) lapply(x, c) %>% duplicated %>% which 

#Aggregate individual variable to household level
povertyData <- povertyData %>%
  group_by(idhogar) %>%
  mutate(mean_age = mean(age, na.rm = TRUE)) %>%
  mutate(no_of_disabled = sum(dis)) %>%
  mutate(no_of_children = sum(estadocivil1)) %>%
  mutate(no_of_coupledunion = sum(estadocivil2)) %>%
  mutate(no_of_married = sum(estadocivil3)) %>%
  mutate(no_of_divorced = sum(estadocivil4)) %>%
  mutate(no_of_separated = sum(estadocivil5)) %>%
  mutate(no_of_widower = sum(estadocivil6)) %>%
  mutate(no_of_single = sum(estadocivil7)) %>%
  mutate(no_of_instlevel1 = sum(instlevel1)) %>%
  mutate(no_of_instlevel2 = sum(instlevel2)) %>%
  mutate(no_of_instlevel3 = sum(instlevel3)) %>%
  mutate(no_of_instlevel4 = sum(instlevel4)) %>%
  mutate(no_of_instlevel5 = sum(instlevel5)) %>%
  mutate(no_of_instlevel6 = sum(instlevel6)) %>%
  mutate(no_of_instlevel7 = sum(instlevel7)) %>%
  mutate(no_of_instlevel8 = sum(instlevel8)) %>%
  mutate(no_of_instlevel9 = sum(instlevel9)) %>%
  mutate(rent_rooms = (v2a1 / rooms)) %>%
  mutate(tamhog_rooms = (tamhog / rooms)) %>%
  mutate(v2a1_r4t1_r4t3 = (v2a1 / (r4t3 - r4t1))) %>%
  mutate(v2a1_r4t1 = (v2a1 / r4t1)) %>%
  mutate(v2a1_r4t2 = (v2a1 / r4t2)) %>%
  mutate(v2a1_r4t3 = (v2a1 / r4t3)) %>%
  mutate(v2a1_r4h1 = (v2a1 / r4h1)) %>%
  mutate(v2a1_r4h2 = (v2a1 / r4h2)) %>%
  mutate(v2a1_r4h3 = (v2a1 / r4h3)) %>%
  mutate(v2a1_r4m1 = (v2a1 / r4m1)) %>%
  mutate(v2a1_r4m2 = (v2a1 / r4m2)) %>%
  mutate(v2a1_r4m3 = (v2a1 / r4m3)) %>%
  mutate(v2a1_bedrooms = (v2a1 / bedrooms)) %>%
  mutate(r4t1_r4t3 = (r4t1 / r4t3)) %>%
  mutate(v2a1_tamhog = (v2a1 / tamhog)) %>%
  mutate(r4m1_r4t3 = (r4m1 / r4t3)) %>%
  mutate(rooms_r4t1 = (rooms / r4t1)) %>%
  mutate(v18q1_r4t3 = (v18q1 / r4t3)) %>%
  mutate(r4h3_r4t3 = (r4h3 / r4t3)) %>%
  mutate(r4m3_r4t3 = (r4m3 / r4t3)) %>%
  mutate(v18q1_r4t3 = (v18q1 / r4t2)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  mutate(r4t3_tamhog = (r4t3 / tamhog)) %>%
  mutate(r4t3_rooms = (r4t3 / rooms)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  ungroup()


#Aggregate individual variable to household level
povertyDataTest <- povertyDataTest %>%
  group_by(idhogar) %>%
  mutate(mean_age = mean(age, na.rm = TRUE)) %>%
  mutate(no_of_disabled = sum(dis)) %>%
  mutate(no_of_children = sum(estadocivil1)) %>%
  mutate(no_of_coupledunion = sum(estadocivil2)) %>%
  mutate(no_of_married = sum(estadocivil3)) %>%
  mutate(no_of_divorced = sum(estadocivil4)) %>%
  mutate(no_of_separated = sum(estadocivil5)) %>%
  mutate(no_of_widower = sum(estadocivil6)) %>%
  mutate(no_of_single = sum(estadocivil7)) %>%
  mutate(no_of_instlevel1 = sum(instlevel1)) %>%
  mutate(no_of_instlevel2 = sum(instlevel2)) %>%
  mutate(no_of_instlevel3 = sum(instlevel3)) %>%
  mutate(no_of_instlevel4 = sum(instlevel4)) %>%
  mutate(no_of_instlevel5 = sum(instlevel5)) %>%
  mutate(no_of_instlevel6 = sum(instlevel6)) %>%
  mutate(no_of_instlevel7 = sum(instlevel7)) %>%
  mutate(no_of_instlevel8 = sum(instlevel8)) %>%
  mutate(no_of_instlevel9 = sum(instlevel9)) %>%
  mutate(rent_rooms = (v2a1 / rooms)) %>%
  mutate(tamhog_rooms = (tamhog / rooms)) %>%
  mutate(v2a1_r4t1_r4t3 = (v2a1 / (r4t3 - r4t1))) %>%
  mutate(v2a1_r4t1 = (v2a1 / r4t1)) %>%
  mutate(v2a1_r4t2 = (v2a1 / r4t2)) %>%
  mutate(v2a1_r4t3 = (v2a1 / r4t3)) %>%
  mutate(v2a1_r4h1 = (v2a1 / r4h1)) %>%
  mutate(v2a1_r4h2 = (v2a1 / r4h2)) %>%
  mutate(v2a1_r4h3 = (v2a1 / r4h3)) %>%
  mutate(v2a1_r4m1 = (v2a1 / r4m1)) %>%
  mutate(v2a1_r4m2 = (v2a1 / r4m2)) %>%
  mutate(v2a1_r4m3 = (v2a1 / r4m3)) %>%
  mutate(v2a1_bedrooms = (v2a1 / bedrooms)) %>%
  mutate(r4t1_r4t3 = (r4t1 / r4t3)) %>%
  mutate(v2a1_tamhog = (v2a1 / tamhog)) %>%
  mutate(r4m1_r4t3 = (r4m1 / r4t3)) %>%
  mutate(rooms_r4t1 = (rooms / r4t1)) %>%
  mutate(v18q1_r4t3 = (v18q1 / r4t3)) %>%
  mutate(r4h3_r4t3 = (r4h3 / r4t3)) %>%
  mutate(r4m3_r4t3 = (r4m3 / r4t3)) %>%
  mutate(v18q1_r4t3 = (v18q1 / r4t2)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  mutate(r4t3_tamhog = (r4t3 / tamhog)) %>%
  mutate(r4t3_rooms = (r4t3 / rooms)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  ungroup()




povertyData <- povertyData %>%
  select(-tamviv) %>% # number of persons living in the household
  select(-hogar_total) %>% # # of total individuals in the household
  select(-r4t3) %>% # Total persons in the household
  select(-tamhog) %>% # size of the household
  select(-r4t1) %>% # persons younger than 12 years of age
  select(-r4t2) %>% # persons 12 years of age and older
  select(-agesq) %>% # Age squared
  select(-Id) %>% # removing id
  select(-idhogar)


povertyDataTest <- povertyDataTest %>%
  #select(-tamviv) %>% # number of persons living in the household
  #select(-hogar_total) %>% # # of total individuals in the household
  #select(-r4t3) %>% # Total persons in the household
  #select(-tamhog) %>% # size of the household
  #select(-r4t1) %>% # persons younger than 12 years of age
  #select(-r4t2) %>% # persons 12 years of age and older
  #select(-agesq) %>% # Age squared
  select(-Id) %>% # removing id
  select(-idhogar)

# Check if the dataset has any non numeric column(s)
povertyData %>%
  select_if(funs(!is.numeric(.)))

povertyDataTest %>%
  select_if(funs(!is.numeric(.)))

# Recode values in dependency, edjefe, edjefa
povertyData[,c("dependency","edjefe","edjefa")] <- povertyData %>% 
  select(dependency,edjefe,edjefa) %>% 
  mutate_all(funs(ifelse(. == "yes",1,ifelse(. == "no",0,.)))) %>% 
  mutate_all(as.numeric)

povertyDataTest[,c("dependency","edjefe","edjefa")] <- povertyDataTest %>% 
  select(dependency,edjefe,edjefa) %>% 
  mutate_all(funs(ifelse(. == "yes",1,ifelse(. == "no",0,.)))) %>% 
  mutate_all(as.numeric)


row<-nrow(povertyData)
set.seed(12345)
trainindex <- sample(row, row*.7, replace=FALSE)
training <- povertyData[trainindex, ]
validation <- povertyData[-trainindex, ]


train_labels <- as.numeric(training$Target) - 1
test_labels<-as.numeric(validation$Target)-1
train_data <- as.matrix(training[,-127])
test_data <- as.matrix(validation[,-127])








###############################################XGB#############################################
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)


#test_labels<-as.numeric(povertyDataTest$Target)-1
#test_data <- as.matrix(povertyDataTest)

#dtest <- xgb.DMatrix(data = test_data, label= test_labels)

#xgbmodel
xgb <- xgboost(data = dtrain,
               objective = "multi:softmax",
               booster = "gbtree",
               eval_metric = "mlogloss",
               num_class = 4,
               nthread = 4,
               eta = 0.05,
               max_depth = 8,
               min_child_weight = 6,
               gamma = 0,
               subsample = 0.7,
               colsample_bytree = 0.7,
               colsample_bylevel = 0.7,
               alpha = 0,
               lambda = 0,
               nrounds = 5000,
               print_every_n = 200, 
               early_stopping_rounds = 400
)



# Get information on how important each feature is and plot it!
xgb.importance(names(train_data), model = xgb) %>% 
  xgb.plot.importance(top_n = 15)
pred<-predict(xgb,test_data)
cm=table(pred,test_labels)

diag = diag(cm)
colsums = apply(cm, 2, sum)
rowsums = apply(cm, 1, sum)
precision = diag / colsums 
recall = diag / rowsums
f1 = 2 * precision * recall / (precision + recall) 
macroF1 = mean(f1)


f1_score <- function(predicted, expected, positive.class="1") {
  predicted <- factor(as.character(predicted), levels=unique(as.character(expected)))
  expected  <- as.factor(expected)
  cm = as.matrix(table(expected, predicted))
  
  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  
  #Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0
  
  #Binary F1 or Multi-class macro-averaged F1
  ifelse(nlevels(expected) == 2, f1[positive.class], mean(f1))
}

f1_score(pred,test_labels)
povertyDataTest$Id<-0
predictions_submit = cbind(samp,pred) %>% data.frame()

names(predictions_submit) = c("Id","Target")
predictions_submit$Target = as.integer(predictions_submit$Target)

write_csv(predictions_submit, "D:\\bin\\OR568\\Project\\costa-rican-household-poverty-prediction\\submission.csv")

#colnames(povertyData)
#row<-nrow(povertyData)
#set.seed(12345)
#trainindex <- sample(row, row*.7, replace=FALSE)
#training <- povertyData[trainindex, ]
#validation <- povertyData[-trainindex, ]
#training
########################SVM#########################
trctrl <- trainControl(method = "cv", number = 5, repeats = 2)
set.seed(3233)

svm_Linear <- train(Target ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    #preProcess = c("center", "scale"),##################################
                    tuneLength = 10)

