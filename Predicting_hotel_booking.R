hotel<- read.csv("hotel_bookings.csv")
hotel.booking<- na.omit(hotel)
total.rows<- nrow(hotel.booking)
train.size<- floor(0.7* total.rows)
train.rows<- sample(1:total.rows, train.size) 
hotel.train<- hotel.booking[train.rows,]
hotel.remaining<- hotel.booking[-train.rows,]
remaining.rows<- nrow(hotel.remaining)
validation.size<- floor(0.2* train.rows)
validation.rows<- sample(1:remaining.rows, validation.size) 
hotel.validation<- hotel.remaining[validation.rows,]
hotel.testing<- hotel.remaining[-validation.rows,]
set.seed(1234)

model<- glm(is_canceled ~ hotel + lead_time + arrival_date_month + children + market_segment + is_repeated_guest + adults + babies +previous_cancellations +
deposit_type + booking_changes  + reserved_room_type + adr + days_in_waiting_list + customer_type +
total_of_special_requests, data = hotel.train , family = "binomial")


actual<- hotel.train$is_canceled
predict.training<- predict(model, type= "response")
threshold <- 0.5
predicted <- ifelse(predict.training > threshold, 1, 0)
table(actual, predicted)


library(pROC)
r<- roc(hotel.testing$is_canceled, test.predict)
plot.roc(r)
auc(r)


Accuracy 

sum(actual == predicted)/nrow(hotel.train)

sum (actual== 1 & predicted ==1)/ sum(actual == 1)

sum (actual==0 & predicted ==0)/ sum(actual == 0)

1-(sum(predicted== 0 & actual==0)/ sum(actual==0))



actual<- hotel.validation$is_canceled
predict.valid<- predict(model, type= "response")
threshold <- 0.5
predicted <- ifelse(predict.valid > threshold, 1, 0)
table(actual, predicted)



library(caret)
val.predict<- predict(model, hotel.validation, type="response")
hotel.booking.classification<- ifelse(val.predict<0.5,0,1)
confusionMatrix(as.factor(hotel.booking.classification), as.factor(hotel.validation$is_canceled), positive = "1")


actual<- hotel.validation$is_canceled
1-(sum(val.predict== 0 & actual==0)/ sum(actual==0))


test.predict<- predict(model, hotel.testing, type="response")
hotel.booking.classification<- ifelse(test.predict<0.5,0,1)
confusionMatrix(as.factor(hotel.booking.classification), as.factor(hotel.testing$is_canceled), positive = "1"
actual<- hotel.testing$is_canceled                  

1-(sum(test.predict==0 & actual == 0)/ sum(actual==0))


table(actual, predict.training)

sum(actual == predict.train)/ nrow(hotel.train)


val.predict<- predict(model, hotel.validation, type="response")
head(val.predict)

test.predict<- predict(model, hotel.testing, type="response")
head(test.predict)


predict.probability<- predict(model, type="response")
threshold<- 0.22
predicted <- ifelse(predict.probability > threshold, 1, 0)
actual <- hotel.train$is_canceled
table(actual)


table(actual, predicted)
sum(actual == predicted)/nrow(hotel.train)

sum (actual== 1 & predicted ==1)/ sum(actual == 1)

sum (actual==0 & predicted ==0)/ sum(actual == 0)


library(caret)
hotel.booking.classification<- ifelse(val.predict<0.22,0,1)
confusionMatrix(as.factor(hotel.booking.classification), as.factor(hotel.validation$is_canceled), positive = "1")


test.predict<- predict(model, hotel.testing, type="response")
hotel.booking.classification<- ifelse(test.predict<0.22,0,1)
confusionMatrix(as.factor(hotel.booking.classification), as.factor(hotel.testing$is_canceled), positive = "1")




threshold <- 0.18
predicted <- ifelse(predicted.probability > threshold, 1, 0)
actual <- credit.train$RESPONSE


hotel.booking.classification<- ifelse(val.predict,0,1)
confusionMatrix(as.factor(hotel.booking.classification), as.factor(hotel.validation$is_canceled), positive = "1")



hotel.booking<- hotel.booking[, c(1, 2, 3,5,10, 11, 12, 15,17,18, 20, 22,23,26,27,28,30)]

library(randomForest)


set.seed(1234)   # set a random seed 


total.rows<- nrow(hotel.booking)
train.size<- floor(0.7* total.rows)
train.rows<- sample(1:total.rows, train.size) ## not actual data, only no on the row##
hotel.train<- hotel.booking[train.rows,]
hotel.remaining<- hotel.booking[-train.rows,]

## validation and testing ##

remaining.rows<- nrow(hotel.remaining)
validation.size<- floor(0.2* train.rows)

validation.rows<- sample(1:remaining.rows, validation.size) 

hotel.validation<- hotel.remaining[validation.rows,]
hotel.testing<- hotel.remaining[-validation.rows,]



## classification tree ##


library(rpart)

training_model<-rpart(is_canceled ~ hotel + lead_time + arrival_date_month + children + market_segment + is_repeated_guest + adults + babies + previous_cancellations +
                        deposit_type + booking_changes  + reserved_room_type + adr + days_in_waiting_list + customer_type + total_of_special_requests,
                      data= hotel.train, 
                      method="class", 
                      control=rpart.control(cp=0.03))

plotcp(training_model)


library(rpart.plot)
rpart.plot(training_model)


##Accuracy on Training ##

hotel.train$ct_pred_prob<-predict(training_model,hotel.train)[,2]
hotel.train$ct_pred_class<-predict(training_model,hotel.train,type="class")
table(hotel.train$ct_pred_class==hotel.train$is_canceled)
table(hotel.train$ct_pred_class, hotel.train$is_canceled, dnn=c("predicted","actual"))
Accuracy<- (48124+18837)/ nrow(hotel.train)
Accuracy


sensitivity<- 48124/(48124+12101)
sensitivity


specificity<- 18837/(4508+18837)
specificity

FPR<- 1-(sum(hotel.train$is_canceled==0 & hotel.train$ct_pred_class==0)/ sum(hotel.train$is_canceled==0))
FPR


## Accuracy of the model  on validation##

hotel.validation$ct_pred_prob<-predict(training_model,hotel.validation)[,2]
hotel.validation$ct_pred_class<-predict(training_model,hotel.validation,type="class")
table(hotel.validation$ct_pred_class==hotel.validation$is_canceled)

table(hotel.validation$ct_pred_class, hotel.validation$is_canceled, dnn=c("predicted","actual"))
Accuracy<- (7460+2917)/ nrow(hotel.validation)
Accuracy


sensitivity<- 7460/(7460+1867)
sensitivity


specificity<- 2917/(2917+657)
specificity

FPR<- 1-(sum(hotel.validation$is_canceled & hotel.validation$ct_pred_class)/ sum(hotel.validation$is_canceled==0))



## Accuracy on testing ##

hotel.testing$ct_pred_prob<-predict(training_model,hotel.testing)[,2]
hotel.testing$ct_pred_class<-predict(training_model,hotel.testing,type="class")
table(hotel.testing$is_canceled ==hotel.testing$ct_pred_class)

table(hotel.testing$ct_pred_class, hotel.testing$is_canceled, dnn=c("predicted","actual"))

Accuracy<- (13275 + 5117)/ nrow(hotel.testing)
Accuracy


sensitivity<- 13275/ (13275 + 3354)
sensitivity


specificity<- 5117/(5117+1169)
specificity

## K-cross validation ##

set.seed(1234)

options(scipen = 999)


full_tree<-rpart(is_canceled~ hotel + lead_time + arrival_date_month + children + market_segment + is_repeated_guest + adults + babies + previous_cancellations +
                   deposit_type + booking_changes  + reserved_room_type + adr + days_in_waiting_list + customer_type + total_of_special_requests,
                 data=hotel.train, 
                 method="class",
                 control=rpart.control(cp=0, maxdepth = 3))

rpart.plot(full_tree)

printcp(full_tree)


## cp value that minimizes cross-validation errors ##

min_xerror<-full_tree$cptable[which.min(full_tree$cptable[,"xerror"]),]
min_xerror


# prune tree with minimum cp value
min_xerror_tree<-prune(full_tree, cp=min_xerror[1])
rpart.plot(min_xerror_tree)


## Accuracy ##

bp_tree<-min_xerror_tree
hotel.train$ct_bp_pred_prob<-predict(bp_tree, hotel.train)[,2]
hotel.train$ct_bp_pred_class=ifelse(hotel.train$ct_bp_pred_prob>0.5,1,0)

table(hotel.train$is_canceled == hotel.train$ct_bp_pred_class)

table(hotel.train$ct_bp_pred_class, hotel.train$is_canceled, dnn=c("predicted","actual"))


Accuracy<- (52522+11585)/ nrow(hotel.train)
Accuracy


sensitivity<- 52522/(52522+19353)
sensitivity


specificity<- 11585/(11585+110)
specificity


FPR<- 1-(sum(hotel.train$is_canceled==0 & hotel.train$ct_bp_pred_class==0)/ sum(hotel.train$is_canceled==0))
FPR


bp_tree<-min_xerror_tree
hotel.validation$ct_bp_pred_prob<-predict(bp_tree, hotel.validation)[,2]
hotel.validation$ct_bp_pred_class=ifelse(hotel.validation$ct_bp_pred_prob>0.5,1,0)

table(hotel.validation$is_canceled == hotel.validation$ct_bp_pred_class)

table(hotel.validation$ct_bp_pred_class, hotel.validation$is_canceled, dnn=c("predicted","actual"))


Accuracy<- (8091+1785)/nrow(hotel.validation)
Accuracy


Sensitivity<- 8091/(8091+2999)
Sensitivity

Specificity<- 1785/(26+1785)
Specificity


## Accuracy on testing ##

bp_tree<-min_xerror_tree
hotel.testing$ct_bp_pred_prob<-predict(bp_tree, hotel.testing)[,2]
hotel.testing$ct_bp_pred_class=ifelse(hotel.testing$ct_bp_pred_prob>0.5, 1,0)

table(hotel.testing$ct_bp_pred_class, hotel.testing$is_canceled, dnn=c("predicted","actual"))

table(hotel.testing$is_canceled == hotel.testing$ct_bp_pred_class)

Accuracy<- (14406+3189)/nrow(hotel.testing)
Accuracy

Sensitivity<- 14406/(14406+ 5282)
Sensitivity

Specificity<- 3189/(38+3189)
Specificity

### knn method ##

train_knn <- knn(is_canceled~ hotel + lead_time + arrival_date_month + children + market_segment + is_repeated_guest + adults + babies + previous_cancellations +
                     deposit_type + booking_changes  + reserved_room_type + adr + days_in_waiting_list + customer_type + total_of_special_requests,
                   data=hotel.train,method = "knn",tuneGrid = data.frame(k = seq(3, 21, 2)))


str(hotel.booking)


## change into  factor ##

hotel.booking$is_canceled< is.numeric(hotel.booking$is_canceled)
hotel.booking$lead_time<- as.factor(hotel.booking$lead_time)
hotel.booking$children<- as.factor(hotel.booking$children)
hotel.booking$is_repeated_guest<- as.factor(hotel.booking$is_repeated_guest)
hotel.booking$adults<- as.factor(hotel.booking$adults)
hotel.booking$adr<- as.factor(hotel.booking$adr)
hotel.booking$babies<- as.factor(hotel.booking$babies)
hotel.booking$previous_cancellations<- as.factor(hotel.booking$previous_cancellations)
hotel.booking$booking_changes<- as.factor(hotel.booking$booking_changes)
hotel.booking$days_in_waiting_list<- as.factor(hotel.booking$days_in_waiting_list)
hotel.booking$total_of_special_requests<- as.factor(hotel.booking$total_of_special_requests)

str(hotel.booking)

model<- randomForest(is_canceled ~ hotel + lead_time + arrival_date_month + children + market_segment + is_repeated_guest + adults + babies + previous_cancellations +
                       deposit_type + booking_changes  + reserved_room_type + adr + days_in_waiting_list + customer_type + total_of_special_requests, data = hotel.train, ntree= 10, tuneGrid = data.frame(mtry= seq(1:7)))



actual.validation<- hotel.validation$is_canceled

predict.valid<- predict(model, newdata = hotel.validation)

sum(actual.validation == predict.valid)/ nrow(hotel.validation)



## k nearest neighbor ##
####################################################################################
set.seed(1234)   # set a random seed 


total.rows<- nrow(hotel.booking)
train.size<- floor(0.7* total.rows)
validation.size<- floor(0.2* train.rows)
train.rows<- sample(1:total.rows, train.size) ## not actual data, only no on the row##
hotel.train<- hotel.booking[train.rows,]
hotel.remaining<- hotel.booking[-train.rows,]

## validation and testing ##

remaining.rows<- nrow(hotel.remaining)

validation.rows<- sample(1:remaining.rows, validation.size) 

hotel.validation<- hotel.remaining[validation.rows,]
hotel.testing<- hotel.remaining[-validation.rows,]

x.train<- sapply(hotel.train[, -2], as.numeric)
y.train <- hotel.train[ ,2]

x.valid<- sapply(hotel.validation [, -2], as.numeric)
y.valid <- hotel.validation[,2]


x.test<- sapply(hotel.testing [, -2], as.numeric)
y.test <- hotel.testing[,2]

dim(y.test)

normalize<- function(numbers) {
(numbers- mean(numbers))/ sd(numbers)
}
x.train.normalize<- apply(x.train, 2, normalize)
x.train.normalize<- as.data.frame(x.train.normalize)

x.valid.normalize<- apply(x.valid, 2, normalize)
x.valid.normalize<- as.data.frame(x.valid.normalize)

dim(x.valid.normalize)

x.testing.normalize<- apply(x.test, 2, normalize)
x.testing.normalize<- as.data.frame(x.testing.normalize)


dim(x.testing.normalize)


library(class)

for (k in 1:20) {
predicted.value<- knn.cv(x.train.normalize, y.train, k)
print(paste("With", k, "neighbors, the accuracy is", sum(y.train== predicted.value)/nrow(x.train.normalize)))
}
  
dim(x.train)
dim(x.valid)
dim(x.test)

predict.valid<- knn(x.train, x.valid, y.train, 9)
sum(predict.valid== y.valid)/ nrow(x.valid)


sum(predict.valid==1 & actual==1 )/ sum(actual==1) 

##Specificity  ##
sum(predict.valid== 0 & actual==0 )/ sum(actual==0) 


predicted.valid.normalize<- knn(x.train.normalize, valid.normalize, y.train, 9)
sum(y.valid==predicted.valid.normalize)/ nrow(valid.normalize)

actual <- hotel.validation$is_canceled

##Sensitivity ##
sum(predicted.valid.normalize==1 & actual==1 )/ sum(actual==1) 
 
##Specificity  ##
sum(predicted.valid.normalize== 0 & actual==0 )/ sum(actual==0) 

## FPR ##
1-(sum(predicted.valid.normalize== 0 & actual==0)/ sum(actual==0))


## testing ############################

predicted.test.normalize<- knn(x.train.normalize, test.normalize, y.train, 9)
sum(y.test==predicted.test.normalize)/ nrow(test.normalize)



actual <- hotel.testing$is_canceled

##Sensitivity ##

sum(predicted.test.normalize==1 & actual==1 )/ sum(actual==1) 

##Specificity  ##

sum(predicted.test.normalize== 0 & actual==0 )/ sum(actual==0) 

## FPR ##

1-(sum(predicted.test.normalize== 0 & actual==0)/ sum(actual==0))



## ROC for all ##


ct_roc<-roc(test$is_canceled,test$ct_bp_pred_prob,auc=TRUE)

logit_roc<-roc(test$is_canceled,test$logit_pred_prob,auc=TRUE)

nb_roc = roc(test$is_canceled,pred_prob_nb[,2],auc=TRUE)
rf_roc<-roc(test$is_canceled,test$rf_pred_prob,auc=TRUE)



