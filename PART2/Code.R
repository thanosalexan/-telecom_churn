library(readxl)
library(randomForest)
library(caret)
library(ROCR)
library(rattle)
library(pROC)

df<-read_excel('churn.xls', sheet=1)
df<-data.frame(df)

View(df)


str(df)


for (i in 1:ncol(df))
{
  na <- is.na(df[,i])
  inf <- is.infinite(df[,i])
  nan <- is.nan(df[,i])
}
any(na)
any(nan)
any(inf)

df$Gender<-factor(df$Gender)
df$Area.Code<-factor(df$Area.Code)
df$Int.l.Plan<-factor(df$Int.l.Plan)
df$VMail.Plan<-factor(df$VMail.Plan)
df$State<-factor(df$State)
df$Churn<-factor(df$Churn)

# Create numeric variable dataset
library(psych)
index <- sapply(df, class) =='numeric'
numeric_df <- df[,index] 
round(t(describe(numeric_df <- df[,index])),2) 
n <- nrow(numeric_df)

str(numeric_df)

# Create factor variable dataset
factors_df <- df[,!index]

str(factors_df)


logit1 <- glm(Churn ~ ., data = df, family = binomial(link = "logit"))

summary(logit1)


library(glmnet)

set.seed(768)
lambdas <- 10 ^ seq(10,-10,length=1000)
x_matrix<- model.matrix(logit1)[,-1]
fit_lasso <- glmnet(x=x_matrix, y = df$Churn, alpha=1,lambda=lambdas, family="binomial")
plot(fit_lasso, label = TRUE)
plot(fit_lasso, xvar = "lambda", label = TRUE)

lasso.cv <- cv.glmnet(x_matrix, df$Churn, alpha=1, lambda=lambdas, family="binomial", type.measure='class')
plot(lasso.cv)
coef(lasso.cv, s = lasso.cv$lambda.min)
coef(lasso.cv, s = lasso.cv$lambda.1se)	

rownames(coef(lasso.cv, s = 'lambda.1se'))[coef(lasso.cv, s = 'lambda.1se')[,1]!= 0]


model2 <- glm(Churn ~ Day.Mins + Eve.Mins + Night.Mins + Intl.Mins + CustServ.Calls+
                Int.l.Plan + VMail.Plan + Day.Charge + Eve.Charge + Night.Charge + Intl.Calls + Intl.Charge +
                State, data = df,family = binomial(link = "logit"))

summary(model2)


2078.2/3270
#Stepwise selection according to AIC
AIC_model <- step(logit1, direction='both') 


summary(AIC_model)

2161.6/3323


n <- dim(df)[1]

k <- 6
set.seed(98798)
deiktes<-sample(1:n)	#random permutation of the rows
methods <- c('naiveBayes','tree', 'svm','randomForest')
accuracy <- matrix(data=NA, ncol= k, nrow = length(methods))
ari <- matrix(data=NA, ncol= k, nrow = length(methods))
rownames(accuracy) <- rownames(ari) <- methods

for (i in 1:k){
  te <- deiktes[ ((i-1)*(n/k)+1):(i*(n/k))]	
  train <- df[-te, ]
  train[,'Churn'] <- as.factor(train[,'Churn'])
  test <- df[te, -8]
  #	naive Bayes
  z <- naiveBayes(Churn ~ VMail.Message  +  Eve.Mins +CustServ.Calls +Int.l.Plan+VMail.Plan+
                    Day.Charge+Night.Charge+Intl.Calls +Intl.Charge, data = train)
  pr_nB <- predict(z, test)
  accuracy['naiveBayes',i] <- sum(df[te,'Churn'] == pr_nB)/dim(test)[1]
  ari['naiveBayes',i] <- adjustedRandIndex(pr_nB, df[te,'Churn'])
 
  #	tree
  tree <- tree(Churn ~ VMail.Message  +  Eve.Mins +CustServ.Calls +Int.l.Plan+VMail.Plan+
                 Day.Charge+Night.Charge+Intl.Calls +Intl.Charge, data = train)
  pr_tree <- predict(tree,newdata=test,type='class')
  accuracy['tree',i] <- sum(df[te,'Churn'] == pr_tree)/dim(test)[1]	
  ari['tree',i] <- adjustedRandIndex(pr_tree, df[te,'Churn'])
  #	svm
  svm <- svm(Churn ~ VMail.Message  +  Eve.Mins +CustServ.Calls +Int.l.Plan+VMail.Plan+
                Day.Charge+Night.Charge+Intl.Calls +Intl.Charge, data=train)
  pr_svm <- predict(svm, newdata=test)
  accuracy['svm',i] <- sum(df[te,'Churn'] == pr_svm)/dim(test)[1]
  ari['svm',i] <- adjustedRandIndex(pr_svm, df[te,'Churn'])
  # Random forest
  rand_for <- randomForest(Churn ~ VMail.Message  +  Eve.Mins +CustServ.Calls +Int.l.Plan+VMail.Plan+
                       Day.Charge+Night.Charge+Intl.Calls +Intl.Charge, data=train, ntree=100, mtry=5, importance=TRUE)
  pr_rand_for <- predict(rand_for, test, type='class')
  accuracy['randomForest',i] <- sum(df[te,'Churn'] == pr_rand_for)/dim(test)[1]
  ari['randomForest',i] <- adjustedRandIndex(pr_rand_for, df[te,'Churn'])
}

avg_accuracy <- cbind(Mean = apply(accuracy, 1, mean))
avg_ari <- cbind(Mean = apply(ari, 1, mean))

# ROC Curves
predictions_nB <- prediction(as.numeric(as.character(pr_nB)), df[te,'Churn'])
roc_nB <- performance(predictions_nB, "tpr","fpr")
plot(roc_nB, col = "black", main = "ROC Curve - Naive Bayes")


auc(df[te,'Churn'],as.numeric(as.character(pr_nB)))


predictions_tree <- prediction(as.numeric(as.character(pr_tree)), df[te,'Churn'])
roc_tree <- performance(predictions_tree, "tpr","fpr")
plot(roc_tree, col = "black", main = "ROC Curve - Tree")

auc(df[te,'Churn'],as.numeric(as.character(pr_tree)))

predictions_svm <- prediction(as.numeric(as.character(pr_svm)), df[te,'Churn'])
roc_svm <- performance(predictions_svm, "tpr","fpr")
plot(roc_svm, col = "black", main = "ROC Curve - SVM")

auc(df[te,'Churn'],as.numeric(as.character(pr_svm)))

predictions_rand_for <- prediction(as.numeric(as.character(pr_rand_for)), df[te,'Churn'])
roc_rand_for <- performance(predictions_rand_for, "tpr","fpr")
plot(roc_rand_for, col = "black", main = "ROC Curve - Random Forest")

auc(df[te,'Churn'],as.numeric(as.character(pr_rand_for)))

#comparison
plot(roc_nB, col = "red",  main = "ROC Curves - Comparison")
plot(roc_tree, col = "orange", add = TRUE)
plot(roc_svm, col = "purple", add = TRUE)
plot(roc_rand_for, col = "black", add = TRUE)

legend("bottomright", legend=c("naive Bayes","Tree","SVM", "Random Forest"), col=c("red","orange","purple","black"), lty=1,cex=0.5)

boxplot(t(accuracy), ylab='predictive accuracy', xlab='method')
boxplot(t(ari), ylab='Adjusted Rand Index', xlab='method')



####          clustering         #######################################################
#########################################################################################


library('pgmm')
library('cluster')
library('mclust')
library('NbClust')
library('jpeg')
library(corrgram)
library(nnet)
library(class)
library(tree)
library(MASS)
library(pgmm)
library(penalizedLDA)
library(corrplot)
library(fpc)

usage<-df[,c("Day.Mins", "Eve.Mins", "Night.Mins",  "Day.Calls", "Eve.Calls", "Night.Calls", "Day.Charge","Eve.Charge","Night.Charge")]
cor(usage)
corrplot(cor(usage), method = "color")

usage_data<-df[,c("Day.Mins", "Eve.Mins", "Night.Mins", "Day.Calls", "Eve.Calls", "Night.Calls")]
scaled0<-scale(usage_data)




####transformation 2
usage_totals2<-usage_data
usage_totals2$total_Mins<-usage_totals2$Day.Mins+usage_totals2$Eve.Mins+usage_totals2$Night.Mins
usage_totals2$total_calls<-usage_totals2$Day.Calls+usage_totals2$Eve.Calls+usage_totals2$Night.Calls
usage_totals2 <- subset(usage_totals2, select = -c(Day.Mins,Eve.Mins,Night.Mins,Day.Calls,Eve.Calls,Night.Calls))

View(usage_totals2)
scaled2<-scale(usage_totals2)

####transformation 3
usage_proportions<-usage_data
usage_proportions$dayMinsToTotalMins<-usage_proportions$Day.Mins/usage_totals2$total_Mins
usage_proportions$eveMinsToTotalMins<-usage_proportions$Eve.Mins/usage_totals2$total_Mins
usage_proportions$nightMinsToTotalMins<-usage_proportions$Night.Mins/usage_totals2$total_Mins
usage_proportions$dayCallsToTotalCalls<-usage_proportions$Day.Calls/usage_totals2$total_calls
usage_proportions$eveCallsToTotalCalls<-usage_proportions$Eve.Calls/usage_totals2$total_calls
usage_proportions$nightCallsToTotalCalls<-usage_proportions$Night.Calls/usage_totals2$total_calls

usage_proportions <- subset(usage_proportions, select = -c(Day.Mins,Eve.Mins,Night.Mins,Day.Calls,Eve.Calls,Night.Calls))

View(usage_proportions)


######################## Hierhahical clustering
###WARD


HC2<-hclust(dist(scaled2),method="ward")
CLAS1<-cutree(HC2, k=2:7)

RES<-NULL
for (i in 1:6){
  a<-silhouette(CLAS1[,i], dist(scaled2))
  RES<-c(RES,mean(a[,3]))
}
plot(2:7,RES,type="b",ylim=c(0,0.5),xlab="clusters", main="Average Silhouette")

plot(HC2$height)


HC2<-hclust(dist(usage_proportions),method="ward")
CLAS1<-cutree(HC2, k=2:7)

RES<-NULL
for (i in 1:6){
  a<-silhouette(CLAS1[,i], dist(usage_proportions))
  RES<-c(RES,mean(a[,3]))
}
plot(2:7,RES,type="b",ylim=c(0,0.5),xlab="clusters", main="Average Silhouette")

#scaled2
hr_cl2_ward <- hclust(dist(scaled2), method="ward.D") 
cutree(hr_cl2_ward, k = 2)
plot(hr_cl2_ward)
rect.hclust(hr_cl2_ward,k=2, border="red")

par(mfrow=c(1,1))
plot(silhouette(cutree(hr_cl2_ward, k = 2), dist(scaled2)), border = NA, col=2:3)

#usage_proportions
hr_cl3_ward <- hclust(dist(usage_proportions), method="ward.D") 
cutree(hr_cl3_ward, k = 2)
plot(hr_cl3_ward)
rect.hclust(hr_cl3_ward, k=2, border="red")

par(mfrow=c(1,1))
plot(silhouette(cutree(hr_cl3_ward, k = 2), dist(usage_proportions)), border = NA, col=2:3)

#####complete

HC2<-hclust(dist(scaled2),method="complete")
CLAS1<-cutree(HC2, k=2:9)

RES<-NULL
for (i in 1:8){
  a<-silhouette(CLAS1[,i], dist(scaled2))
  RES<-c(RES,mean(a[,3]))
}
plot(2:9,RES,type="b",ylim=c(0,0.5),xlab="clusters", main="Average Silhouette")

HC2<-hclust(dist(usage_proportions),method="complete")
CLAS1<-cutree(HC2, k=2:7)

RES<-NULL
for (i in 1:6){
  a<-silhouette(CLAS1[,i], dist(usage_proportions))
  RES<-c(RES,mean(a[,3]))
}
plot(2:7,RES,type="b",ylim=c(0,0.7),xlab="clusters", main="Average Silhouette")


hr_cl2_complete <- hclust(dist(scaled2), method="complete") 
cutree(hr_cl2_complete, k = 3)
plot(hr_cl2_complete)
rect.hclust(hr_cl2_complete, k=3, border="red")

par(mfrow=c(1,1))
plot(silhouette(cutree(hr_cl2_complete, k = 3), dist(scaled2)), border = NA, col=2:4)


hr_cl3_complete <- hclust(dist(usage_proportions), method="complete") 
cutree(hr_cl3_complete, k = 2)
plot(hr_cl3_complete)
rect.hclust(hr_cl3_complete, k=2, border="red")

par(mfrow=c(1,1))
plot(silhouette(cutree(hr_cl3_complete, k = 2), dist(usage_proportions)), border = NA, col=2:2)


par(mfrow=c(1,2))
plot(silhouette(cutree(hr_cl2_ward, k = 2), dist(scaled2)), border = NA, col=2:3)
plot(silhouette(cutree(hr_cl2_complete, k = 3), dist(scaled2)), border = NA, col=2:4)

par(mfrow=c(1,2))
plot(silhouette(cutree(hr_cl3_ward, k = 2), dist(usage_proportions)), border = NA, col=2:3)
plot(silhouette(cutree(hr_cl3_complete, k = 2), dist(usage_proportions)), border = NA, col=2:2)

#	Clustering wine data using k-means:

par(mfrow=c(1,1))
within<-NULL
for (i in 2:15) {
  within<-c(within,kmeans(scaled0,i,nstart=20)$tot.withinss) }
plot(2:15,within, type="b",xlab="number of cluster", ylab="total within ss")


within<-NULL
for (i in 2:15) {
  within<-c(within,kmeans(scaled2,i,nstart=20)$tot.withinss) }
plot(2:15,within, type="b",xlab="number of cluster", ylab="total within ss")

set.seed(9898)

#	Clustering wine data using k-means:
#	using k = 2 clusters
km22<- kmeans(scaled2, 2)
#	using k = 3 clusters
km23<- kmeans(scaled2, 3)
#	using k = 4 clusters
km24<- kmeans(scaled2, 4)

par(mfrow=c(1,3))
plot(silhouette(km22$cluster, dist(scaled2)),border = NA,col=2:3,main ='K-means')
plot(silhouette(km23$cluster,dist(scaled2)),border = NA,col=2:4,main ='K-means')
plot(silhouette(km24$cluster, dist(scaled2)),border = NA,col=2:5,main ='K-means')

par(mfrow=c(1,1))
plot(silhouette(km23$cluster,dist(scaled2)),border = NA,col=2:4,main ='K-means')
plot(silhouette(km24$cluster, dist(scaled2)),border = NA,col=2:5,main ='K-means')


within<-NULL
for (i in 2:15) {
  within<-c(within,kmeans(usage_proportions,i,nstart=20)$tot.withinss) }
plot(2:15,within, type="b",xlab="number of cluster", ylab="total within ss")

#	Clustering wine data using k-means:
#	using k = 2 clusters
km32<- kmeans(usage_proportions, 2)
#	using k = 3 clusters
km33<- kmeans(usage_proportions, 3)
#	using k = 4 clusters
km34<- kmeans(usage_proportions, 4)

par(mfrow=c(1,3))
plot(silhouette(km32$cluster, dist(usage_proportions)),border = NA,col=2:3,main ='ward')
plot(silhouette(km33$cluster,dist(usage_proportions)),border = NA,col=2:4,main ='ward')
plot(silhouette(km34$cluster, dist(usage_proportions)),border = NA,col=2:5,main ='ward')



#####Final 
plot(silhouette(km23$cluster, dist(scaled2)),border = NA,col=2:4,main ='K-means')

par(mfrow=c(1,1))
clusplot(scaled2, km23$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(scaled2, cutree(hr_cl2_ward, k = 2), color=TRUE, shade=TRUE, labels=2, lines=0)

km23$centers



clustered_data<- as.data.frame(cbind(scaled2, cluster = km23$cluster))


usage_totals2$cluster<-clustered_data$cluster

means1 <- apply(usage_totals2[which(usage_totals2$cluster == 1), ], MARGIN = 2, FUN = mean)
means2 <- apply(usage_totals2[which(usage_totals2$cluster == 2), ], MARGIN = 2, FUN = mean)
means3 <- apply(usage_totals2[which(usage_totals2$cluster == 3), ], MARGIN = 2, FUN = mean)

means <- rbind(means1, means2, means3)


