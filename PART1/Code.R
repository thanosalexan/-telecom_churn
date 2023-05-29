
library(readxl)

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
# Visualization numeric variables
par(mfrow=c(2,5))
for (i in 1:10)
{
  h1 <- hist(numeric_df[,i], main=names(numeric_df)[i], col='pink')
}
par(mfrow=c(2,5))
for (i in 11:15)
{
  h2 <- hist(numeric_df[,i], main=names(numeric_df)[i], col='pink')
}


library(ggplot2)



state_plot <- ggplot(df, aes(State, ..count..)) + 
  geom_bar(aes(fill = Churn), position="stack") +
  labs(y = '', x = 'State') +
  scale_fill_manual(name = "churn", labels = c("NO", "YES"), 
                    values = c("steelblue","springgreen3")) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 8), 
        axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))


gender_plot <- ggplot(df, aes(Gender, ..count..)) + 
  geom_bar(aes(fill = Churn), position="stack") +
  labs(y = '', x = 'Gender') +
  scale_fill_manual(name = "churn ", labels = c("NO", "YES"), 
                    values = c("steelblue","springgreen3")) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), 
        axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

Area_code_plot<-ggplot(df, aes(Area.Code, ..count..)) + 
  geom_bar(aes(fill = Churn), position="stack") +
  labs(y = '', x = 'Area.Code') +
  scale_fill_manual(name = "churn", labels = c("NO", "YES"), 
                    values = c("steelblue","springgreen3")) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), 
        axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

Int.l.Plan_plot<-ggplot(df, aes(Int.l.Plan, ..count..)) + 
  geom_bar(aes(fill = Churn), position="stack") +
  labs(y = '', x = 'International Plan') +
  scale_fill_manual(name = "churn", labels = c("NO", "YES"), 
                    values = c("steelblue","springgreen3")) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), 
        axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

VMail.Plan<-ggplot(df, aes(VMail.Plan, ..count..)) + 
  geom_bar(aes(fill = Churn), position="stack") +
  labs(y = '', x = 'Voice mail Plan') +
  scale_fill_manual(name = "churn", labels = c("NO", "YES"), 
                    values = c("steelblue","springgreen3")) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), 
        axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))


par(mfrow=c(1,1))
require(corrplot)

corrplot(cor(numeric_df),method='number',type = "upper", number.cex = .5, tl.offset = 0.6, tl.cex=0.6)

logit1 <- glm(Churn ~ ., data = df, family = binomial(link = "logit"))

summary(logit1)


par(mfrow=c(1,1))

library(glmnet)

set.seed(768)
lambdas <- 10 ^ seq(10,-10,length=1000)
x_matrix<- model.matrix(logit1)[,-1]
fit_lasso <- glmnet(x=x_matrix, y = df$Churn, alpha=1,lambda=lambdas, family="binomial")
plot(fit_lasso, label = TRUE)
plot(fit_lasso, xvar = "lambda", label = TRUE)


# Cross Validation 
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
AIC_model <- step(model2, direction='both') 


summary(AIC_model)

#Stepwise selection according to BIC
BIC_model <- step(model2, direction='both', k = log(3333))

summary(BIC_model)


require(car)

vif(BIC_model)


with(BIC_model, pchisq(deviance, df.residual, lower.tail = FALSE))


install.packages('DescTools')

library(DescTools)

PseudoR2(BIC_model, which = "McFadden")
PseudoR2(model2, which = "McFadden")
PseudoR2(logit1, which = "McFadden")