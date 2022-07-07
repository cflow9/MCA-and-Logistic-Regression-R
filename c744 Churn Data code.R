library(tidyverse)
library(readr)
library(dplyr)
library(assertthat)
library(openxlsx)
library(ggplot2)
library(gridExtra)
churnData <- read.csv(file = "C:/Users/chris/Google Drive/C744/WA_Fn-UseC_-Telco-Customer-Churn.csv",header=TRUE,sep=",",stringsAsFactors = TRUE)
str(churnData)
summary(churnData)
glimpse(churnData)
#removed column the customerID
churnData=subset(churnData,select= -c(customerID))
#checking column customerID is deleted from the data
summary(churnData)
#churnData$TotalCharges[is.na(churnData$TotalCharges)] <-1 
#omitting any missing data 
churnData = na.omit(churnData)
#removing any missing values
churnData <- churnData[complete.cases(churnData), ]
#churnData
glimpse(churnData)
#recoding seniorCitizen to strings
churnData$SeniorCitizen <- ifelse(churnData$SeniorCitizen == 1, "Yes", ifelse(churnData$SeniorCitizen == 0, "No","Unknown"))
#forcing senior citizen to a factor column like all other string columns
churnData$SeniorCitizen <- as.factor(churnData$SeniorCitizen)
#checking senior citizen to see if it is now Yes or No and a factor
summary(churnData$SeniorCitizen)
#recording values that include no to just be NO
for (i in 7:14) {
  levels(churnData[,i])[levels(churnData[,i]) %in% c ("No internet service","No phone service")] <- "No"
}
#checking the distribution of the continous variable total charges, monthly charges and tenure
tenureHist <- ggplot(churnData, aes(tenure)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Tenure  Histogram",
       x = "Length of Customer Tenure",
       y = "Count")
monthlyChargesHist <- ggplot(churnData, aes(MonthlyCharges)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Monthly Charges Histogram",
       x = "Length of Customer Monthly Charges",
       y = "Count")
totalChargesHist <- ggplot(churnData, aes(TotalCharges)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  scale_x_continuous(breaks = seq(from = 0, to = 10000, by = 1000)) +
  labs(title = "Customer Total Charges Histogram",
       x = "Length of Customer Total Charges",
       y = "Count")
grid.arrange(tenureHist, monthlyChargesHist,totalChargesHist, ncol=3)
#spilting tenure into three categories New for short term customers, Traditional for the middle tier and legacy for the long time customers.
churnData$tenure <-ifelse(churnData$tenure <= 25, "New",ifelse(churnData$tenure > 25 & churnData$tenure <=45, "Traditional","Legacy"))
churnData$tenure<- as.factor(churnData$tenure)
summary(churnData$tenure)
#Spliting total charges into three categories Low for cheapest cost, Middle for medium costs and High for the highest chargest
churnData$TotalCharges <-ifelse(churnData$TotalCharges <= 2000, "Low",ifelse(churnData$TotalCharges > 2000 & churnData$TotalCharges <= 6000, "Middle","High"))
churnData$TotalCharges<- as.factor(churnData$TotalCharges)
summary(churnData$TotalCharges)
#spliting monthly charges into three categoreis Low for cheapest cost, Middle for medium costs and High for the highest chargest
churnData$MonthlyCharges <-ifelse(churnData$MonthlyCharges <=25, "Low",ifelse(churnData$MonthlyCharges > 25 & churnData$MonthlyCharges <= 100, "Middle","High"))
churnData$MonthlyCharges<- as.factor(churnData$MonthlyCharges)
summary(churnData$MonthlyCharges)
ggplot(churnData, aes(Contract, fill = SeniorCitizen)) + 
  geom_bar() +
  labs(title = "Customer Contract VS Age Bar Graph",
       x = "Contract",
       y = "Count")
#continous variables turned categories
tenureChurn <- ggplot(churnData, aes(tenure, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Tenure VS Churn Bar",
       x = "Length of Customer Tenure",
       y = "Count")
montlyChargesChurn <- ggplot(churnData, aes(MonthlyCharges, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Monthly Charges VS Churn Bar",
       x = "Length of Customer Monthly Charges",
       y = "Count")
totalChargesChurn <- ggplot(churnData, aes(TotalCharges, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Total Charges Bar",
       x = "Length of Customer Total Charges",
       y = "Count")
grid.arrange(tenureChurn, montlyChargesChurn,totalChargesChurn, ncol=3)
#continous boolean variable turned categorical
SeniorCitizenChurn <- ggplot(churnData, aes(SeniorCitizen, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Senior Citizen VS Churn Bar Graph",
       x = "Senior Citizen",
       y = "Count")
#Categorical Independent Variables 
genderChurn <- ggplot(churnData, aes(gender, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Gender VS Churn Bar Graph",
       x = "Gender",
       y = "Count")
partnerChurn <- ggplot(churnData, aes(Partner, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Partner VS   Bar Graph",
       x = "Partner",
       y = "Count")
dependentsChurn <- ggplot(churnData, aes(Dependents, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Dependents VS Churn Bar Graph",
       x = "Dependents",
       y = "Count")
phoneServicesChurn <- ggplot(churnData, aes(PhoneService, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Phone Service vs Churn Bar Graph",
       x = "Phone Service",
       y = "Count")
multipleLinesChurn <- ggplot(churnData, aes(MultipleLines, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Multiple Lines VS Churn Bar Graph",
       x = "Multiple Lines",
       y = "Count")
InternetServicesChurn <- ggplot(churnData, aes(InternetService, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Internet Service VS Churn Bar Graph",
       x = "Internet Service",
       y = "Count")
onlineSecurityChurn <- ggplot(churnData, aes(OnlineSecurity, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Online Security VS ChurnBar Graph",
       x = "Online Security",
       y = "Count")
onlineBackupChurn <-ggplot(churnData, aes(OnlineBackup, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Online Backup VS Churn Bar Graph",
       x = "Online Backup",
       y = "Count")
DeviceProtectionChurn <- ggplot(churnData, aes(DeviceProtection, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Device Protection VS Churn Bar Graph",
       x = "Device Protection",
       y = "Count")
techSupportChurn <- ggplot(churnData, aes(TechSupport, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Tech Support VS Churn Bar Graph",
       x = "Tech Support",
       y = "Count")
StreamingTVChurn <- ggplot(churnData, aes(StreamingTV, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Streaming TV VS Churn Bar Graph",
       x = "Streaming TV",
       y = "Count")
streamingMoviesChurn <- ggplot(churnData, aes(StreamingMovies, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Streaming Movies VS Churn Bar Graph",
       x = "Streaming Movies",
       y = "Count")
contractChurn <- ggplot(churnData, aes(Contract, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Contract VS Churn Bar Graph",
       x = "Contract",
       y = "Count")
paperlessBillingChurn <- ggplot(churnData, aes(PaperlessBilling, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Paperless Billing VS Churn Bar Graph",
       x = "Paperless Billing",
       y = "Count")
paymentMethodChurn <- ggplot(churnData, aes(PaymentMethod, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Payment Method VS Churn Bar Graph",
       x = "Payment Method",
       y = "Count")

grid.arrange(SeniorCitizenChurn,genderChurn, partnerChurn, dependentsChurn, phoneServicesChurn, multipleLinesChurn, InternetServicesChurn, onlineSecurityChurn, 
onlineBackupChurn, DeviceProtectionChurn, techSupportChurn, StreamingTVChurn, streamingMoviesChurn, contractChurn, paperlessBillingChurn, paymentMethodChurn,ncol=3)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(churnData))

## set the seed to make your partition reproducible
set.seed(101)
train_level <- sample(seq_len(nrow(churnData)), size = smp_size)

train <- churnData[train_level, ]
test <- churnData[-train_level, ]
glimpse(train)
glimpse(test)
#Exporting all three data sets to xlsx
write.xlsx(churnData, "C:/Users/chris/Google Drive/C744/Cleaned Data Set.xlsx")
write.xlsx(train, "C:/Users/chris/Google Drive/C744/Train Data set.xlsx")
write.xlsx(test, "C:/Users/chris/Google Drive/C744/Test Data Set.xlsx")
#univariate Statistic
tenure <- ggplot(train, aes(tenure, fill = tenure)) + 
  geom_bar() +
  labs(title = "Customer Tenure Bar",
       x = "Length of Customer Tenure",
       y = "Count")
MonthlyCharges <- ggplot(train, aes(MonthlyCharges, fill = MonthlyCharges)) + 
  geom_bar() +
  labs(title = "Customer Monthly Charges Bar",
       x = "Length of Customer Monthly Charges",
       y = "Count")
TotalCharges <- ggplot(train, aes(TotalCharges, fill = TotalCharges)) + 
  geom_bar() +
  labs(title = "Customer Total Charges Bar",
       x = "Length of Customer Total Charges",
       y = "Count")
Churn <- ggplot(train, aes(Churn, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Churn Bar Graph",
       x = "Churn",
       y = "Count")
gender <- ggplot(train, aes(gender, fill = gender)) + 
  geom_bar() +
  labs(title = "Customer Gender Bar Graph",
       x = "Gender",
       y = "Count")
SeniorCitizen <- ggplot(train, aes(SeniorCitizen, fill = SeniorCitizen)) + 
  geom_bar() +
  labs(title = "Customer Senior Citizen Bar Graph",
       x = "Senior Citizen",
       y = "Count")

Partner<- ggplot(train, aes(Partner, fill = Partner)) + 
  geom_bar() +
  labs(title = "Customer Partner Bar Graph",
       x = "Partner",
       y = "Count")
Dependents <- ggplot(train, aes(Dependents, fill = Dependents)) + 
  geom_bar() +
  labs(title = "Customer Dependents Bar Graph",
       x = "Dependents",
       y = "Count")
PhoneService <- ggplot(train, aes(PhoneService, fill = PhoneService)) + 
  geom_bar() +
  labs(title = "Customer Phone Service Bar Graph",
       x = "Phone Service",
       y = "Count")
MultipleLines <- ggplot(train, aes(MultipleLines, fill = MultipleLines)) + 
  geom_bar() +
  labs(title = "Customer Multiple Lines Bar Graph",
       x = "Multiple Lines",
       y = "Count")
InternetService <- ggplot(train, aes(InternetService, fill = InternetService)) + 
  geom_bar() +
  labs(title = "Customer Internet Service Bar Graph",
       x = "Internet Service",
       y = "Count")
OnlineSecurity <- ggplot(train, aes(OnlineSecurity, fill = OnlineSecurity)) + 
  geom_bar() +
  labs(title = "Customer Online Security Bar Graph",
       x = "Online Security",
       y = "Count")
OnlineBackup <- ggplot(train, aes(OnlineBackup, fill = OnlineBackup)) + 
  geom_bar() +
  labs(title = "Customer Online Backup Bar Graph",
       x = "Online Backup",
       y = "Count")
DeviceProtection <- ggplot(train, aes(DeviceProtection, fill = DeviceProtection)) + 
  geom_bar() +
  labs(title = "Customer Device Protection Bar Graph",
       x = "Device Protection",
       y = "Count")
TechSupport <- ggplot(train, aes(TechSupport, fill = TechSupport)) + 
  geom_bar() +
  labs(title = "Customer Tech Support Bar Graph",
       x = "Tech Support",
       y = "Count")
StreamingTV <- ggplot(train, aes(StreamingTV, fill = StreamingTV)) + 
  geom_bar() +
  labs(title = "Customer Streaming TV Bar Graph",
       x = "Streaming TV",
       y = "Count")
StreamingMovies <- ggplot(train, aes(StreamingMovies, fill = StreamingMovies)) + 
  geom_bar() +
  labs(title = "Customer Streaming Movies Bar Graph",
       x = "Streaming Movies",
       y = "Count")
Contract <- ggplot(train, aes(Contract, fill = Contract)) + 
  geom_bar() +
  labs(title = "Customer Contract Bar Graph",
       x = "Contract",
       y = "Count")
PaperlessBilling <- ggplot(train, aes(PaperlessBilling, fill = PaperlessBilling)) + 
  geom_bar() +
  labs(title = "Customer Paperless Billing Bar Graph",
       x = "Paperless Billing",
       y = "Count")
PaymentMethod <- ggplot(train, aes(PaymentMethod, fill = PaymentMethod)) + 
  geom_bar() +
  labs(title = "Customer Payment Method Bar Graph",
       x = "Payment Method",
       y = "Count")
grid.arrange(tenure,MonthlyCharges,TotalCharges,Churn,gender,SeniorCitizen,Partner,Dependents,PhoneService,MultipleLines,ncol=3)
grid.arrange(InternetService,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies,Contract,PaperlessBilling,PaymentMethod,ncol=3)
#closer look at categorical variables with more than two categories for univariates 
drillInternetService <- ggplot(train, aes(InternetService, fill = InternetService, group = InternetService)) + 
  geom_bar() +
  facet_wrap(~InternetService) +
  labs(title = "Customer Internet Service Bar Graph",
       x = "Internet Service",
       y = "Count")
drillContract <- ggplot(train, aes(Contract, fill = Contract, group = Contract )) + 
  geom_bar() +
  facet_wrap(~Contract) +
  labs(title = "Customer Contract Bar Graph",
       x = "Contract",
       y = "Count")
drillPaymentMethod <- ggplot(train, aes(PaymentMethod, fill = PaymentMethod, group = PaymentMethod)) + 
  geom_bar() +
  facet_wrap(~PaymentMethod) +
  labs(title = "Customer Payment Method Bar Graph",
       x = "Payment Method",
       y = "Count")
drilltenure <- ggplot(train, aes(tenure, fill = tenure, group = tenure)) + 
  geom_bar() +
  facet_wrap(~tenure) +
  labs(title = "Customer Tenure Bar",
       x = "Length of Customer Tenure",
       y = "Count")
drillMonthlyCharges <- ggplot(train, aes(MonthlyCharges, fill = MonthlyCharges, group = MonthlyCharges)) + 
  geom_bar() +
  facet_wrap(~MonthlyCharges) +
  labs(title = "Customer Monthly Charges Bar",
       x = "Length of Customer Monthly Charges",
       y = "Count")
drillTotalCharges <- ggplot(train, aes(TotalCharges, fill = TotalCharges, group = TotalCharges)) + 
  geom_bar() +
  facet_wrap(~TotalCharges) +
  labs(title = "Customer Total Charges Bar",
       x = "Length of Customer Total Charges",
       y = "Count")
grid.arrange(drillInternetService,drillContract,drillPaymentMethod,drillMonthlyCharges,drillTotalCharges,drilltenure,ncol=2)

#tables for bivariate data
genderTable <- table(train$gender,train$Churn)
names(dimnames(genderTable)) <- c("Gender","Churn")
genderTable

SeniorCitizenTable <- table(train$SeniorCitizen,train$Churn)
names(dimnames(SeniorCitizenTable)) <- c("Senior Citizen","Churn")
SeniorCitizenTable

PartnerTable <- table(train$Partner,train$Churn)
names(dimnames(PartnerTable)) <- c("Partner","Churn")
PartnerTable

DependentsTable <- table(train$Dependents,train$Churn)
names(dimnames(DependentsTable)) <- c("Dependents","Churn")
DependentsTable


PhoneServiceTable <- table(train$PhoneService,train$Churn)
names(dimnames(PhoneServiceTable)) <- c("Phone Service","Churn")
PhoneServiceTable

MultipleLinesTable <- table(train$MultipleLines,train$Churn)
names(dimnames(MultipleLinesTable)) <- c("Multiple Lines","Churn")
MultipleLinesTable

InternetServiceTable <- table(train$InternetService,train$Churn)
names(dimnames(InternetServiceTable)) <- c("Internet Service","Churn")
InternetServiceTable

OnlineSecurityTable <- table(train$OnlineSecurity,train$Churn)
names(dimnames(OnlineSecurityTable)) <- c("Online Security","Churn")
OnlineSecurityTable

OnlineBackupTable <- table(train$OnlineBackup,train$Churn)
names(dimnames(OnlineBackupTable)) <- c("Online Backup","Churn")
OnlineBackupTable

DeviceProtectionTable <- table(train$DeviceProtection,train$Churn)
names(dimnames(DeviceProtectionTable)) <- c("Device Protection","Churn")
DeviceProtectionTable

TechSupportTable <- table(train$TechSupport,train$Churn)
names(dimnames(TechSupportTable)) <- c("Tech Support","Churn")
TechSupportTable

StreamingTVTable <- table(train$StreamingTV,train$Churn)
names(dimnames(StreamingTVTable)) <- c("Streaming TV","Churn")
StreamingTVTable

StreamingMoviesTable <- table(train$StreamingMovies,train$Churn)
names(dimnames(StreamingMoviesTable)) <- c("Streaming Movies","Churn")
StreamingMoviesTable

ContractTable <- table(train$Contract,train$Churn)
names(dimnames(ContractTable)) <- c("Contract","Churn")
ContractTable

PaperlessBillingTable <- table(train$PaperlessBilling,train$Churn)
names(dimnames(PaperlessBillingTable)) <- c("Paperless Billing","Churn")
PaperlessBillingTable

PaymentMethodTable <- table(train$PaymentMethod,train$Churn)
names(dimnames(PaymentMethodTable)) <- c("Payment Method","Churn")
PaymentMethodTable

TenureTable <- table(train$tenure,train$Churn)
names(dimnames(TenureTable)) <- c("Tenure","Churn")
TenureTable

MonthlyChargesTable <- table(train$MonthlyCharges,train$Churn)
names(dimnames(MonthlyChargesTable)) <- c("Monthly Charges","Churn")
MonthlyChargesTable

TotalChargesTable <- table(train$TotalCharges,train$Churn)
names(dimnames(TotalChargesTable)) <- c("Total Charges","Churn")
TotalChargesTable
#
# ggplot(churnData) +
#   aes(x = TotalCharges,y = ..density.., fill= Churn) +
#   geom_histogram() +
#   geom_density() +
#   labs(title = "Customer Total Charges VS Churn Graph",
#        x = "Total Charges",
#        y = "Density")
# 
# ggplot(churnData) +
#   aes(x = MonthlyCharges,y = ..density.., fill= Churn) +
#   geom_histogram() +
#   geom_density() +
#     labs(title = "Customer Monthly Charges VS Churn Graph",
#        x = "Monthly Charges",
#        y = "Density")
# 
# ggplot(churnData) +
#   aes(x = tenure,y = ..density.., fill= Churn) +
#   geom_histogram() +
#   geom_density() +
#   labs(title = "Customer Tenure VS Churn Graph",
#        x = "Tenure",
#        y = "Density")

summary(churnData)
#starting factor mining
library(FactoMineR)
library(factoextra)
library(missMDA)
library(dplyr)
library(gridExtra)
res.mca <- MCA(train, graph = TRUE)
summary(res.mca)

eig.val <- res.mca$eig
summary(eig.val)
# Results for active Variables
res.var <- res.mca$var
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for qualitative supp. variables
res.mca$quali.sup
# Results for active individuals
res.ind <- res.mca$var
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 


 barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red") 
mcaPlot1 <- plot(res.mca, autoLab = "yes")
mcaPlot2 <- plot.MCA(res.mca)
mcaPlot3 <- plot.FAMD(res.mca)
mcaPlot4 <- plot(res.mca,invisible="var",title="Variable") 
 mcaPlot5 <- plot(res.mca,choix="var")
mcaPlot6 <- plot(res.mca,choix="ind")
mcaPlot7 <- plot(res.mca,invisible=c("var","quali.sup"),title="Variable Quantitative") 
mcaPlot8 <- plot(res.mca,invisible=c("var","quanti.sup"),title="Vaiable Quailitative") 
mcaPlot9 <- plot(res.mca,invisible = c("var", "quali.sup", "quanti.sup"),cex = 0.8,autoLab = "yes",title="All Variables")
mcaPlot10 <- plot(res.mca,invisible="ind",title="Individuals")
mcaPlot11 <- plot(res.mca,invisible=c("ind","quali.sup"),title="Individuals Quantative")
mcaPlot12 <- plot(res.mca,invisible=c("ind", "quanti.sup"),title="Individuals Qualitative")
mcaPlot13 <- plot(res.mca,invisible = c("ind", "quali.sup", "quanti.sup"),cex = 0.8,autoLab = "yes",title="All Individuals")
dimdesc(res.mca)
mcaPlot14 <- plotellipses(res.mca,keepvar=test)
grid.arrange(mcaPlot1,mcaPlot2,mcaPlot3,mcaPlot4,ncol=2)
grid.arrange(mcaPlot5,mcaPlot6,mcaPlot7,mcaPlot8,ncol=2)
grid.arrange(mcaPlot9,mcaPlot10,mcaPlot11,mcaPlot12,ncol=2)
grid.arrange(mcaPlot13,mcaPlot14,ncol=2)

###### 
#MCA Test 
# res.mca <- MCA(test, graph = TRUE)
# summary(res.mca)
# #eig.val <- get_eigenvalue((res.mca))
# #cust.mca = MCA(train,quali.sup=20,graph=FALSE)
# # summary(cust.mca)
# #fviz_eig(res.mca, addlabels = TRUE)
# #fviz_screeplot(cust.mca,addlabels=TRUE,ylim=c(0,20))
# eig.val <- res.mca$eig
# summary(eig.val)
# # Results for active Variables
# res.var <- res.mca$var
# res.var$coord          # Coordinates
# res.var$contrib        # Contributions to the PCs
# res.var$cos2           # Quality of representation 
# # Results for qualitative supp. variables
# res.mca$quali.sup
# # Results for active individuals
# res.ind <- res.mca$var
# res.ind$coord          # Coordinates
# res.ind$contrib        # Contributions to the PCs
# res.ind$cos2           # Quality of representation 
# 
# #Graphs
# mcaPlot1 <- plot(res.mca, autoLab = "yes")
# mcaPlot2 <- plot.MCA(res.mca)
# mcaPlot3 <- plot.FAMD(res.mca)
# mcaPlot4 <- plot(res.mca,invisible="var",title="Variable") 
# mcaPlot5 <- plot(res.mca,choix="var")
# mcaPlot6 <- plot(res.mca,choix="ind")
# mcaPlot7 <- plot(res.mca,invisible=c("var","quali.sup"),title="Variable Qualitative") 
# mcaPlot8 <- plot(res.mca,invisible=c("var","quanti.sup"),title="Var Quanitative") 
# mcaPlot9 <- plot(res.mca,invisible = c("var", "quali.sup", "quanti.sup"),cex = 0.8,autoLab = "yes",title="All Variables")
# mcaPlot10 <- plot(res.mca,invisible="ind",title="Individuals")
# mcaPlot11 <- plot(res.mca,invisible=c("ind","quali.sup"),title="Individuals Qualitative")
# mcaPlot12 <- plot(res.mca,invisible=c("ind", "quanti.sup"),title="Individuals Quantative")
# mcaPlot13 <- plot(res.mca,invisible = c("ind", "quali.sup", "quanti.sup"),cex = 0.8,autoLab = "yes",title="All Individuals")
# dimdesc(res.mca)
# mcaPlot14 <- plotellipses(res.mca,keepvar=test)
#######################

#logisticRegression
library(MASS)
library(tidyverse)
library(pscl)
library(ROCR)
library(pROC)
logModel <- glm(formula = Churn~.,family = binomial, data = train)
print(summary(logModel))
anova(logModel, test="Chisq")
pR2(logModel)

fitted.results <- predict(logModel,newdata=subset(test,select=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != train$Churn)
print(paste('Accuracy',1-misClasificError))


predicted <- predict(logModel, train, type="response")
pr <- prediction(predicted, train$Churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prfPlot <- plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
predictedPlot <- plot(predicted)

roc.plotChart <-  plot.roc(train$Churn,predicted,
                    identity.col = "red",
                    print.auc=TRUE,auc.polygon=TRUE,auc.polygon.col="steelblue",
                    main = "ROC Curve for Logistic Regression Model")

grid.arrange(prfPlot,predictedPlot,roc.plotchart,ncol=3)