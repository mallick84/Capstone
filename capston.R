setwd("G:\\MY Project\\Jigsaw\\R\\Capston")                                                                                                                               
tele<-read.csv("telecomfinal.csv", header = TRUE)

library(dplyr)
library(ggplot2)
library(irr)
library(caret)

dim(tele)

#Our main target variable is the CHURN and since it is having 
#categorical variable so we will go for logistic regression

qplot(y=tele$churn, x=tele$mou_Mean, col="red")

#making another datasheet that will be more relevent to churn thus simplyfy our problem
tele1<-tele%>%select(Customer_ID,actvsubs,adjrev, adjmou, avgmou, avgrev,avgqty,age1,age2,blck_dat_Mean,
                     callwait_Mean, callwait_Range, change_mou, children,comp_vce_Mean,custcare_Mean,
                     datovr_Mean,da_Mean, drop_blk_Mean,drop_dat_Mean,drop_vce_Mean,eqpdays,forgntvl,
                     hnd_webcap,income,marital,months,mou_Mean,prizm_social_one,ovrmou_Mean,ovrrev_Mean,
                     retdays,rev_Range,roam_Mean,totcalls,totrev,wrkwoman,asl_flag,dwlltype,refurb_new,
                     mtrcycle,truck,hnd_price,models,numbcars,plcd_vce_Mean, churn)

dim(tele1)
str(tele1)
summary(tele1)

#delet the missing values of mou_mean and eqpdays.
tele2<-tele1%>%filter(!is.na(mou_Mean))%>%filter(!is.na(eqpdays))

summary(tele2$mou_Mean)

#Imputing the missing values with the averages of the variable
tele3<-tele2%>%mutate_all(funs(replace(.,which(is.na(.)), mean(.,na.rm = TRUE))))

summary(tele3)

#Some more data preparation 

tele3$children<-ifelse(tele3$children=="Y",1,0)
tele3$asl_flag<-ifelse(tele3$asl_flag=="Y",1,0)
tele3$dwlltype<-ifelse(tele3$dwlltype=="M",1,0)
tele3$refurb_new<-ifelse(tele3$refurb_new=="N",1,0)
tele3$wrkwoman<-ifelse(tele3$wrkwoman=="Y",1,0)




tele4<-tele3%>%mutate(urban=ifelse(prizm_social_one=="U",1,0),city=ifelse(prizm_social_one=="C",1,0),
                     suburban=ifelse(prizm_social_one=="S",1,0),town=ifelse(prizm_social_one=="T",1,0),
                     married=ifelse(marital=="M",1,0),married_A=ifelse(marital=="A",1,0),
                     married_B=ifelse(marital=="B",1,0),married_S=ifelse(marital=="S",1,0),
                     webcap=ifelse(hnd_webcap=="WCMB",1,0))%>%select(-prizm_social_one,-marital,-hnd_webcap)

#new variable are added

tele5<-tele4%>%mutate(change_per_call=totrev/totcalls, change_per_min=adjrev/adjmou, 
                      completion_percentage=comp_vce_Mean/plcd_vce_Mean)



summary(tele6$completion_percentage)

#again impute the missing value
tele6<-tele5%>%replace(is.na(.),0)

#variable with only missing value
apply(is.na(tele6),2,sum)

str(tele6)

# finding if there is any infinite value in tele6, since step function will not run.

apply(is.na(tele6), 2, sum)

summary(tele6$change_per_call)

summary(tele6$change_per_min)

summary(tele6)

tele6%>%filter(change_per_min=="infinite")%>%nrow()

range(tele6$change_per_call)

# replacing the infinite value with the NA values

is.na(tele6)<-sapply(tele6,is.infinite)

# Now replace the NA values with the their mean value

for(i in 1:ncol(tele6)){
  tele6[is.na(tele6[,i]),i]<-mean(tele6[,i],na.rm = TRUE)
}

summary(tele6)

#splitting data into test and train.
set.seed(42)
index<-sample(nrow(tele6), 0.7*nrow(tele6), replace = FALSE)
train<-tele6[index,]
test<-tele6[-index,]

#Build the first model using all the variables 

attach(tele6)
model1<-glm(formula =churn~ actvsubs+ adjrev+ adjmou+ avgmou+ avgrev+avgqty+age1+age2+blck_dat_Mean+
              callwait_Mean+ callwait_Range+ change_mou+ children+comp_vce_Mean+custcare_Mean+
              datovr_Mean+da_Mean+ drop_blk_Mean+drop_dat_Mean+drop_vce_Mean+eqpdays+forgntvl +
              income+months+mou_Mean+ovrmou_Mean+ovrrev_Mean+
              retdays+rev_Range+roam_Mean+totcalls+totrev+wrkwoman+asl_flag+dwlltype+refurb_new+
              mtrcycle+truck+hnd_price+models+numbcars+
              completion_percentage+ change_per_call +change_per_min, data = train[,-1], family = "binomial")

summary(model1)

#doing stepwise logistic reg so let R choose the perfect combination
step(model1, direction = "both")


#doing iteration method model2, to get less P-value

model2<-glm(formula =churn~ actvsubs+ adjrev+ adjmou+ avgmou+ avgrev+avgqty+age1+age2+
              #blck_dat_Mean+
              #callwait_Mean+ 
              #callwait_Range+ 
              change_mou+ children+comp_vce_Mean+custcare_Mean+
              #datovr_Mean+
              #da_Mean+ 
              drop_blk_Mean+
              #drop_dat_Mean+
              drop_vce_Mean+eqpdays+
              #forgntvl+
              #income+
              months+mou_Mean+ovrmou_Mean+ovrrev_Mean+
              retdays+rev_Range+roam_Mean+totcalls+totrev+wrkwoman+asl_flag+dwlltype+refurb_new+
              mtrcycle+truck+hnd_price+models+numbcars+change_per_call+change_per_call+change_per_min,data = train[,-1], family = "binomial")

summary(model2)

#doing iteration method model3, to get less P-value

model3<-glm(formula =churn~ actvsubs+ adjrev+ adjmou+ avgmou+ avgrev+avgqty+age1+age2+
              #blck_dat_Mean+
              #callwait_Mean+ 
              #callwait_Range+ 
              change_mou+ children+comp_vce_Mean+custcare_Mean+
              #datovr_Mean+
              #da_Mean+ 
              drop_blk_Mean+
              #drop_dat_Mean+
              drop_vce_Mean+eqpdays+
              #forgntvl+
              #income+
              months+mou_Mean+ovrmou_Mean+ovrrev_Mean+
              retdays+rev_Range+roam_Mean+
              #totcalls+
              totrev+wrkwoman+asl_flag+dwlltype+refurb_new+
              #mtrcycle+truck+
              hnd_price+models
            #+numbcars
            + change_per_call +change_per_min,data = train[,-1], family = "binomial")

summary(model3)

#doing iteration method model4, to get less P-value

model4<-glm(formula =churn~ actvsubs+ adjrev+ 
              #adjmou+ 
              avgmou+ avgrev+
              #avgqty+
              age1+age2+
              #blck_dat_Mean+
              #callwait_Mean+ 
              #callwait_Range+ 
              change_mou+ children+comp_vce_Mean+custcare_Mean+
              #datovr_Mean+
              #da_Mean+ 
              #drop_blk_Mean+
              #drop_dat_Mean+
              drop_vce_Mean+eqpdays+
              #forgntvl+
              #income+
              months+mou_Mean+
              #ovrmou_Mean+
              ovrrev_Mean+
              retdays+rev_Range+roam_Mean+
              #totcalls+
              #totrev+
              wrkwoman+asl_flag+dwlltype+refurb_new+
              #mtrcycle+truck+
              hnd_price+models
            #+numbcars
            + change_per_call +change_per_min,data = train[,-1], family = "binomial")

summary(model4)


#doing iteration method model5, to get less P-value

model5<-glm(formula =churn~ actvsubs+ adjrev+ 
              #adjmou+ 
              avgmou+ 
              #avgrev+
              #avgqty+
              age1+
              #age2+
              #blck_dat_Mean+
              #callwait_Mean+ 
              #callwait_Range+ 
              change_mou+ children+comp_vce_Mean+custcare_Mean+
              #datovr_Mean+
              #da_Mean+ 
              #drop_blk_Mean+
              #drop_dat_Mean+
              drop_vce_Mean+eqpdays+
              #forgntvl+
              #income+
              months+mou_Mean+
              #ovrmou_Mean+
              ovrrev_Mean+
              retdays+rev_Range+roam_Mean+
              #totcalls+
              #totrev+
              wrkwoman+asl_flag+dwlltype+refurb_new+
              #mtrcycle+truck+
              hnd_price+models
            #+numbcars
            + change_per_call +change_per_min,data = train[,-1], family = "binomial")

summary(model5)

str(tele6)
tele6$change_per_call<-as.numeric(tele6$change_per_call)


model6<-glm(formula =churn~ actvsubs+ adjrev+ 
              #adjmou+ 
              avgmou+ 
              #avgrev+
              #avgqty+
              age1+
              #age2+
              #blck_dat_Mean+
              #callwait_Mean+ 
              #callwait_Range+ 
              change_mou+ children+comp_vce_Mean+custcare_Mean+
              #datovr_Mean+
              #da_Mean+ 
              #drop_blk_Mean+
              #drop_dat_Mean+
              drop_vce_Mean+eqpdays+
              #forgntvl+
              #income+
              months+mou_Mean+
              #ovrmou_Mean+
              ovrrev_Mean+
              retdays+
              #rev_Range+
              roam_Mean+
              #totcalls+
              #totrev+
              wrkwoman+
              asl_flag+dwlltype+refurb_new+
              #mtrcycle+truck+
              hnd_price+models
            #+numbcars
            + change_per_call +change_per_min,data = train[,-1], family = "binomial")

summary(model6)



model7<-glm(formula =churn ~ actvsubs + 
              #adjrev + 
              avgmou + 
              #avgrev + 
              age1 + change_mou + 
              children + 
              #comp_vce_Mean + 
              custcare_Mean + 
              #datovr_Mean + 
              #drop_blk_Mean + 
              drop_vce_Mean + eqpdays + 
              #income + months + 
              mou_Mean + ovrrev_Mean + retdays + #rev_Range + 
              roam_Mean + 
              totrev + 
              #wrkwoman + 
              asl_flag + dwlltype + refurb_new + hnd_price + 
              models + change_per_call + change_per_min +completion_percentage
            ,data = train[,-1], family = "binomial")

summary(model7)


# fixing the model7 after iteration. Here we are getting all the P-values is less than )0.05

#prediction of model by finding the fitted value on train data set, we can also do on test data set also. 



train$predicted<-model7$fitted.values

head(train$predicted)
#or

train$predicted<-predict(model7,type = "response",newdata = train)


# to decide the cut off value of prediction matrix, we have to fist find out the proportion of churn

#now see probability of churn 1 and churn 0

table(tele6$churn)/nrow(tele6)


#here I have seen out of all data churn data is 0.2388.
#i.e all the customer having probability value greater that 0.2388 are having churn
#now choose only the churn having cutoff value 0.399

train$pcut<-ifelse(train$predicted>=0.2388263, 1,0)
View(train)



#calibrating with kappa from library irr measure
kappa2(data.frame(train$churn, train$pcut)) #kappa should > 0.62 for good model

# now do another check confusion matrix from library caret.


table(train$pcut,train$churn)


#Plotting ROC Curve
library(ROCR)
?performance
# The prediction function of the ROCR library basically creates a structure to validate 
#our predictions qirh actual values

pred<-prediction(train$predicted, train$churn)

# "tpr" and "fpr" are arguments of the "performance" function indicating that the plot is 
#between the true positive rate and the false positive rate.

perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

#now finding the AUC value ..it should ve >0.5

auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc




