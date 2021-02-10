############################### Project 7 : Healthcare Cost Analysis ##############################

#import required libraries
library(ggplot2)
library(dplyr)
#set the working directory
setwd("C:\\Users\\IDH\\Desktop\\simplilearn\\Master's Courses\\
      Data science with R\\Project\\Projects for Submission\\Healthcare\\Healthcare\\")
getwd()

#Read  and understand the dataset
healthcare <- read.csv("HospitalCosts.csv")
df <- data.frame(healthcare)
head(df)
str(df)
summary(df)
plot_missing(df)

#install.packages("DataExplorer")
library(DataExplorer)
plot_str(df)

is.na(df)

plot_missing(df)

sum(is.na(df$RACE))

table(df$RACE)
df$RACE <- ifelse(test=is.na(df$RACE),1,df$RACE)
sum(is.na(df$RACE))





#1. To record the patient statistics,
#the agency wants to find the age category of people who frequent the hospital and has the maximum expenditure.
age=table(healthcare$AGE)
age                            # new born babies has highest ffrequency(307) of getting hospitalized
View(age)

#visualization
library(ggplot2)
ggplot(df,aes(AGE))+geom_histogram(binwidth = 1,color="Black",fill="Darkgreen")+
  labs(x="Age",y="Frequency of getting hospitalized",title = "Patient Statistics")



#gives same result that newborns(0-1 yr age) have higher frequency of getting hospitalized


#Now we will find the maximum expenditure 
df1 <- (aggregate(TOTCHG~AGE,data=df,sum))
head(df1)
ggplot(df1,aes(x=AGE,y=TOTCHG))+geom_bar(stat = "Identity",fill="Darkblue")+
  labs(x="Age",y="Total Expenditure",title = "Total expenditure of patients with different ages")




df1 <- (aggregate(TOTCHG~AGE,data=df,sum))
freq<-table(df$AGE)
df1$frequency <- freq
df1$Age_group <- cut(df1$AGE,seq(0,18,6),right = F,labels = c("0-6","7-12","13-18"))
G1 <-ggplot(df1,aes(x=Age_group,y=TOTCHG))+geom_bar(stat = "Identity",fill="Darkblue")+
  labs(x="Age",y="Total Expenditure",title = "Total expenditure of patients ")
G2 <- ggplot(df1,aes(x=Age_group,y=frequency))+geom_bar(stat = "Identity",fill="Darkblue")+
  labs(x="Age",y="Patient frequency",title = "Frequency of patients ")
G1
G2
#So, new born babies have highest frequency of getting hospitalized 
#and has highest expenditure of 678118 
#To plot different plots together,install cowplot library
install.packages("cowplot")
library(cowplot)
plot_grid(G1,G2,label_x = 0.8)




#2. In order of severity of the diagnosis and treatments and to find out the expensive treatments, 
#the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.

freq_1 <- data.frame(table(healthcare$APRDRG))
freq_1$Var1[which.max(freq_1$Freq)]
# group no. 640 has highest freq of hospitalization


#Let's find maximum total expenditure of diagnosis-related groups
freq_2 <- aggregate(TOTCHG ~ APRDRG ,data = df,sum)
head(freq_2)
max(freq_2$TOTCHG)     # maximum expenditure is of 437978
#to find which group has highest expenditure of 437978
freq_2$APRDRG[which.max(freq_2$TOTCHG)]






#3.To make sure that there is no malpractice,
#the agency needs to analyze if the race of the patient is related to the hospitalization costs.

#To have visual understanding we'll first plot a scatter plot to see if there is any relation between RACE of patient and hospitalization cost
head(df$RACE)
table(df$RACE)
boxplot(df$TOTCHG~df$RACE,xlab="Race of Patient",ylab="Hospitalization cost")


ggplot(df,aes(x=RACE,y=TOTCHG))+geom_point(fill="Grey",color="Blue")
#the graphs show that there is no relation between Race of patient and his/her hospitalization cost
#we can also confirm by applying linear model to the data

model <- lm(formula = TOTCHG~RACE,data=df)
model
summary(model)

# from the summary we can say that Race of patient does not put any contribution to the hospitalization cost
#further there is huge residual standard error which shows that there is no relation between the race of pationt and hospitalization cost

#finally to check the relation between the two variables under study we apply t-test
#H0=Null hypothesis : There is a relation between race of the patient and the hospitalization cost
#Ha=Alternate hypothesis : There is no relation between race of the patient and the hospitalization cost


test= t.test(healthcare$RACE,healthcare$TOTCHG,conf.level = 0.95)
test
if(test$p.value < 0.05){
  print("Alternative Hypothesis : 
        There is no relationship between race of patient and hospitalization cost")
}else{
  print("Null hypothesis : 
        There is a reationship between race of the patient and the hospitalization cost")
}

# we are 95% sure that There is no relationship between the race and hospitalization cost which makes sure that there is no malpractice in the hospital







#4. To properly utilize the costs, 
#the agency has to analyze the severity of the hospital costs by age and gender for the proper allocation of resources.

regressor <- lm(formula = TOTCHG ~ FEMALE + AGE,data=df)
regressor
summary(regressor)


# from the p-value we can say that Hospital cost is severely affected by Age of the patient rather than the gender of the patient. But Gender is also statisticlly significant in the analysis of the model
# also the coeficient for FEMALE is negative which says that the hospitalization cost reduces for females compared to males

df2 <- data.frame(aggregate(TOTCHG~FEMALE,data=df,sum))
df2
df2$GENDER <- ifelse(df2$FEMALE==0,"Male","Female")
ggplot(df2,aes(x=GENDER,y=TOTCHG))+geom_bar(stat = "Identity",color="Black",fill="Darkgreen")+
  labs(x="Gender",y="Hospitalization Cost")

# hospitalization cost of males id higher than that of females
#from the graph G1 we have a knowledge that 0-6 yr olds has higher hospitalization cost
df1 <- (aggregate(TOTCHG~AGE+GENDER,data=df,sum))
df1
freq<-table(df$AGE)
df1$frequency <- freq


df1$Age_group <- cut(df1$AGE,seq(0,18,6),right = F,labels = c("0-6","7-12","13-18"))

#CheckG1 <-ggplot()+geom_bar(df1,aes(x=Age_group,y=TOTCHG),stat = "Identity",fill="Darkblue")+labs(x="Age",y="Total Expenditure",title = "Total expenditure of patients with different ages")+
  geom_bar(stat="Identity",df1,aes(x=GENDER,y=TOTCHG),beside=T)

G1








#5.Since the length of stay is the crucial factor for inpatients,
#the agency wants to find if the length of stay can be predicted from age, gender, and race.


View(healthcare)

model1 <-lm(formula = LOS~AGE+FEMALE+RACE,data=df)
summary(model1)


# None of the AGE, GENDER and RACE can predict Length of stay of that patient






#6.To perform a complete analysis,
#the agency wants to find the variable that mainly affects the hospital costs.

model2 <- lm(formula = TOTCHG~.,data=df)
summary(model2)

# On the basis of the statistical significace of variables,with 95% confidence interval, 
#we can say that gender and race don't afect hospital cost 
#so, we will remove these variables and check our model

model3 <- lm(formula = TOTCHG~AGE+LOS+APRDRG,data=df)
summary(model3)

