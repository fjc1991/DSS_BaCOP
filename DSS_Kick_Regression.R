## Logistic Regression
library(car)
library(oddsratio)
library(lmtest)
library(MASS)
library(pscl)

##Data load $ preprocessing

data <- read.csv(file.choose())

summary(data)

data$log_funding_percent <- as.numeric(data$log_funding_percent)
data$log_funding_percent[is.na(data$log_funding_percent)] <- 0

summary(data)
#sd(data$Topic_2_y)

##Logistic regression RC model 1.1

summary(M1 <-glm(data$status ~ images + video + RC_V_hiller_all + RC_V_Polysemy + RC_V_Contronyms + RC_descrip_effect + Sent_count_RC +   
                   RC_Flesch  + RC_sent_score, family = "binomial",data=data)) 


a1<-summary(M1)
a1$aic
exp(coef(M1))
car::vif(M1)


##Logistic regression RC model 1.2

summary(M2 <-glm(data$status ~ video + images + RC_V_hiller_all + Sent_count_RC + RC_descrip_effect + RC_V_Polysemy + RC_V_Contronyms + 
                               RC_Flesch + RC_sent_score + Topic_0_y + Topic_1_y + Topic_2_y, family = "binomial",data=data)) 

b1<-summary(M2)
b1$aic
exp(coef(M2))
car::vif(M2)

##Logistic regression description model 1.2

summary(M3 <-glm(data$status ~ images + video + Des_V_hiller_all + Des_V_Polysemy + Des_V_Contronyms + des_descrip_effect + Sent_count_des +   
                                        Flesch_des  + des_sent_score, family = "binomial",data=data)) 

c1<-summary(M3)
c1$aic
exp(coef(M3))
car::vif(M3)

#############################################################################
##Linear regression

##OLS Regression RC model 1.1

summary(M4 <-lm(data$log_funding_percent ~ images + video + RC_V_hiller_all + RC_V_Polysemy + RC_V_Contronyms + RC_descrip_effect + Sent_count_RC +   
                                           RC_Flesch  + RC_sent_score, data=data))

d1<-summary(M4)
d1
d2<-round(d1$coefficients,4)
d2[d2[,4] < 0.05,1] <- paste0(d2[d2[,4] < 0.05,1],"*")
d2[d2[,4] < 0.01,1] <- paste0(d2[d2[,4] < 0.01,1],"*")
d2[d2[,4] < 0.001,1] <- paste0(d2[d2[,4] < 0.001,1],"*")
d2<-rbind(d2[,1:2],c("Adj R-square",d1$adj.r.squared))
car::vif(M4)
write.csv(d2,"OLS_RC_M1_1.csv")

##OLS Regression RC model 1.2

summary(M5 <-lm(data$log_funding_percent ~ video + images + RC_V_hiller_all + Sent_count_RC + RC_descrip_effect + RC_V_Polysemy + 
                                           RC_V_Contronyms + RC_Flesch + RC_sent_score + Topic_0_y + Topic_1_y + Topic_2_y, data=data))

e1<-summary(M5)
e1
e2<-round(e1$coefficients,4)
e2[e2[,4] < 0.05,1] <- paste0(e2[e2[,4] < 0.05,1],"*")
e2[e2[,4] < 0.01,1] <- paste0(e2[e2[,4] < 0.01,1],"*")
e2[e2[,4] < 0.001,1] <- paste0(e2[e2[,4] < 0.001,1],"*")
e2<-rbind(e2[,1:2],c("Adj R-square",e1$adj.r.squared))
car::vif(M5)
write.csv(e2,"OLS_RC_M1_2.csv")

##OLS Regression description model 1.1

summary(M6 <-lm(data$log_funding_percent ~ images + video + Des_V_hiller_all + Des_V_Polysemy + Des_V_Contronyms + des_descrip_effect + Sent_count_des +   
                                           Flesch_des  + des_sent_score, data=data))

f1<-summary(M6)
f1
f2<-round(d1$coefficients,4)
f2[f2[,4] < 0.05,1] <- paste0(f2[f2[,4] < 0.05,1],"*")
f2[f2[,4] < 0.01,1] <- paste0(f2[f2[,4] < 0.01,1],"*")
f2[f2[,4] < 0.001,1] <- paste0(f2[f2[,4] < 0.001,1],"*")
f2<-rbind(f2[,1:2],c("Adj R-square",f1$adj.r.squared))
car::vif(M6)
write.csv(f2,"OLS_DES_M1_1.csv")


