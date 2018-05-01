rm(list=ls())

################
###Question 1###
################
#Expected probabilities are 0.5 for each sample.
proptest.hg <- function(succ, n, stat.type=""){
  p.hat=(succ[1] + succ[2])/ (n[1] + n[2])
  p.hat.1=succ[1]/n[1]
  p.hat.2= succ[2]/n[2]
  exp.value.1=n[1]*0.5
  exp.value.2=n[2]*0.5
  x=(p.hat.1-p.hat.2)
  y=sqrt(((p.hat *(1-p.hat))*((1/n[1])+(1/n[2]))))
  
  (if(stat.type=="z"){
    test.stat=(x/y)
  } else {
    test.stat=((x/y)^2)})
  p.value=2*(1-pnorm(abs(test.stat)))
  cat("The test statistic, p-value, sample proportion 1, sample proportion 2, stat type are as follows :", test.stat, p.value, p.hat.1, p.hat.2, stat.type)
}

proptest.hg(c(23,25), c(40,50),stat.type = "z")
proptest.hg(c(23,25), c(40,50),stat.type = "chisq")
prop.test(c(23,25), c(40,50), corr=F)


for (n in c(10^2,10^4,10^6)){
  X<-table(sample(c(0,1), n, replace=T,
                  prob=c(0.82, 0.18)))
  total<- (X[names(X)==0])
  phat<- (X[names(X)==0])/n
  Y<-table(sample(c(0,1), n, replace=T,
                  prob=c(0.26, 0.74)))
  total1<- (Y[names(Y)==0])
  phat1<- (Y[names(Y)==0])/n
  print(" p1=0.43  p2=0.52")
  cat("sample size:", n, " successes:", c(total, total1), " phat:", c(phat, phat1), " ")
}

for (n in c(10^2,10^4,10^6)){
  X<-table(sample(c(0,1), n, replace=T,
                  prob=c(0.43, 0.57)))
  total<- (X[names(X)==0])
  phat<- (X[names(X)==0])/n
  Y<-table(sample(c(0,1), n, replace=T,
                  prob=c(0.52, 0.48)))
  total1<- (Y[names(Y)==0])
  phat1<- (Y[names(Y)==0])/n
  print(" p1=0.43  p2=0.52")
  cat("sample size:", n, " successes:", c(total, total1), " phat:", c(phat, phat1), " ")
}

for (n in c(10^2,10^4,10^6)){
  X<-table(sample(c(0,1), n, replace=T,
                  prob=c(0.3, 0.7)))
  total<- (X[names(X)==0])
  phat<- (X[names(X)==0])/n
  Y<-table(sample(c(0,1), n, replace=T,
                  prob=c(0.3, 0.7)))
  total1<- (Y[names(Y)==0])
  phat1<- (Y[names(Y)==0])/n
  print(" p1=0.43  p2=0.52")
  cat("sample size:", n, " successes:", c(total, total1), " phat:", c(phat, phat1), " ")
}

#As the sample sizes increase, the p values get closer and closer to the actual 
#proportions of the underlying distribution.

################
###Question 2###
################

#Ho: p1-p2=0; Ha: p1-p2=/0
#Fisher's exact test is better for smaller samples. 

fisher.test(matrix(c(23,18,7,13),2))
prop.test(c(23,18), c(30,31), corr=F)

#Fisher's p-value = 0.1737
#Chisq's p-value = 0.1218
#Fisher's p-value is higher indicating less significance than Chisquar
#but both are ultimately insignificant conclusions.
#We do not have enough evidence to reject the null hypothesis that
#the two proportions of healed by Pirenzepine vs healed with Trithiozine
#are different.

#The confidence interval is computed by inverting the hypotheis test.
#It represents the values of po that don't reject Ho against Ha at 
#significance level alpha.

################
###Question 3###
################

nyse<-read.csv("/Users/hallehghaemi/Desktop/nyse.csv", header=T)
attach(nyse)
nyse <- nyse[c("price","return")]
nyse.noNA <- na.omit(nyse)
nyse.lm <- lm(return~price, data=nyse.noNA)
summary(nyse.lm)

#There is not a significant relationship between the explanatory
#and the response because the p-values are > 0.05 (alpha)

#plot(nyse.lm)
#The data points in the residuals vs fitted are not completely random 
#as there are clusters and outliers so this plot doesn't point
#to a normal distribution.
#The QQ plot shows outliers on the upper and lower ends of the
#data indicating that the data does not follow a normal distribution.

nyse.lm.new <- lm(return~price, data=nyse.noNA, subset = c(-615, -616) )
#plot(nyse.lm.new)

#Although there are less outliers on the QQ plot, the residuals vs fitted
#plot is still not showing complete randomness pointing to a continued
#distribution pattern that deviates from normality

################
###Question 4###
################

#	US Judge Ratings
judge.ratings<-read.csv("/Users/hallehghaemi/Desktop/USJudgeRatings.csv", header=T)
attach(judge.ratings)

plot(judge.ratings)
#There appear to be many collinearities. One is between INTG and DMNR.
#A very strong collinearity appears between PREP and FAMI

judge.ratings.lm<- lm(CONT~INTG+
                        DMNR+DILG+CFMG+DECI+
                        PREP+FAMI+ORAL+WRIT+
                        PHYS+RTEN, data=judge.ratings)
summary(judge.ratings.lm)

#CFMG demonstrates significance with a p-value of 0.0331. The overall
#model also seems significant with a p-value of 0.03327.

#step(judge.ratings.lm)

#Step:  AIC=-14.65
#CONT ~ DMNR + DILG + CFMG + ORAL + WRIT + PHYS

#Df Sum of Sq    RSS     AIC
#<none>              22.087 -14.646
#- DILG  1    1.2471 23.334 -14.285
#- PHYS  1    1.8506 23.938 -13.187
#- DMNR  1    2.3348 24.422 -12.326
#- ORAL  1    2.4097 24.497 -12.194
#- WRIT  1    3.1958 25.283 -10.836
#- CFMG  1    5.8131 27.900  -6.600

#The step-wise variable selection procedure revealed that 
#DMNR + DILG + CFMG + ORAL + WRIT + PHYS are the most 
#optimal subset to describe CONT. 