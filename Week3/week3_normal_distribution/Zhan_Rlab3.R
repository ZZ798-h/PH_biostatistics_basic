pnorm(1.96)
#pnorm(), qnorm(), dnorm(), calculate quantities for normal distributions
#pnorm(x)


qnorm(0.975)
#Finding the critical z-score for a given area under the curve to the left of the z-score
#(i.e. reading Table A backwards)
#qnorm(p)

#Q1
#Part1

#Q1 The BMI of American adults follows a Normal Distribution with mean 26.5 and standard deviation 4.2. Use R to: 
#a. Find the probability that a randomly selected adult is underweight 
#(i.e. has a BMI less than 18.5).

pnorm(18.5,mean=26.5,sd=4.2)

#b.Find the probability that a randomly selected adult is overweight or obese (i.e. has a BMI greater than 25.0)

pnorm(25.0,mean = 26.5,sd=4.2)
p2<-1-pnorm(25.0,mean = 26.5,sd=4.2)

#c.Find the proportion of American adults who have a healthy weight (i.e. a BMI between 18.5 and 25.0)

P_health<-pnorm(25.0,mean = 26.5,sd = 4.2)-pnorm(18.5,mean = 26.5,sd = 4.2)

#Q2
#a.Find the critical z-score such that 30% of the area under the normal curve is to the left of this value.

qnorm(0.3)


#b.Find the BMI level such that 30% of American adults have a BMI less than that number.


qnorm(0.3,mean = 26.5,sd = 4.2)

#Part 2:
#For Part 2 of this lab, we will return to the tango dataset from Lab 1. 
#Preparation:
#Create a new folder on your computer called Lab 3.
#Download the dataset tango.csv from Canvas into the Lab 3 folder (or copy/paste the tango.csv dataset from your Lab 1 folder to your Lab 3 folder).

#1. Read the dataset tango.csv into R, naming it tg.

tg<-read.csv("/Users/zhaozhan/Downloads/Tufts\ University/principles\ of\ Biostatistics/week3/IN_class/tango-5.csv")

#2. Conduct an exploratory data analysis of the variable High Density Lipoprotein (HDL) Cholesterol (hdl). 
#This should include both numerical and graphical summaries. 
#Write a very short paragraph describing what this variable is (you may use Google to learn the basics of HDL and the units it is measured in) 
#and the distribution of this variable in this dataset.

#Hint
#For numerical summaries, if the variable is fairly symmetric, then the mean and standard deviation are appropriate measures of center and spread. If the distribution is skewed, then median and IQR are more appropriate measures of center and spread. 

hist(tg$hdl)
summary(tg$hdl)
IQR(tg$hdl,na.rm = TRUE)
sd(tg$hdl,na.rm = TRUE)
# The variable is approximately symmetric.And the sd is 13.34 ,mean is 43.51.

#Look for outliers, and if there are any, look into them.

boxplot(tg$hdl,
        xlab = "hdl status",
        ylab = "number of population",
        outline = TRUE)
iqr_hdl<-IQR(tg$hdl,na.rm = TRUE)
Q1<-35.00
Q3<-52.00
lowerH<-Q1-1.5*iqr_hdl
upperH<-Q3+1.5*iqr_hdl
OUtlierH<-tg$hdl[tg$hdl<lowerH|tg$hdl>upperH]

#A nice graphic includes an appropriate title and properly labeled x- and y-axes (including correct measurement units where appropriate).
hist_Hdl <- hist(tg$hdl,
                 main = "Distribution of HDL Cholesterol Levels",
                 xlab = "HDL (mg/dL)",
                 ylab = "Frequency")




#3. (Re)draw a histogram of hdl, and superimpose the normal curve on it.
#Syntax: hist(tg$hdl, freq=FALSE)
#curve(dnorm(x, mean(tg$hdl, na.rm=TRUE), sd(tg$hdl, na.rm=TRUE)), add=TRUE)
#freq = false ; to make the ylab show density ,not frequncy.all area =1 .

#dnorm () to measure the normal distribution  probability density function.


hist_Hdl2 <- hist(tg$hdl,
                  freq = FALSE,
                  main = "Distribution of HDL Cholesterol Levels",
                  xlab = "HDL (mg/dL)",
                  ylab = "Density")

curve(dnorm(x,
            mean = mean(tg$hdl, na.rm = TRUE),
            sd   = sd(tg$hdl, na.rm = TRUE)),
      add = TRUE,
      col = "red",
      lwd = 2)

#Using the relevant numerical summaries from Question 2, 
#if HDL is exactly normally distributed, 
#what percentage of subjects would you expect to have HDL cholesterol between 40 and 60 mg/dL, inclusive?

pnorm(40,mean = mean(tg$hdl,na.rm = TRUE),sd=sd(tg$hdl,na.rm = TRUE))

pnorm(60,mean = mean(tg$hdl,na.rm = TRUE),sd=sd(tg$hdl,na.rm = TRUE))

P_46<-pnorm(60,mean = mean(tg$hdl,na.rm = TRUE),sd=sd(tg$hdl,na.rm = TRUE))-pnorm(40,mean = mean(tg$hdl,na.rm = TRUE),sd=sd(tg$hdl,na.rm = TRUE))

#5. What percentage of subjects in our sample have HDL in this range? 
#Syntax: sum(tg$hdl >= 40 & tg$hdl <= 60)

s46<-sum(tg$hdl>=40 & tg$hdl<=60,na.rm = TRUE)
nrow(tg)

P_sa<-(s46/nrow(tg))*100


#6.If HDL is exactly normally distributed, what percentage of subjects would you expect to have HDL cholesterol in the desirable range (i.e. greater than 60 mg/dL)?


pnorm(60,
      mean = mean(tg$hdl, na.rm = TRUE),
      sd   = sd(tg$hdl, na.rm = TRUE))

P_L60<-pnorm(60,
             mean = mean(tg$hdl, na.rm = TRUE),
             sd   = sd(tg$hdl, na.rm = TRUE))
P_G60<-(1-P_L60)*100



#7. Assume the population of interest has the same mean and standard deviation for HDL as this sample. What is the probability that the sample mean, for a sample of 30 participants, has HDL cholesterol in the desirable range (i.e. greater than 60 mg/dL)?

St.er1<-(sd(tg$hdl,na.rm = TRUE)/sqrt(30))
z1<-(60-mean(tg$hdl,na.rm = TRUE))/St.er1
z2<-1-z1
Pg60<-pnorm(z2)*100

#8.What percentage of subjects in our sample have HDL in the desirable range?

S_desriable<-sum(tg$hdl>60,na.rm = TRUE)
S_tol<-nrow(tg)
P_desirable<-(S_desriable/S_tol)*100

