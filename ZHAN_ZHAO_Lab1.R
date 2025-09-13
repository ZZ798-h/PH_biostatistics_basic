#Q1&Q2
dim(tg)

#Q3
is.factor(tg$drinker_cat)
levels(tg$drinker_cat)
table(tg$drinker_cat)

#Q4
levels(tg$sex)
table(tg$sex)

#Q5
T1 <- table(tg$idu_ever)
prop.table(T1) * 100

#Q6
T_all<-table(tg$idu_ever, tg$sex)
table(tg$idu_ever, tg$sex)

#Q7
T_m<-table(tg$idu_ever[tg$sex =="Male"])
prop.table(T_m)*100

#Q8
T_Fm<-table(tg$idu_ever[tg$sex =="Female"])
prop.table(T_Fm)*100

#Q9
summary(tg$weight_kg)

#Q10
G_hisW<-hist(tg$weight_kg)

#Q11
G_boxpW<-boxplot(tg$weight_kg,main="boxplot_of_weight_kg",xlab = "weight_kg",ylab="Frequency")

#Q12
B_compareMF<-boxplot(weight_kg ~ sex, data = tg,main = "Weight by Sex",xlab = "Sex", ylab = "Weight (kg)",col = c("red","yellow"))
summary(tg$weight_kg[tg$sex=="Male"])
summary(tg$weight_kg[tg$sex=="Female"])
