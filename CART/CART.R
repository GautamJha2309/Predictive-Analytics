library(dplyr)

buys_comp = read.csv("D:/College/SEN-6/PA_LAB/buys_comp.csv") #convert char to factor        , stringsAsFactors = TRUE
View(buys_comp)
str(buys_comp)
dim(buys_comp)
glimpse(buys_comp)


#CLASS:
c1 <- as.data.frame(buys_comp%>%
                      filter(Buys_Computer == "Yes"))
dim(c1)
head(c1)

c2 <- as.data.frame(buys_comp%>%
                      filter(Buys_Computer == "No"))
dim(c2)



count_c1 = sum(buys_comp$Buys_Computer == "Yes")
count_c1

count_c2 = sum(buys_comp$Buys_Computer == "No")
count_c2

count_TTL = count_c1 + count_c2
count_TTL

P_c1 = count_c1 / count_TTL
P_c1

P_c2 = count_c2 / count_TTL
P_c2



gini_p_n = 1-((P_c1^2)+(P_c2^2))
gini_p_n

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

age_1 <- as.data.frame(buys_comp%>%
                         filter(ï..Age == "< = 30"))
head(age_1)


count_age_1 = sum(buys_comp$ï..Age == "< = 30")
count_age_1


age_1_y = sum(age_1$Buys_Computer == "Yes")
age_1_y
P_age_1_y = age_1_y/count_age_1
P_age_1_y

age_1_n = sum(age_1$Buys_Computer == "No")
age_1_n
P_age_1_n = age_1_n/count_age_1
P_age_1_n






age_2 <- as.data.frame(buys_comp%>%
                         filter(ï..Age == "31 . . . 40"))
head(age_2)


count_age_2 = sum(buys_comp$ï..Age == "31 . . . 40")
count_age_2




age_2_y = sum(age_2$Buys_Computer == "Yes")
age_2_y


age_2_n = sum(age_2$Buys_Computer == "No")
age_2_n





age_3 <- as.data.frame(buys_comp%>%
                         filter(ï..Age == "> 40"))
head(age_3)


count_age_3 = sum(buys_comp$ï..Age == "> 40")
count_age_3


age_3_y = sum(age_3$Buys_Computer == "Yes")
age_3_y


age_3_n = sum(age_3$Buys_Computer == "No")
age_3_n





gini_age_1 = 1-(((age_1_y/count_age_1)^2)+((age_1_n/count_age_1)^2))
gini_age_1

gini_age_2 = 1-(((age_2_y/count_age_2)^2)+((age_2_n/count_age_2)^2))
gini_age_2

gini_age_3 = 1-(((age_3_y/count_age_3)^2)+((age_3_n/count_age_3)^2))
gini_age_3



gini_age = ((count_age_1/count_TTL)*gini_age_1)+((count_age_2/count_TTL)*gini_age_2)+((count_age_3/count_TTL)*gini_age_3)
gini_age



#--------------------------------------------------------------------------------------------------------------------------------------------------------------------


income_1 <- as.data.frame(buys_comp%>%
                            filter(Income == "High"))
head(income_1)


count_income_1 = sum(buys_comp$Income == "High")
count_income_1


income_1_y = sum(income_1$Buys_Computer == "Yes")
income_1_y
P_income_1_y = income_1_y/count_income_1
P_income_1_y

income_1_n = sum(income_1$Buys_Computer == "No")
income_1_n
P_income_1_n = income_1_n/count_income_1
P_income_1_n






income_2 <- as.data.frame(buys_comp%>%
                            filter(Income == "Medium"))
head(income_2)


count_income_2 = sum(buys_comp$Income == "Medium")
count_income_2




income_2_y = sum(income_2$Buys_Computer == "Yes")
income_2_y


income_2_n = sum(income_2$Buys_Computer == "No")
income_2_n





income_3 <- as.data.frame(buys_comp%>%
                            filter(Income == "Low"))
head(income_3)


count_income_3 = sum(buys_comp$Income == "Low")
count_income_3


income_3_y = sum(income_3$Buys_Computer == "Yes")
income_3_y


income_3_n = sum(income_3$Buys_Computer == "No")
income_3_n





gini_income_1 = 1-(((income_1_y/count_income_1)^2)+((income_1_n/count_income_1)^2))
gini_income_1

gini_income_2 = 1-(((income_2_y/count_income_2)^2)+((income_2_n/count_income_2)^2))
gini_income_2

gini_income_3 = 1-(((income_3_y/count_income_3)^2)+((income_3_n/count_income_3)^2))
gini_income_3



gini_income = ((count_income_1/count_TTL)*gini_income_1)+((count_income_2/count_TTL)*gini_income_2)+((count_income_3/count_TTL)*gini_income_3)
gini_income


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



student_1 <- as.data.frame(buys_comp%>%
                             filter(Student == "No"))
head(student_1)


count_student_1 = sum(buys_comp$Student == "No")
count_student_1


student_1_y = sum(student_1$Buys_Computer == "Yes")
student_1_y
P_student_1_y = student_1_y/count_student_1
P_student_1_y

student_1_n = sum(student_1$Buys_Computer == "No")
student_1_n
P_student_1_n = student_1_n/count_student_1
P_student_1_n






student_2 <- as.data.frame(buys_comp%>%
                             filter(Student == "Yes"))
head(student_2)


count_student_2 = sum(buys_comp$Student == "Yes")
count_student_2




student_2_y = sum(student_2$Buys_Computer == "Yes")
student_2_y


student_2_n = sum(student_2$Buys_Computer == "No")
student_2_n






gini_student_1 = 1-(((student_1_y/count_student_1)^2)+((student_1_n/count_student_1)^2))
gini_student_1

gini_student_2 = 1-(((student_2_y/count_student_2)^2)+((student_2_n/count_student_2)^2))
gini_student_2



gini_student = ((count_student_1/count_TTL)*gini_student_1)+((count_student_2/count_TTL)*gini_student_2)
gini_student



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


credit_1 <- as.data.frame(buys_comp%>%
                            filter(Credit_Rating == "Fair"))
head(credit_1)


count_credit_1 = sum(buys_comp$Credit_Rating == "Fair")
count_credit_1


credit_1_y = sum(credit_1$Buys_Computer == "Yes")
credit_1_y
P_credit_1_y = credit_1_y/count_credit_1
P_credit_1_y

credit_1_n = sum(credit_1$Buys_Computer == "No")
credit_1_n
P_credit_1_n = credit_1_n/count_credit_1
P_credit_1_n






credit_2 <- as.data.frame(buys_comp%>%
                            filter(Credit_Rating== "Excellent"))
head(credit_2)


count_credit_2 = sum(buys_comp$Credit_Rating == "Excellent")
count_credit_2




credit_2_y = sum(credit_2$Buys_Computer == "Yes")
credit_2_y


credit_2_n = sum(credit_2$Buys_Computer == "No")
credit_2_n




gini_credit_1 = 1-(((credit_1_y/count_credit_1)^2)+((credit_1_n/count_credit_1)^2))
gini_credit_1

gini_credit_2 = 1-(((credit_2_y/count_credit_2)^2)+((credit_2_n/count_credit_2)^2))
gini_credit_2


gini_credit = ((count_credit_1/count_TTL)*gini_credit_1)+((count_credit_2/count_TTL)*gini_credit_2)
gini_credit



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


gini_age

gini_income

gini_student

gini_credit

minm = min(c(gini_age, gini_income, gini_student, gini_credit))
minm

maxm = max(c(gini_age, gini_income, gini_student, gini_credit))
maxm

print(paste(minm, " is the purest value"))

print(paste(maxm, " is the most impure value"))

