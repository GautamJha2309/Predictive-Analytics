library(dplyr)

golf = read.csv("D:/College/SEN-6/PA_LAB/Golf.csv", stringsAsFactors = TRUE) #convert char to factor
View(golf)
str(golf)
dim(golf)
glimpse(golf)

#CLASS:
c1 <- as.data.frame(golf%>%
  filter(Play_Golf == "YES"))
dim(c1)

c2 <- as.data.frame(golf%>%
                     filter(Play_Golf == "NO"))
dim(c2)


outlook_overcast_c1 = sum(c1$ï..Outlook == "OverCast")
outlook_overcast_c1


#PRIOR PROBABILITY:
count_c1 = sum(golf$Play_Golf == "YES")
count_c1

count_c2 = sum(golf$Play_Golf == "NO")
count_c2

count_TTL = count_c1 + count_c2
count_TTL

P_c1 = count_c1 / count_TTL
P_c1

P_c2 = count_c2 / count_TTL
P_c2

#LIKELYHOOD PROBRBLITY:
#FOR C1:
outlook_overcast_c1 = sum(c1$ï..Outlook == "OverCast")
outlook_overcast_c1
Temp_Mild_c1 = sum(c1$Temperature == "Mild")
Temp_Mild_c1
Humidity_Normal_c1 = sum(c1$Humidity == "Normal")
Humidity_Normal_c1
Wind_FALSE_c1 = sum(c1$Wind == 'FALSE')
Wind_FALSE_c1


P_outlook_overcast_c1 = outlook_overcast_c1 / count_c1
P_outlook_overcast_c1
P_Temp_Mild_c1 = Temp_Mild_c1 / count_c1
P_Temp_Mild_c1
P_Humidity_Normal_c1 = Humidity_Normal_c1 / count_c1
P_Humidity_Normal_c1
P_Wind_FALSE_c1 = Wind_FALSE_c1 / count_c1
P_Wind_FALSE_c1

P_X_c1 = P_outlook_overcast_c1 * P_Temp_Mild_c1 * P_Humidity_Normal_c1 * P_Wind_FALSE_c1
P_X_c1



#FOR C2:
outlook_overcast_c2 = sum(c2$ï..Outlook == "OverCast")
outlook_overcast_c2
Temp_Mild_c2 = sum(c2$Temperature == "Mild")
Temp_Mild_c2
Humidity_Normal_c2 = sum(c2$Humidity == "Normal")
Humidity_Normal_c2
Wind_FALSE_c2 = sum(c2$Wind == 'FALSE')
Wind_FALSE_c2


P_outlook_overcast_c2 = outlook_overcast_c2 / count_c2
P_outlook_overcast_c2
P_Temp_Mild_c2 = Temp_Mild_c2 / count_c2
P_Temp_Mild_c2
P_Humidity_Normal_c2 = Humidity_Normal_c2 / count_c2
P_Humidity_Normal_c2
P_Wind_FALSE_c2 = Wind_FALSE_c2 / count_c2
P_Wind_FALSE_c2

P_X_c2 = P_outlook_overcast_c2 * P_Temp_Mild_c2 * P_Humidity_Normal_c2 * P_Wind_FALSE_c2
P_X_c2


#P(H|X) = P(X|H) * P(H)

P_C1_X = P_X_c1 * P_c1
P_C1_X

P_c2_X = P_X_c2 * P_c2
P_c2_X


if(P_C1_X > P_c2_X){
  print("X belongs to class c1 i.e. Play_Golf = YES")
} else{
  print("X belongs to class c2 i.e. Play_Golf = NO")
}
