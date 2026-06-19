install_github("RobinHankin/hyper2")

library(hyper2)
library(dplyr)
library(ggplot2)
library(car)

javelin_table

#Disadvantage received from reduced throws
table(replicate(1e6,max(rnorm(6))>max(rnorm(5))))
table(replicate(1e6,max(rnorm(6))>max(rnorm(4))))
table(replicate(1e6,max(rnorm(6))>max(rnorm(3))))
table(replicate(1e6,max(rnorm(6))>max(rnorm(2))))
table(replicate(1e6,max(rnorm(6))>max(rnorm(1))))

#Number of no-throws per athlete by final placement
no_throws=rowSums(is.na(javelin_table))
plot(no_throws, xlab="Placement", ylab="No-Throws")
placements=1:8
abline(lm(no_throws~placements))
summary(lm(no_throws~placements))

#Get max value of each athlete
final_scores=apply(javelin_table,1,max,na.rm=TRUE)

#Final score against number of no-throws
plot(final_scores,no_throws)
abline(mod<-lm(no_throws~final_scores))
summary(mod)

#Chi-square test for no-throw sequential independence, removing athletes that didn't no-throw
chisq.test(x=c(0,2),p=c(0.2*2,0.8*2),rescale.p=TRUE) #Chopra, insignificant
chisq.test(x=c(2,1),p=c(0.8*3,0.2*3),rescale.p=TRUE) #Vadlejch, insignificant
chisq.test(x=c(0,2),p=c(0.4*2,0.6*2),rescale.p=TRUE) #Vesely, insignificant
chisq.test(x=c(1,1),p=c(0.4*2,0.6*3),rescale.p=TRUE) #Nadeem, insignificant
chisq.test(x=c(0,2),p=c(0.4*2,0.6*3),rescale.p=TRUE) #Katkavets, insignificant

#Sums of totals of number of no-throws
plot(table(rowSums(is.na(javelinmaster_numbers))))
n<-rowSums(is.na(javelinmaster_numbers))

#Overall no-throw rate
o<-c(13,24,27,17,3,4,0)
o*(0:6)
sum(o*(0:6))
11*48
161/528

#Is this statistically significant?
#Null there are 0 intentional no-throws
phat<-161/1056
(161/528)/phat*(1-phat)*(1/264)
#So there are intentional no-throws

#Are no-throws binomially distributed?
e<-dbinom(0:6,6,161/528)*sum(o)
sum((o-e)^2/e)
pchisq(11.94635,df=5,lower.tail=F)

#Something happening at 5 no-throws
o
e

#Plot observed no-throws against expected
plot(o,e,asp=1)

#Number of no-throws per athlete by final placement, expanded data
no_throws=rowSums(is.na(javelinmaster_numbers))
plot(no_throws, xlab="Placement", ylab="No-Throws")

#Get max value of each athlete
final_scores=apply(javelinmaster_numbers,1,max,na.rm=TRUE)

#Final score against number of no-throws
plot(final_scores,no_throws)
abline(mod<-lm(no_throws~final_scores))
summary(mod)

#Check distribution of best throws (earlier or later?)
for (i in 1:88){
  temp<-match(final_scores[i],javelinmaster_numbers[i,])
  max_throw_number[i]<-temp
}
maxes<-data.frame(final_scores,max_throw_number,no_throws)

hist(max_throw_number)

#Are max throws uniformly distributed?
o<-c(18,26,19,5,11,9)
chisq.test(o)

#Check if people who threw a strong first attempt have higher no-throw rates than people who didn't
plot(maxes)
#Nonsense question unfortunately


#Interrogate athletes who show up multiple times
#2: Chopra, Etelatalo, Hecht, Henry, Kinnunen, Nadeem, Ruuskanen, Vadlejch, Weber, Yego
#3: Makarov, Pitkamaki, Raty, Thorkildsen, Vesely, Walcott
#4: Backley, Zelezny

#Interrogate people who had 0 no-throws
javelinmaster[n==0,]
#Backley: 4, Raty: 3 (twice), Ruuskanen: 2, Pitkamaki: 3, Weber: 2, Etelatalo: 2
#Raty came 3rd in 1988 and 1996, Backley came 3rd in 1992, Kovals came 2nd in 2008 (last throw mattered), Ruuskanen came 2nd in 2012


#Check performance of five no-throw games
javelinmaster[n==5,]
#Yego came 2nd in 2016, Copra came 2nd in 2024.  Both have shown up twice.


#Order by number of appearances and alphabetically for ease of use
ordered
#Use Fisher test to check if certainly players have a higher foul rate
#Fisher tests number of no throws.  Rewrite data.
ordered_throws<-data.frame(ordered$names,ordered$no_throws)
mean(ordered_throws$ordered.no_throws)
#1.806818
#Start with Backley
fisher.test(matrix(c(0,1,3,1,1.806818,1.806818,1.806818,1.806818),nrow=2),alternative = "lower")
#Nothing
#Zelezny?
fisher.test(matrix(c(1,2,1,3,1.806818,1.806818,1.806818,1.806818),nrow=2),alternative = "lower")
#Makarov
fisher.test(matrix(c(1,2,1,1.806818,1.806818,1.806818),nrow=2),alternative = "lower")
#Pitkamaki
fisher.test(matrix(c(3,2,0,1.806818,1.806818,1.806818),nrow=2),alternative = "lower")
#Raty
fisher.test(matrix(c(0,2,0,1.806818,1.806818,1.806818),nrow=2),alternative = "lower")
#Thorkildsen
fisher.test(matrix(c(3,1,4,1.806818,1.806818,1.806818),nrow=2),alternative = "higher")
#Vadlejch
fisher.test(matrix(c(2,3,1,1.806818,1.806818,1.806818),nrow=2),alternative = "higher")
#Vesely
fisher.test(matrix(c(2,3,2,1.806818,1.806818,1.806818),nrow=2),alternative = "higher")
#Walcott
fisher.test(matrix(c(3,2,2,1.806818,1.806818,1.806818),nrow=2),alternative = "higher")
#Chopra
fisher.test(matrix(c(2,4,1.806818,1.806818),nrow=2))
#Etelatalo
fisher.test(matrix(c(0,1,1.806818,1.806818),nrow=2))
#Hecht
fisher.test(matrix(c(2,4,1.806818,1.806818),nrow=2))
#Henry
fisher.test(matrix(c(1,1,1.806818,1.806818),nrow=2))
#Kinnunen
fisher.test(matrix(c(5,2,1.806818,1.806818),nrow=2))
#Ruuskanen
fisher.test(matrix(c(0,3,1.806818,1.806818),nrow=2))
#Weber
fisher.test(matrix(c(0,2,1.806818,1.806818),nrow=2))
#Yego
fisher.test(matrix(c(5,1,1.806818,1.806818),nrow=2))


#Analyze standard deviation with the null that variance is not affected by the number of no throws.
#Calculate the standard deviation of each run
throw_sd<-c(1:88)
for (i in 1:88){
  throw_sd[i]<-sd(javelinmaster_numbers[i,],na.rm=TRUE)
}
levene_setup<-data.frame(throw_sd,max_no_throws)

levene_setup

zeroes<-c(2.3136004,1.6528117,1.9792389,3.8695822,1.6861198,1.9327640,2.7977664,1.6374696,2.1088330,2.6108115,4.1592327,0.8957827,2.6361108)
ones<-c(1.4700748,4.1517370,2.4385405,1.9937101,1.0804258,1.6175043,1.7766485,2.1380645,3.0665485,1.6760310,1.5828455,1.3545221,
        1.7777430,3.4516185,1.7068333,2.4178337,0.952530,2.1001738,1.2065571,5.5300949,2.7798975,3.0179844,2.9086801,2.7690702)
twos<-c(1.7666918,4.7884688,1.9824144,1.1073693,3.3880968,2.3446108,2.1369761,1.6470580,1.5691505,1.4679805,3.4306511,3.1782805,
        2.7132023,2.7533298,2.5157421,2.9158532,3.5021232,1.2347031,2.0870474,1.5478668,1.1190286,4.9667763,3.0142039,1.1591484,1.9267827,1.5708039,4.1356046)
threes<-c(4.1137331,1.8766282,4.2560232,1.6943435,2.3103535,0.6070695,4.4558426,2.8405868,3.8053165,
          1.3469348,4.7870555,1.4337713,2.0373758,2.0223089,2.6317358,2.3740051,1.9581709)
fours<-c(8.2377940,0.2262742,0.6576093)

leveneTest(throw_sd~max_no_throws)
#Doesn't work because of different object lengths and "quantitative explanatory variables"



#Reanalyse for new data
javelinmaster_women

no_throws=rowSums(is.na(javelinmaster_women))
plot(no_throws, xlab="Placement", ylab="No-Throws")
javelinmaster_women_numbers<-javelinmaster_women[,-1]
javelinmaster_women_numbers<-javelinmaster_women_numbers[,-7]
final_scores=apply(javelinmaster_women_numbers,1,max,na.rm=TRUE)
plot(final_scores,no_throws)
abline(mod<-lm(no_throws~final_scores))
summary(mod)
#A value of -0.036 with a p-value of 0.497.  Nothing of note.

shot_put

no_throws=rowSums(is.na(shot_put))
plot(no_throws, xlab="Placement", ylab="No-Throws")
shot_put_numbers<-shot_put[,-1]
shot_put_numbers<-shot_put_numbers[,-7]
final_scores=apply(shot_put_numbers,1,max,na.rm=TRUE)
plot(final_scores,no_throws)
abline(mod<-lm(final_scores~no_throws))
summary(mod)
#Interesting.  A value of -0.48 with a p-value of 0.016.  Very interesting.

discus

no_throws=rowSums(is.na(discus))
plot(no_throws, xlab="Placement", ylab="No-Throws")
discus_numbers<-discus[,-1]
discus_numbers<-discus_numbers[,-7]
final_scores=apply(discus_numbers,1,max,na.rm=TRUE)
plot(no_throws,final_scores)
abline(mod<-lm(final_scores~no_throws))
summary(mod)
#A value of -0.074 at a p-value of 0.227.  Nothing of note.

boxplot(final_scores~no_throws)

#Continue looking into shot-put

#Sums of totals of number of no-throws
plot(table(rowSums(is.na(shot_put_numbers))))

#Overall no-throw rate
o<-c(13,23,24,13,9,3,0)
o*(0:6)
sum(o*(0:6))
11*48
161/528
#Interesting.  Identical no-throw rate.

#Is this statistically significant?
#Null there are 0 intentional no-throws
phat<-161/1056
(161/528)/phat*(1-phat)*(1/264)


#Are no-throws binomially distributed?
e<-dbinom(0:6,6,161/528)*sum(o)
sum((o-e)^2/e)
pchisq(9.69711,df=5,lower.tail=F)

#Plot observed no-throws against expected
plot(o,e,xlab="Observed",ylab="Expected",asp=1)
abline(mod<-lm(o~e))
summary(mod)

#Does this hold for women?
no_throws=rowSums(is.na(shot_put.women))
plot(no_throws, xlab="Placement", ylab="No-Throws")
shot_put.women_numbers<-shot_put.women[,-1]
shot_put.women_numbers<-shot_put.women_numbers[,-7]
final_scores=apply(shot_put.women_numbers,1,max,na.rm=TRUE)
plot(no_throws,final_scores)
abline(mod<-lm(final_scores~no_throws))
summary(mod)

plot(no_throws,final_scores)
abline(mod<-lm(final_scores~no_throws))
summary(mod)

#Compare the variances of each no throw category
zeroes_shot=c(22.93,22.65,22.52,21.09,20.51,20.49,20.48,22.49,21.99,20.57,20.36,21.26,20.93)
#var=0.9389436
ones_shot=c(22.18,21.41,21.36,21.2,20.64,21.89,21.23,20.84,21.51,20.84,20.55,20.32,21.62,20.74,20.45,20.39,20.94,20.91,22.39,21.4,20.38,21.09,20.97,19.98)
#var=0.3600897
twos_shot=c(22.03,21.15,20.88,21.88,21,20.73,21.02,21.19,20.93,20.71,20.63,21.07,21.29,21.21,21.2,20.87,20.84,20.79,20.75,20.07,21.7,20.96,20.32)
#var=0.197568
threes_shot=c(22.9,22.15,21.42,21.78,21.86,20.69,21.04,20.53,20.6,20.34,20.26,20.18,20.28)
#var=0.7523244
fours_shot=c(22.15,21.7,20.89,20.72,20.64,20.31,20.23,20.36,19.81)
#var=0.5493
fives_shot=c(20.42,21.16,19.65)
#var=0.5701
var.test(zeroes_shot,twos_shot,alternative="two.sided")