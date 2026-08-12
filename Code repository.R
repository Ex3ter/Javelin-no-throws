install_github("RobinHankin/hyper2")

library(hyper2)
library(dplyr)
library(ggplot2)
library(car)
library(tidyr)

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
abline(glm(no_throws~placements))
summary(glm(no_throws~placements))

#Get max value of each athlete
final_scores=apply(javelin_table,1,max,na.rm=TRUE)

#Final score against number of no-throws
plot(no_throws,final_scores)
abline(mod<-glm(final_scores~no_throws))
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
plot(no_throws,final_scores)
abline(mod<-glm(final_scores~no_throws))
summary(mod)

#Plot number of athletes in each no throw bracket
data_temp<-data.frame(Number_of_no_throws=c("0","1","2","3","4","5","6"),Number_of_athletes=c(13,24,27,17,3,4,0))
ggplot(data_temp,aes(x=Number_of_no_throws,y=Number_of_athletes))+geom_bar(stat = "identity")

#Check distribution of best throws (earlier or later?)
for (i in 1:88){
  temp<-match(final_scores[i],javelinmaster_numbers[i,])
  max_throw_number[i]<-temp
}
maxes<-data.frame(final_scores,max_throw_number,no_throws)

hist(max_throw_number)
hist(table(max_throw_number))
#Alright, fine, I'll find another way
data_temp<-data.frame(Max_throw_number=c("1","2","3","4","5","6"),Frequency=c(18,26,19,5,11,9))
ggplot(data_temp,aes(x=Max_throw_number,y=Frequency))+geom_bar(stat = "identity")
#Better

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
abline(mod<-glm(no_throws~final_scores))
summary(mod)
#A value of -0.036 with a p-value of 0.497.  Nothing of note.

shot_put

no_throws=rowSums(is.na(shot_put))
plot(no_throws, xlab="Placement", ylab="No-Throws")
shot_put_numbers<-shot_put[,-1]
shot_put_numbers<-shot_put_numbers[,-7]
final_scores=apply(shot_put_numbers,1,max,na.rm=TRUE)
plot(final_scores,no_throws)
abline(mod<-glm(final_scores~no_throws))
summary(mod)
#Interesting.  A value of -0.48 with a p-value of 0.016.  Very interesting.

boxplot(final_scores~no_throws)


discus

no_throws=rowSums(is.na(discus))
plot(no_throws, xlab="Placement", ylab="No-Throws")
discus_numbers<-discus[,-1]
discus_numbers<-discus_numbers[,-7]
final_scores=apply(discus_numbers,1,max,na.rm=TRUE)
plot(no_throws,final_scores)
abline(mod<-glm(final_scores~no_throws))
summary(mod)
#A value of -0.074 at a p-value of 0.227.  Nothing of note.

long.jump

no_throws=rowSums(is.na(long.jump))
plot(no_throws, xlab="Placement", ylab="No-Throws")
long.jump_numbers<-long.jump[,-1]
long.jump_numbers<-long.jump_numbers[,-7]
final_scores=apply(long.jump_numbers,1,max,na.rm=TRUE)
plot(no_throws,final_scores)
abline(mod<-glm(final_scores~no_throws))
summary(mod)
#A value of -0.0017 at a p-value of 0.905.  Nothing of note.



#Continue looking into shot-put

#Sums of totals of number of no-throws
plot(table(rowSums(is.na(shot_put_numbers))),xlab = "Number of no throws",ylab = "Number of athletes")

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
abline(mod<-glm(o~e))
summary(mod)

#Does this hold for women?
no_throws=rowSums(is.na(shot_put.women))
plot(no_throws, xlab="Placement", ylab="No-Throws")
shot_put.women_numbers<-shot_put.women[,-1]
shot_put.women_numbers<-shot_put.women_numbers[,-7]
final_scores=apply(shot_put.women_numbers,1,max,na.rm=TRUE)
plot(no_throws,final_scores)
abline(mod<-glm(final_scores~no_throws))
summary(mod)

plot(no_throws,final_scores)
abline(mod<-glm(final_scores~no_throws))
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


#Get max value of each athlete
final_scores_shot=apply(shot_put_numbers,1,max,na.rm=TRUE)
max_throw_number_shot<-c(1:85)

#Check distribution of best throws (earlier or later?)
for (i in 1:85){
  temp<-match(final_scores_shot[i],shot_put_numbers[i,])
  max_throw_number_shot[i]<-temp
}
maxes_shot<-data.frame(final_scores_shot,max_throw_number_shot,no_throws)

hist(max_throw_number_shot)

#Are max throws uniformly distributed?
o<-c(13,23,24,13,9,3)
chisq.test(o)


#Order by number of appearances and alphabetically for ease of use
shot_put_ordered
#Use Fisher test to check if certainly players have a higher foul rate
#Fisher tests number of no throws.  Rewrite data.
ordered_throws<-data.frame(shot_put_ordered$name,shot_put_ordered$no_throws)
mean(ordered_throws$shot_put_ordered.no_throws)
#1.894118
#Start with Crouser
fisher.test(matrix(c(3,0,0,1.894118,1.894118,1.894118),nrow=2),alternative = "lower")
#Nothing
#Gunthor?
fisher.test(matrix(c(1,0,3,1.894118,1.894118,1.894118),nrow=2),alternative = "lower")
#Kovacs
fisher.test(matrix(c(3,0,3,1.894118,1.894118,1.894118),nrow=2),alternative = "higher")
#Majewski
fisher.test(matrix(c(4,1,1,1.894118,1.894118,1.894118),nrow=2),alternative = "higher")
#Andrei
fisher.test(matrix(c(0,0,1.894118,1.894118),nrow=2))
#Armstrong
fisher.test(matrix(c(2,3,1.894118,1.894118),nrow=2))
#Barnes
fisher.test(matrix(c(1,1,1.894118,1.894118),nrow=2))
#Buder
fisher.test(matrix(c(3,0,1.894118,1.894118),nrow=2))
#Cantwell
fisher.test(matrix(c(2,0,1.894118,1.894118),nrow=2))
#Godina
fisher.test(matrix(c(2,2,1.894118,1.894118),nrow=2))
#Hoffa
fisher.test(matrix(c(1,3,1.894118,1.894118),nrow=2))
#Nelson
fisher.test(matrix(c(5,2,1.894118,1.894118),nrow=2))
#Peric
fisher.test(matrix(c(2,2,1.894118,1.894118),nrow=2))
#Romani
fisher.test(matrix(c(2,2,1.894118,1.894118),nrow=2))
#Storl
fisher.test(matrix(c(2,3,1.894118,1.894118),nrow=2))
#Timmerman
fisher.test(matrix(c(0,0,1.894118,1.894118),nrow=2))
#Walsh
fisher.test(matrix(c(1,1,1.894118,1.894118),nrow=2))

#Get mean and sd of each sport
jmax=apply(javelinmaster_numbers,1,max,na.rm=TRUE)
mean(jmax)
#84.59386
sd(jmax)
#2.985811
dmax=apply(discus_numbers,1,max,na.rm=TRUE)
mean(dmax)
#66.01644
sd(dmax)
#2.019403
lmax=apply(long.jump_numbers,1,max,na.rm=TRUE)
mean(lmax)
#8.198161
sd(lmax)
#0.1895069
smax=apply(shot_put_numbers,1,max,na.rm=TRUE)
mean(smax)
#21.05212
sd(smax)
#0.7267198

#Javelin
A<-replicate(1000000,max(rnorm(6,mean=84.59386,sd=2.985811)))
B<-replicate(1000000,max(rnorm(5,mean=84.59386,sd=3.27)))
table(A>B)
#Increase by 0.284
#Discus
A<-replicate(1000000,max(rnorm(6,mean=66.01644,sd=2.019403)))
B<-replicate(1000000,max(rnorm(5,mean=66.01644,sd=2.21)))
table(A>B)
#Increase by 0.191
#Long jump
A<-replicate(1000000,max(rnorm(6,mean=8.198161,sd=0.1895069)))
B<-replicate(1000000,max(rnorm(5,mean=8.198161,sd=0.2075)))
table(A>B)
#Increase by 0.018
#Shot put
A<-replicate(1000000,max(rnorm(6,mean=21.05212,sd=0.7267198)))
B<-replicate(1000000,max(rnorm(5,mean=21.05212,sd=0.795)))
table(A>B)
#Increase by 0.0683


#Shot-put variance redo
#apply(na.omit(shot_put_numbers),1,var)
#apply(shot_put.women_numbers,1,var)
#Didn't work.  Do it manually.
men_sd_0<-c(0.22946798, 0.56885118, 0.44271887, 0.22559181, 0.26562296,0.21919677,0.23697163,0.36818247,0.3044485,0.17764665,0.21551618,
            0.28371053,0.46452186)
mean(men_sd_0)
men_sd_1<-c(0.52981506,0.44314332,0.31755315, 0.36831508, 0.25435408, 0.62302488, 0.5358358,0.41802392,0.23085926, 0.3978643,0.58029648,
            0.69247383,0.3245674,0.35838527,0.15252541,0.37288068,0.45989564,0.73805149,0.25072694,0.092217135,0.25340876,0.94669108,
            0.29763064)
mean(men_sd_1)
men_sd_2<-c(1.075,0.2676168, 0.72517239,0.22653642,0.43874822, 0.30873735,0.090553851, 0.26929306, 0.076648549, 0.36578682,0.30678779,
            0.19149086,0.11648498,0.24728526,0.36731288,0.27562429,0.36338685,0.36058979,0.22522211,0.38942265,0.16887495,0.1662077,
            0.22079402,0.41469115,0.26542419)
mean(men_sd_2)
men_sd_3<-c(0.11264497, 0.21228911, 0.34296096, 0.17682383, 0.20757863, 0.3353605, 0.24124676,0.31016125,0.54100729,0.21452791,
            0.10984838,0.22065559,0.39041289)
mean(men_sd_3)
men_sd_4<-c(0.37, 0.225, 0.1, 0.15, 0.12,0.045,0.125,0.135)
mean(men_sd_4)

women_sd_0<-c(0.20669355,0.46114592,0.46309346,0.7189344,0.19316803,0.19787763,0.23513589,0.43094534,0.33289972,0.27384606,
              0.30137186,0.31867782,0.36917701,0.49851167,0.39914144, 0.55132416,0.36043569,0.2803371,0.32575127)
mean(women_sd_0)
women_sd_1<-c(0.47759397,1.0659568,0.3222049,0.39309032,0.26951809,0.35387003,0.22973028,0.1995996,0.25918333,0.12890306,0.67889911,
              0.74477916,0.35210226,0.092822411,0.42804673,0.14062717,0.43301732,0.57875729,0.20212867,0.33878607,0.3718817,0.14953261,
              0.2605072,0.3005994)
mean(women_sd_1)
women_sd_2<-c(0.46574537,0.10062306,0.23005434,0.10662434,0.43774279,0.40659409,0.30520485,0.16628289,0.17507141,0.38288379,0.5027114,
              0.67514813,0.28760868,0.60788054,0.42373341,0.41695323,0.053851648,0.46113989,0.88855219)
mean(women_sd_2)
women_sd_3<-c(0.24097026,0.10338708,0.16739839,0.19905332,0.75113248,0.35928942,0.50387388,0.30684777,0.18190352,0.40002778,
              0.33489634,0.37348211)
mean(women_sd_3)
women_sd_4<-c(0.235,0.055,0.115,0.38,0.08,0.04)
mean(women_sd_4)