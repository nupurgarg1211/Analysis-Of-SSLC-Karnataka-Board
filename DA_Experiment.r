#Prediction

# 1 - Predict Medium based on L1-L3

model_marks=rpart(NRC_MEDIUM ~ L1_CODE+L2_CODE+L3_CODE ,data=data3[1:23707,],method="class",cp = 0.0001)
p=predict(model_marks,data3[23707:29417,],type="class")
table(data3[23707:29417,13],predicted=p)
#predicted
#      E    H    K    L    M    T    U
# E  501    0  174    0    0    0    9
# H    5    0    0    0    0    0    3
# K   12    0 4501    0    0    0    0
# L    2    0    0    0    0    0    6
# M    0    0    0    0  136    0    0
# T    1    0    0    0    0    0    0
# U   27    0    0    0    0    0  333

m=rpart(NRC_RESULT ~ S2_MARKS+S3_MARKS,data=data[1:23707,],method="class",cp = 0.0001)
rpart.plot(m,type=3,extra=101,fallen.leaves = T)
p=predict(m,data[20708:29634,],type="class")
table(data[20708:29634,29],predicted=p)

m=rpart(NRC_RESULT ~ S1_MARKS+S2_MARKS,data=data[1:23707,],method="class",cp = 0.0001)
rpart.plot(m,type=3,extra=101,fallen.leaves = T)
p=predict(m,data[23708:29416,],type="class")
table(data[23708:29416,29],predicted=p)

sorted_total_marks<-data[order(-TOTAL_MARKS),]
nupur<-head(sorted_total_marks)
sorted_total_marks$SCHOOL_NAME
head(sorted_total_marks$SCHOOL_NAME)
sorted_total_marks$SCHOOL_NAME[1:5]

sample_last<-subset(data, TOTAL_MARKS>600)


count<-table(NRC_GENDER_CODE,NRC_CASTE_CODE)
barplot(count,beside=TRUE)

#############################################################
###   Month wise pass Percentage


per_month_pass <- function(x,y,z){
  per_mon_pass<-numeric(12)
  for(i in 1:12){
    per_mon_pass[i]<-length(z[x==i & y=="P"])
  }
  per_mon_pass
}

per_month_pass(BIRTH_MONTH,NRC_RESULT,REG_NO)

num_student_per_month <- function(x,y){
  per_mon_student<-numeric(12)
  for(i in 1:12){
    per_mon_student[i]<-length(y[x==i])
  }
  per_mon_student
}

num_student_per_month(BIRTH_MONTH,REG_NO)


per_month_perentage_pass <- function(x,y){
  per_mon_percentage_pass<-numeric(12)
  for(i in 1:12){
    per_mon_percentage_pass[i]<-((y[i]*100)/x[i])
  }
  per_mon_percentage_pass
}

per_month_perentage_pass(per_mon_student,per_mon_pass)

plot(per_month_perentage_pass(per_mon_student,per_mon_pass),xlab="Month",ylab="Pass Percentage")
lines(per_month_perentage_pass(per_mon_student,per_mon_pass), y = NULL, type = "l",col="GREEN")
title("Month Wise Pass Percentage")
###################################################################
####### Month wise Fail Percentage

per_month_fail <- function(x,y,z){
  per_mon_fail<-numeric(12)
  for(i in 1:12){
    per_mon_fail[i]<-length(z[x==i & y=="F"])
  }
  per_mon_fail
}

per_mon_fail<-per_month_fail(BIRTH_MONTH,NRC_RESULT,REG_NO)

num_student_per_month <- function(x,y){
  per_mon_student<-numeric(12)
  for(i in 1:12){
    per_mon_student[i]<-length(y[x==i])
  }
  per_mon_student
}

num_student_per_month(BIRTH_MONTH,REG_NO)


per_month_perentage_fail <- function(x,y){
  per_mon_percentage_fail<-numeric(12)
  for(i in 1:12){
    per_mon_percentage_fail[i]<-((y[i]*100)/x[i])
  }
  per_mon_percentage_fail
}

per_month_perentage_fail(per_mon_student,per_mon_fail)

plot(per_month_perentage_fail(per_mon_student,per_mon_fail),xlab="Month",ylab="Fail Percentage")
lines(per_month_perentage_fail(per_mon_student,per_mon_fail), y = NULL, type = "l",col="RED")
title("Month Wise Fail Percentage")



#############No Of Peope who don't write mother's father's name########

NoMotherName<-length(REG_NO[NRC_MOTHER_NAME == 'NA'])
 NoFatherName<-length(REG_NO[NRC_FATHER_NAME == 'NA'])

 num<-c(NoFatherName,NoMotherName)
 barplot(num,col = c("RED","BLUE"))
 
 ##########################################
 
 
 #########  Most Popular name############
 
 