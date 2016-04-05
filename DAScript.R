

###Total_marks across gender and U/R#################

Legend<-paste(NRC_GENDER_CODE,URBAN_RURAL)
qplot(TOTAL_MARKS, col = Legend,fill=Legend,alpha=I(.5) ,geom = "density")





##3Total marks VS S1 marks##############33

coef(lm(sslc$TOTAL_MARKS~sslc$S1_MARKS),data=sslc)
qplot(S1_MARKS,TOTAL_MARKS, data = sslc)+geom_abline(intercept = 54.51, slope = 5.13,color="BLUE",size=2)


#####S2marks vs. total marks#######

coef(lm(sslc$TOTAL_MARKS~sslc$S2_MARKS),data=sslc)

qplot(S2_MARKS,TOTAL_MARKS, data = sslc)+geom_abline(intercept = 52.99, slope = 5.62,color="BLUE",size=2)+coord_polar()

####no. of school per district##############
numberOfSchoolcode<-unique(SCHOOL_CODE)
numberOfSchoolPerDistrict<-substr(numberOfSchoolcode,1,2)
table(numberOfSchoolPerDistrict)



###########Above graph and SVM###
##############scatter.smooth(S1_MARKS,TOTAL_MARKS)

numberOfStudentPerDistrictCount<-table(DIST_CODE)
NoOfschoolAndStudent<-data.frame[numberOfSchoolPerDistrictCount,numberOfStudentPerDistrictCount]
NoOfschoolAndStudent<-cbind(numberOfSchoolPerDistrictCount,numberOfStudentPerDistrictCount)
cor(NoOfschoolAndStudent)
cor(NoOfschoolAndStudent, method="kendall")
cor(NoOfschoolAndStudent, use="complete.obs",method="kendall")
plot(NoOfschoolAndStudent)

#########Association rules######

library(arules)
b <- sslc[,c(4,6,14)]###NRC_RESULT ,NRC_MEDIUM and 
rules <- apriori(b)
##########
b <- sslc[,c(6,30,15)]###NRC_RESULT ,NRC_MEDIUM and 
rules <- apriori(b)
####3
inspect(rules)
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
##########
b <- sslc[,c(4,5,30)]###NRC_RESULT ,NRC_MEDIUM and 
rules <- apriori(b)
###########
b <- sslc[,c(23,25)]###NRC_RESULT ,NRC_MEDIUM and 
rules <- apriori(b)

inspect(rules)
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)


#####clustering silhouette plot###########
newsslc<-sslc[c(17,19,21,23,25,27)]

cl=kmeans(newsslc[1:500,],4)
cl<-kmeans(newsslc[1:500,],4,iter.max=15)
plot(data[c(23,25)],col=cl$cluster)
clusplot(newsslc[1:500,], cl$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
         main = paste('Major Market Clusters over'), sub='')
library("cluster", lib.loc="~/R/win-library/3.2")
diss=daisy(newsslc[1:100,])
sk <- silhouette(cl$cl,diss)
plot(sk)
###########clustering ###########3
marks <- sslc[c("L1_MARKS","L2_MARKS","L3_MARKS","S1_MARKS","S2_MARKS","S3_MARKS")]
cl<-kmeans(newsslc,4,iter.max=30)
plot(sslc[c(23,25)],col=cl$cluster)
cor(23,25)

 

####################hierarchical clustering#####

d <- dist(newsslc[1:100,])
fit <- hclust(d,method="complete")
plot(fit)


###########BOX PLOT OF TOTAL MARKS AND CAST AND GENDER CODE###########
boxplot(TOTAL_MARKS ~ NRC_GENDER_CODE+NRC_CASTE_CODE,main="TOTAL MARKS VS. CASTE VS GENDER CODE ",col=c("GREEN","PINK"))

###################################
