sslc_Sorted_total_marks<-sslc[order(-sslc$TOTAL_MARKS)]
sslc_Sorted_total_marks<-sslc[order(-sslc$TOTAL_MARKS),]
blind_school<-sslc_Sorted_total_marks$SCHOOL_NAME[sslc_Sorted_total_marks$NRC_RESULT == 'P' & sslc_Sorted_total_marks$NRC_PHYSICAL_CONDITION == 'B']
##########P School name###
P_school_name<-sslc_Sorted_total_marks$SCHOOL_NAME[sslc_Sorted_total_marks$NRC_RESULT == 'P' & sslc_Sorted_total_marks$NRC_PHYSICAL_CONDITION == 'P']
##########
head(blind_school)
blind_school[1:5]
unique_blind_school_code<-unique(blind_school)
duplicated(blind_school)
###########BLind school code#######
blind_school_code<-sslc_Sorted_total_marks$SCHOOL_CODE[sslc_Sorted_total_marks$NRC_RESULT == 'P' & sslc_Sorted_total_marks$NRC_PHYSICAL_CONDITION == 'B']


#########For deaf school code
Deaf_school_code<-sslc_Sorted_total_marks$SCHOOL_CODE[sslc_Sorted_total_marks$NRC_RESULT == 'P' & sslc_Sorted_total_marks$NRC_PHYSICAL_CONDITION == 'D']
head(Deaf_school)
#########For H school code
H_school_code<-sslc_Sorted_total_marks$SCHOOL_CODE[sslc_Sorted_total_marks$NRC_RESULT == 'P' & sslc_Sorted_total_marks$NRC_PHYSICAL_CONDITION == 'H']
head(Deaf_school)
#########For P school code
P_school_code<-sslc_Sorted_total_marks$SCHOOL_CODE[sslc_Sorted_total_marks$NRC_RESULT == 'P' & sslc_Sorted_total_marks$NRC_PHYSICAL_CONDITION == 'P']
head(Deaf_school)
#########For s school code
s_school_code<-sslc_Sorted_total_marks$SCHOOL_CODE[sslc_Sorted_total_marks$NRC_RESULT == 'P' & sslc_Sorted_total_marks$NRC_PHYSICAL_CONDITION == 'S']
head(Deaf_school)
 ###3School that have deaf and blind both##########33


phy_school<-function(code,result,phy_condition)
{
  type_Of_Phy_problems<-levels(sslc_Sorted_total_marks$NRC_PHYSICAL_CONDITION)
  list_of<-levels(sslc_Sorted_total_marks$NRC_PHYSICAL_CONDITION)
  num_Of_Phy_problems<-length(type_Of_Phy_problems)
  for(i in 1:num_Of_Phy_problems)
  {
    num
  }
}