sslc$L1_MARKS<-((100/125)*sslc$L1_MARKS)
sslc$L1_MARKS<-round(sslc$L1_MARKS)
sslc$TOTAL_MARKS<-(sslc$L1_MARKS + sslc$L2_MARKS + sslc$L3_MARKS + sslc$S1_MARKS + sslc$S2_MARKS +sslc$S3_MARKS)

######changing NRC_Class

sslc$NRC_CLASS <- as.character(sslc$NRC_CLASS)

sslc$NRC_CLASS[(sslc$TOTAL_MARKS/600)*100 >= 80 & sslc$NRC_RESULT == "P"] <- "D"

sslc$NRC_CLASS[(sslc$TOTAL_MARKS/600)*100 < 80 & (sslc$TOTAL_MARKS/600)*100 >=60 & sslc$NRC_RESULT == "P"] <- "1"

sslc$NRC_CLASS[(sslc$TOTAL_MARKS/600)*100 < 60 & (sslc$TOTAL_MARKS/600)*100 >=50 & sslc$NRC_RESULT == "P"] <- "2"

sslc$NRC_CLASS[(sslc$TOTAL_MARKS/600)*100 < 50 & (sslc$TOTAL_MARKS/600)*100 >=35 & sslc$NRC_RESULT == "P"] <- "PASS"
