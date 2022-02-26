library(intsvy)

###### Data process ######
# AUS--Australia, GBR--the U.K., QCH-- China, KOR--South Korea
pisa <- pisa.select.merge(folder="2015PISA",
                          student.file="student_data.sav",
                          school.file = "school_data.sav",
                          student= c("ST001D01T","ST003D02T","ST003D03T"
                                     ,"ESCS", "EC015Q02NA","EC015Q03NA"
                                     ,"EC015Q01NA","EC015Q04NA","EC015Q05NA"
                                     ,"EC015Q06NA","EC015Q07NA","EC015Q08NA"),
                          school = c("SC001Q01TA","SC013Q01TA"),
                          countries = c("AUS", "GBR", "QCH", "KOR"))

pisa$ESCS[is.na(pisa$ESCS)]<- 0
school <- aggregate(pisa$ESCS, by =list(pisa$CNTSCHID), mean)
colnames(school) <- c("CNTSCHID", "school_status")
pisa <- merge(x=pisa,y=school,by.x="CNTSCHID",by.y="CNTSCHID",all.x=T)
coun_school <- aggregate(pisa$ESCS, by =list(pisa$CNT,pisa$CNTSCHID), mean)
colnames(coun_school) <- c("CNT","CNTSCHID", "school_status")

# ¡°online_edu_attend¡± means students who have attended online shadow education
# and ¡°offline_edu_attend¡±  means students who have attended offline shadow education
# We define students who only attend online shadow education as online education participants.

pisa$online_edu[pisa$EC015Q02NA == 1] <- "online_edu_attend"
pisa$online_edu[pisa$EC015Q03NA==1] <- "online_edu_attend"
pisa$online_edu[pisa$EC015Q02NA == 0 & pisa$EC015Q03NA==0] <- "online_edu_not attend"
pisa$online_edu_num[pisa$online_edu=="online_edu_not attend"]<- 0
pisa$online_edu_num[pisa$online_edu=="online_edu_attend"]<- 1

pisa$offline_edu[pisa$EC015Q01NA == 1] <- "offline_edu_attend"
pisa$offline_edu[pisa$EC015Q04NA==1] <- "offline_edu_attend"
pisa$offline_edu[pisa$EC015Q05NA==1] <- "offline_edu_attend"
pisa$offline_edu[pisa$EC015Q06NA==1] <- "offline_edu_attend"
pisa$offline_edu[pisa$EC015Q07NA==1] <- "offline_edu_attend"
pisa$offline_edu[pisa$EC015Q08NA==1] <- "offline_edu_attend"
pisa$offline_edu[pisa$EC015Q01NA == 0 & pisa$EC015Q04NA==0
                 & pisa$EC015Q05NA==0 & pisa$EC015Q06NA==0
                 & pisa$EC015Q07NA==0 & pisa$EC015Q08NA==0] <- "offline_edu_not attend"

pisa$shadow_label[pisa$online_edu=="online_edu_not attend" 
                  & pisa$offline_edu== "offline_edu_attend"] <- "only_attend_offline_edu"
pisa$shadow_label[pisa$online_edu=="online_edu_attend" 
                  & pisa$offline_edu== "offline_edu_not attend"] <- "only_attend_online_edu"
pisa$shadow_label[pisa$online_edu=="online_edu_attend" 
                  & pisa$offline_edu== "offline_edu_attend"] <- "attend_online_offline_edu"
pisa$shadow_label[pisa$online_edu=="online_edu_not attend" 
                  & pisa$offline_edu== "offline_edu_not attend"] <- "not_attend"
table(pisa$shadow_label)

pisa$shadow_label_num[pisa$shadow_label=="not_attend"]<- 0
pisa$shadow_label_num[pisa$shadow_label=="only_attend_offline_edu"]<- 1
pisa$shadow_label_num[pisa$shadow_label=="attend_online_offline_edu"]<- 2
pisa$shadow_label_num[pisa$shadow_label=="only_attend_online_edu"]<- 3
table(pisa$shadow_label_num)

# urban/rural
# Code 1 village, 2 small town as rural, the others(3 town,4 city,5 larger city) as urban.
pisa$rural_urban[pisa$SC001Q01TA == 1|pisa$SC001Q01TA == 2] <- 0
pisa$rural_urban[pisa$SC001Q01TA == 3|pisa$SC001Q01TA == 4|pisa$SC001Q01TA == 5] <- 1

# family_ses
# distinguish the last 25% as unprivileged group
aggregate(pisa$ESCS, by =list(pisa$CNT), FUN = quantile, probs = c(0.25))
pisa$family_ses_level_new[pisa$CNT == "AUS" & pisa$ESCS<=-0.34955] <- 0
pisa$family_ses_level_new[pisa$CNT == "AUS" & pisa$ESCS>-0.34955] <- 1
pisa$family_ses_level_new[pisa$CNT == "GBR" & pisa$ESCS<=-0.40190] <- 0
pisa$family_ses_level_new[pisa$CNT == "GBR" & pisa$ESCS>-0.40190] <- 1
pisa$family_ses_level_new[pisa$CNT == "KOR" & pisa$ESCS<=-0.69950] <- 0
pisa$family_ses_level_new[pisa$CNT == "KOR" & pisa$ESCS>-0.69950] <- 1
pisa$family_ses_level_new[pisa$CNT == "QCH" & pisa$ESCS<=-1.70870] <- 0
pisa$family_ses_level_new[pisa$CNT == "QCH" & pisa$ESCS>-1.70870] <- 1

# school_status
# distinguish the last 25% as unprivileged group
aggregate(coun_school$school_status, by =list(coun_school$CNT)
          , FUN = quantile, probs = c(0.25))
pisa$school_status_level_25_per[pisa$CNT == "AUS" & pisa$school_status<=-0.12334698] <- 0
pisa$school_status_level_25_per[pisa$CNT == "AUS" & pisa$school_status>-0.12334698] <- 1
pisa$school_status_level_25_per[pisa$CNT == "GBR" & pisa$school_status<=-0.07433248] <- 0
pisa$school_status_level_25_per[pisa$CNT == "GBR" & pisa$school_status>-0.07433248] <- 1
pisa$school_status_level_25_per[pisa$CNT == "KOR" & pisa$school_status<=-0.41946573] <- 0
pisa$school_status_level_25_per[pisa$CNT == "KOR" & pisa$school_status>-0.41946573] <- 1
pisa$school_status_level_25_per[pisa$CNT == "QCH" & pisa$school_status<=-1.49289539] <- 0
pisa$school_status_level_25_per[pisa$CNT == "QCH" & pisa$school_status>-1.49289539] <- 1

###### The number of students in different types (table4) ######
da_QCH <- pisa[pisa$CNT=='QCH',]
da_KOR <- pisa[pisa$CNT=='KOR',]
da_GBR <- pisa[pisa$CNT=='GBR',]
da_AUS <- pisa[pisa$CNT=='AUS',]
table(da_QCH$shadow_label_num)
table(da_KOR$shadow_label_num)
table(da_GBR$shadow_label_num)
table(da_AUS$shadow_label_num)

###### The mean of students academic performance (table1) ######
pisa_not_attend <- pisa[pisa$shadow_label=="not_attend",]
pisa_only_on <- pisa[pisa$shadow_label=="only_attend_online_edu",]

# rural_urban
na.omit(pisa.mean.pv(pvlabel = "MATH", by = c("CNT", "rural_urban")
                     ,data =pisa_only_on))
na.omit(pisa.mean.pv(pvlabel = "MATH", by = c("CNT", "rural_urban")
                     ,data =pisa_not_attend))

# family_ses
na.omit(pisa.mean.pv(pvlabel = "MATH", by = c("CNT", "family_ses_level_new")
                     ,data =pisa_only_on))

na.omit(pisa.mean.pv(pvlabel = "MATH", by = c("CNT", "family_ses_level_new")
                     ,data =pisa_not_attend))

# school_status
na.omit(pisa.mean.pv(pvlabel = "MATH", by = c("CNT", "school_status_level_25_per")
                     ,data =pisa_only_on))
na.omit(pisa.mean.pv(pvlabel = "MATH", by = c("CNT", "school_status_level_25_per")
                     ,data =pisa_not_attend))
