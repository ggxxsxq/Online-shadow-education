require(robust)

dta<-read.csv("data_online.csv",header=T,na.strings = c("NA"))
dta$course_before<-as.factor(dta$course_before)
dta$city_tier<-as.factor(dta$city_tier)
dta$city_tier<-relevel(dta$city_tier, ref="0") 
dta$school_status<-relevel(dta$school_status, ref="high") 
dta$family_ses<-relevel(dta$family_ses, ref="high") 
dta$rural_urban<-relevel(dta$rural_urban, ref="urban") 

###### DV: academic improvement(table 2) #####
## family ses
model1<-lmRob(improvement~gender+course_before+family_ses+scale(gold_coin),data=dta)
summary(model1)

## rural-urban
model2<-lmRob(improvement~gender+course_before+rural_urban+scale(gold_coin),data=dta)
summary(model2)

## city tiers
model3<-lmRob(improvement~gender+course_before+city_tier+scale(gold_coin),data=dta)
summary(model3)

## school status 
model4<-lmRob(improvement~gender+course_before+school_status+scale(gold_coin),data=dta)
summary(model4)

## Full model
model5<-lmRob(improvement~gender+course_before+family_ses+rural_urban+school_status+city_tier+scale(gold_coin)
              ,data=dta)
summary(model5)


##### DV: post score (table 3)#####
## family ses
model6<-lmRob(scale(post_score)~gender+family_ses+scale(gold_coin)+scale(pre_score)+ course_before,data=dta)
summary(model7)
## family ses * golden coins
model7<-lmRob(scale(post_score)~gender+family_ses*scale(gold_coin)+scale(pre_score)
                + course_before+scale(gold_coin),data=dta)
summary(model7)

## rural-urban
model8<-lmRob(scale(post_score)~gender+rural_urban+scale(gold_coin)+scale(pre_score)+ course_before,data=dta)
summary(model8)
## rural-urban* golden coins
model9<-lmRob(scale(post_score)~gender+rural_urban*scale(gold_coin)+scale(pre_score)
                + course_before+scale(gold_coin),data=dta)
summary(model9)

# city tiers
model10<-lmRob(scale(post_score)~gender+city_tier+scale(gold_coin)+scale(pre_score)+ course_before
               ,data=dta)
summary(model10)
# city tiers * golden coins
model11<-lmRob(scale(post_score)~gender+city_tier*scale(gold_coin)+scale(pre_score)+ course_before
               ,data=dta)
summary(model11)

# school status
model12<-lmRob(scale(post_score)~gender+school_status+scale(gold_coin)+scale(pre_score)+ course_before,data=dta)
summary(model12)
# school status* golden coins
model13<-lmRob(scale(post_score)~gender+school_status*scale(gold_coin)+scale(pre_score)
                 + course_before+scale(gold_coin),data=dta)
summary(model13)

# Full model
model14<-lmRob(scale(post_score)~scale(pre_score)+gender+family_ses*scale(gold_coin)+school_status*scale(gold_coin)+city_tier*scale(gold_coin)+rural_urban*scale(gold_coin)
               + course_before,data=dta)
summary(model14)
