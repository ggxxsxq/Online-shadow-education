## ANOVA ANALYSIS##
setwd("C:/Users/cecilexgao/Desktop/lbs/code/")
dta<-read.csv("data_online.csv",header=T,na.strings = c("NA"))

####################school status#################
aov_school <- aov(improvement~school_status, dta)         
aov_school
summary(aov_school)
tukey_school <- TukeyHSD(aov_school)
tukey_school = as.data.frame(tukey_school$school_status)
tukey_school$pair = rownames(tukey_school)

# Plot pairwise TukeyHSD comparisons and color by significance level
ggplot(tukey_school, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                                    label=c("p<0.01","p<0.05","Non-Sig")))) +
  scale_color_manual(values = c("#EE2C2C","#1E90FF"))+
  theme_bw(base_size = 16)+
  geom_hline(yintercept=0, lty="11", colour="grey30",size = 1) +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size = 1,) +
  geom_point(aes(pair, diff),size = 2) +
  labs(colour="")+
  theme(axis.text.x = element_text(size = 14))


############### urban/rural ###############
dta$rural_urban<-as.factor(dta$rural_urban)
dta$rural_urban<-relevel(dta$rural_urban,ref='urban')
aov_region <- aov(improvement~rural_urban, dta)         
aov_region
summary(aov_region)
tukey_region <- TukeyHSD(aov_region)
tukey_region = as.data.frame(tukey_region$rural_urban)
tukey_region$pair = rownames(tukey_region)

# Plot pairwise TukeyHSD comparisons and color by significance level
ggplot(tukey_region, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                                    label=c("p<0.01","p<0.05","Non-Sig")))) +
  scale_color_manual(values = c("#EE2C2C","#1E90FF"))+
  theme_bw(base_size = 16)+
  geom_hline(yintercept=0, lty="11", colour="grey30",size = 1) +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size = 1,) +
  geom_point(aes(pair, diff),size = 2) +
  labs(colour="")+
  theme(axis.text.x = element_text(size = 14))

############ family SES################
aov_family <- aov(dta$improvement~family_ses, dta)         
aov_family
summary(aov_family)
tukey_family <- TukeyHSD(aov_family)
tukey_family = as.data.frame(tukey_family$family_ses)
tukey_family$pair = rownames(tukey_family)


# Plot pairwise TukeyHSD comparisons and color by significance level
ggplot(tukey_family,aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                                   label=c("p<0.01","p<0.05","Non-Sig")))) +
  theme_bw(base_size = 16)+
  geom_hline(yintercept=0, lty="11", colour="grey30",size = 1) +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size = 1,colour="#1E90FF") +
  geom_point(aes(pair, diff),size = 2,colour="#1E90FF") +
  labs(colour="")+
  theme(axis.text.x = element_text(size = 14))


###############city tiers ############
dta$city_tier<-as.factor(dta$city_tier)
dta$city_tier<-relevel(dta$city_tier, ref="0") 
aov_tier <- aov(dta$improvement~city_tier, dta)         
aov_tier
summary(aov_tier)
tukey_tier <- TukeyHSD(aov_tier)
tukey_tier = as.data.frame(tukey_tier$city_tier)
tukey_tier$pair = rownames(tukey_tier)

# Plot pairwise TukeyHSD comparisons and color by significance level
ggplot(tukey_tier, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                                  label=c("p<0.01","p<0.05","Non-Sig")))) +
  theme_bw(base_size = 16)+
  geom_hline(yintercept=0, lty="11", colour="grey30",size = 1) +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size = 1) +
  geom_point(aes(pair, diff),size = 2) +
  labs(colour="")+
  theme(axis.text.x = element_text(size = 14))

