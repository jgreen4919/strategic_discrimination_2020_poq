rm(list = ls())

set.seed(11111)

library(foreign)
library(dplyr)
library(car)
library(cjoint)
library(ltm)
library(cregg)
library(gridExtra)

here::here()

dfp <- read.csv("data/FIRSTDEBATE_DATA.csv")


# Give each respondent a unique caseid so that conjoint analysis SEs can be clustered appropriately
dfp$caseid <- seq.int(nrow(dfp))

#### Create appropriate beat trump variables (1 if candidate can beat trump, 0 otherwise) ####
dfp$beat_trump01_a <- dfp$beat_trump01_1
dfp$beat_trump01_b <- dfp$beat_trump01_2
dfp$beat_trump02_a <- dfp$beat_trump02_1
dfp$beat_trump02_b <- dfp$beat_trump02_2
dfp$beat_trump03_a <- dfp$beat_trump03_1
dfp$beat_trump03_b <- dfp$beat_trump03_2
dfp$beat_trump04_a <- dfp$beat_trump04_1
dfp$beat_trump04_b <- dfp$beat_trump04_2
dfp$beat_trump05_a <- dfp$beat_trump05_1
dfp$beat_trump05_b <- dfp$beat_trump05_2

dfp$beat_trump01_a[dfp$beat_trump01_3==1] <- 1
dfp$beat_trump01_b[dfp$beat_trump01_3==1] <- 1
dfp$beat_trump02_a[dfp$beat_trump02_3==1] <- 1
dfp$beat_trump02_b[dfp$beat_trump02_3==1] <- 1
dfp$beat_trump03_a[dfp$beat_trump03_3==1] <- 1
dfp$beat_trump03_b[dfp$beat_trump03_3==1] <- 1
dfp$beat_trump04_a[dfp$beat_trump04_3==1] <- 1
dfp$beat_trump04_b[dfp$beat_trump04_3==1] <- 1
dfp$beat_trump05_a[dfp$beat_trump05_3==1] <- 1
dfp$beat_trump05_b[dfp$beat_trump05_3==1] <- 1


#### The following code stacks the data so that it can be analyzed properly ####
trial1_a <- dplyr::select(dfp, "caseid", "cje_gen_01_a", "cje_age_01_a", "cje_rac_01_a", "cje_env_01_a", "cje_health_01_a", "cje_stra_01_a", "cje_back_01_a", "DEM_CONJOINT01", "DEM_CONJOINT_MOTIVA01", "beat_trump01_a", "survey_flag", "attribute_order" )
trial1_a <- dplyr::rename(trial1_a, "Gender"= "cje_gen_01_a", "Age"= "cje_age_01_a", 
                          "Race" = "cje_rac_01_a", "Health" = "cje_health_01_a", "Environment" = "cje_env_01_a",
                          "Strategy" = "cje_stra_01_a", "Background" = "cje_back_01_a",  "choice" = "DEM_CONJOINT01", "motivation"= "DEM_CONJOINT_MOTIVA01", "beattrump"="beat_trump01_a")
trial1_a$choice <- car::recode(trial1_a$choice, "2=0")
trial1_a$trial <- 1
trial1_a$profile <- 1

trial1_b <- dplyr::select(dfp, "caseid", "cje_gen_01_b", "cje_age_01_b", "cje_rac_01_b", "cje_env_01_b", "cje_health_01_b", "cje_stra_01_b", "cje_back_01_b", "DEM_CONJOINT01", "DEM_CONJOINT_MOTIVB01", "beat_trump01_b", "survey_flag", "attribute_order"  )
trial1_b <- dplyr::rename(trial1_b, "Gender"= "cje_gen_01_b", "Age"= "cje_age_01_b", 
                          "Race" = "cje_rac_01_b", "Health" = "cje_health_01_b", "Environment" = "cje_env_01_b",
                          "Strategy" = "cje_stra_01_b", "Background" = "cje_back_01_b",  "choice" = "DEM_CONJOINT01", "motivation"= "DEM_CONJOINT_MOTIVB01", "beattrump"="beat_trump01_b")
trial1_b$choice <- car::recode(trial1_b$choice, "1=0; 2=1")
trial1_b$trial <- 1
trial1_b$profile <- 2


trial2_a <- dplyr::select(dfp, "caseid", "cje_gen_02_a", "cje_age_02_a", "cje_rac_02_a", "cje_env_02_a", "cje_health_02_a", "cje_stra_02_a", "cje_back_02_a", "DEM_CONJOINT02", "DEM_CONJOINT_MOTIVA02", "beat_trump02_a", "survey_flag",  "attribute_order"  )
trial2_a <- dplyr::rename(trial2_a, "Gender"= "cje_gen_02_a", "Age"= "cje_age_02_a", 
                          "Race" = "cje_rac_02_a", "Health" = "cje_health_02_a", "Environment" = "cje_env_02_a",
                          "Strategy" = "cje_stra_02_a", "Background" = "cje_back_02_a",  "choice" = "DEM_CONJOINT02", "motivation"= "DEM_CONJOINT_MOTIVA02", "beattrump"="beat_trump02_a")
trial2_a$choice <- car::recode(trial2_a$choice, "2=0")
trial2_a$trial <- 2
trial2_a$profile <- 1

trial2_b <- dplyr::select(dfp, "caseid", "cje_gen_02_b", "cje_age_02_b", "cje_rac_02_b", "cje_env_02_b", "cje_health_02_b", "cje_stra_02_b", "cje_back_02_b", "DEM_CONJOINT02", "DEM_CONJOINT_MOTIVB02", "beat_trump02_b", "survey_flag" ,  "attribute_order" )
trial2_b <- dplyr::rename(trial2_b, "Gender"= "cje_gen_02_b", "Age"= "cje_age_02_b", 
                          "Race" = "cje_rac_02_b", "Health" = "cje_health_02_b", "Environment" = "cje_env_02_b",
                          "Strategy" = "cje_stra_02_b", "Background" = "cje_back_02_b",  "choice" = "DEM_CONJOINT02", "motivation"= "DEM_CONJOINT_MOTIVB02", "beattrump"="beat_trump02_b")
trial2_b$choice <- car::recode(trial2_b$choice, "1=0; 2=1")
trial2_b$trial <- 2
trial2_b$profile <- 2


trial3_a <- dplyr::select(dfp, "caseid", "cje_gen_03_a", "cje_age_03_a", "cje_rac_03_a", "cje_env_03_a", "cje_health_03_a", "cje_stra_03_a", "cje_back_03_a", "DEM_CONJOINT03", "DEM_CONJOINT_MOTIVA03", "beat_trump03_a", "survey_flag",  "attribute_order"  )
trial3_a <- dplyr::rename(trial3_a, "Gender"= "cje_gen_03_a", "Age"= "cje_age_03_a", 
                          "Race" = "cje_rac_03_a", "Health" = "cje_health_03_a", "Environment" = "cje_env_03_a",
                          "Strategy" = "cje_stra_03_a", "Background" = "cje_back_03_a",  "choice" = "DEM_CONJOINT03", "motivation"= "DEM_CONJOINT_MOTIVA03", "beattrump"="beat_trump03_a")
trial3_a$choice <- car::recode(trial3_a$choice, "2=0")
trial3_a$trial <- 3
trial3_a$profile <- 1

trial3_b <- dplyr::select(dfp, "caseid", "cje_gen_03_b", "cje_age_03_b", "cje_rac_03_b", "cje_env_03_b", "cje_health_03_b", "cje_stra_03_b", "cje_back_03_b", "DEM_CONJOINT03", "DEM_CONJOINT_MOTIVB03", "beat_trump03_b", "survey_flag", "attribute_order"  )
trial3_b <- dplyr::rename(trial3_b, "Gender"= "cje_gen_03_b", "Age"= "cje_age_03_b", 
                          "Race" = "cje_rac_03_b", "Health" = "cje_health_03_b", "Environment" = "cje_env_03_b",
                          "Strategy" = "cje_stra_03_b", "Background" = "cje_back_03_b",  "choice" = "DEM_CONJOINT03", "motivation"= "DEM_CONJOINT_MOTIVB03", "beattrump"="beat_trump03_b")
trial3_b$choice <- car::recode(trial3_b$choice, "1=0; 2=1")
trial3_b$trial <- 3
trial3_b$profile <- 2


trial4_a <- dplyr::select(dfp, "caseid", "cje_gen_04_a", "cje_age_04_a", "cje_rac_04_a", "cje_env_04_a", "cje_health_04_a", "cje_stra_04_a", "cje_back_04_a", "DEM_CONJOINT04", "DEM_CONJOINT_MOTIVA04", "beat_trump04_a", "survey_flag", "attribute_order"  )
trial4_a <- dplyr::rename(trial4_a, "Gender"= "cje_gen_04_a", "Age"= "cje_age_04_a", 
                          "Race" = "cje_rac_04_a", "Health" = "cje_health_04_a", "Environment" = "cje_env_04_a",
                          "Strategy" = "cje_stra_04_a", "Background" = "cje_back_04_a",  "choice" = "DEM_CONJOINT04", "motivation"= "DEM_CONJOINT_MOTIVA04", "beattrump"="beat_trump04_a")
trial4_a$choice <- car::recode(trial4_a$choice, "2=0")
trial4_a$trial <- 4
trial4_a$profile <- 1

trial4_b <- dplyr::select(dfp, "caseid", "cje_gen_04_b", "cje_age_04_b", "cje_rac_04_b", "cje_env_04_b", "cje_health_04_b", "cje_stra_04_b", "cje_back_04_b", "DEM_CONJOINT04", "DEM_CONJOINT_MOTIVB04", "beat_trump04_b", "survey_flag",  "attribute_order"  )
trial4_b <- dplyr::rename(trial4_b, "Gender"= "cje_gen_04_b", "Age"= "cje_age_04_b", 
                          "Race" = "cje_rac_04_b", "Health" = "cje_health_04_b", "Environment" = "cje_env_04_b",
                          "Strategy" = "cje_stra_04_b", "Background" = "cje_back_04_b",  "choice" = "DEM_CONJOINT04", "motivation"= "DEM_CONJOINT_MOTIVB04", "beattrump"="beat_trump04_b")
trial4_b$choice <- car::recode(trial4_b$choice, "1=0; 2=1")
trial4_b$trial <- 4
trial4_b$profile <- 2


trial5_a <- dplyr::select(dfp, "caseid", "cje_gen_05_a", "cje_age_05_a", "cje_rac_05_a", "cje_env_05_a", "cje_health_05_a", "cje_stra_05_a", "cje_back_05_a", "DEM_CONJOINT05", "DEM_CONJOINT_MOTIVA05", "beat_trump05_a", "survey_flag",  "attribute_order"  )
trial5_a <- dplyr::rename(trial5_a, "Gender"= "cje_gen_05_a", "Age"= "cje_age_05_a", 
                          "Race" = "cje_rac_05_a", "Health" = "cje_health_05_a", "Environment" = "cje_env_05_a",
                          "Strategy" = "cje_stra_05_a", "Background" = "cje_back_05_a",  "choice" = "DEM_CONJOINT05", "motivation"= "DEM_CONJOINT_MOTIVA05", "beattrump"="beat_trump05_a")
trial5_a$choice <- car::recode(trial5_a$choice, "2=0")
trial5_a$trial <- 5
trial5_a$profile <- 1

trial5_b <- dplyr::select(dfp, "caseid", "cje_gen_05_b", "cje_age_05_b", "cje_rac_05_b", "cje_env_05_b", "cje_health_05_b", "cje_stra_05_b", "cje_back_05_b", "DEM_CONJOINT05", "DEM_CONJOINT_MOTIVB05", "beat_trump05_b", "survey_flag",  "attribute_order"  )
trial5_b <- dplyr::rename(trial5_b, "Gender"= "cje_gen_05_b", "Age"= "cje_age_05_b", 
                          "Race" = "cje_rac_05_b", "Health" = "cje_health_05_b", "Environment" = "cje_env_05_b",
                          "Strategy" = "cje_stra_05_b", "Background" = "cje_back_05_b",  "choice" = "DEM_CONJOINT05", "motivation"= "DEM_CONJOINT_MOTIVB05", "beattrump"="beat_trump05_b")
trial5_b$choice <- car::recode(trial5_b$choice, "1=0; 2=1")
trial5_b$trial <- 5
trial5_b$profile <- 2

dfpcjoint <- rbind(trial1_a, trial2_a, trial3_a,trial4_a, trial5_a, trial1_b, trial2_b, trial3_b,trial4_b, trial5_b)
dfpcjoint <- droplevels(dfpcjoint)

#### Recode attributes so that they exist as text (not numeric) ####
dfpcjoint$Gender <- as.factor(car::recode(dfpcjoint$Gender, "1='Male'; 2='Female'"))
dfpcjoint$Age <- as.factor(car::recode(dfpcjoint$Age, "1='Age 35'; 2='Age 40'; 3='Age 45'; 4='Age 50'; 5='Age 55'; 6='Age 60'; 7='Age 65'; 8='Age 70'; 9='Age 75'"))
dfpcjoint$Race <- as.factor(car::recode(dfpcjoint$Race, "1='White'; 2='Black'; 3='Latino'"))
dfpcjoint$Health <- as.factor(car::recode(dfpcjoint$Health, "1='ACA expansion'; 2='Medicare for all'; 3='Public option'"))
dfpcjoint$Environment <- as.factor(car::recode(dfpcjoint$Environment, "1='Net zero by 2030'; 2='Net zero by 2045'; 3='Net zero by 2050'"))
dfpcjoint$Strategy <- as.factor(car::recode(dfpcjoint$Strategy, "1='Persuade moderates'; 2='Energize base'"))
dfpcjoint$Background <- as.factor(car::recode(dfpcjoint$Background, "1='Establishment'; 2='Outsider'"))
dfpcjoint$motivation <- as.numeric(dfpcjoint$motivation)


#### By trial ####
trial1 <- plot(mm(subset(dfpcjoint, trial==1), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Trial 1") + theme(legend.position="none") + xlim(.44, .56)

trial2 <- plot(mm(subset(dfpcjoint, trial==2), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Trial 2") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.44, .56)

trial3 <- plot(mm(subset(dfpcjoint, trial==3), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Trial 3") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.44, .56)

trial4 <- plot(mm(subset(dfpcjoint, trial==4), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Trial 4") + theme(legend.position="none")+
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.44, .56)

trial5 <- plot(mm(subset(dfpcjoint, trial==5), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Trial 5") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.44, .56)

png(file="output/choice_by_trial.png", width=1200, height=600, res = 150)
egg::ggarrange(trial1, trial2, trial3, trial4, trial5, nrow=1)
dev.off()


#### By profile ####
prof1 <- plot(mm(subset(dfpcjoint, profile==1), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Profile 1") + theme(legend.position="none") + xlim(.44, .56)

prof2 <- plot(mm(subset(dfpcjoint, profile==2), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Profile 2") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.44, .56)

png(file="output/choice_by_profile.png", width=1200, height=600, res = 150)
egg::ggarrange(prof1, prof2, nrow=1)
dev.off()


#### By order ####
order1 <- plot(mm(subset(dfpcjoint, attribute_order==1), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Order 1") + theme(legend.position="none") + xlim(.44, .58)

order2 <- plot(mm(subset(dfpcjoint, attribute_order==2), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Order 2") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.44, .58)

order3 <- plot(mm(subset(dfpcjoint, attribute_order==3), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Order 3") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.44, .58)

order4 <- plot(mm(subset(dfpcjoint, attribute_order==4), choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5)  + labs(title="Order 4") + theme(legend.position="none")+
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.44, .58)

png(file="output/choice_by_order.png", width=1200, height=600, res = 150)
egg::ggarrange(order1, order2, order3, order4, nrow=1)
dev.off()



#### By trial ####
trial1 <- plot(mm(subset(dfpcjoint, trial==1), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Trial 1") + theme(legend.position="none") + xlim(.6, .8)

trial2 <- plot(mm(subset(dfpcjoint, trial==2), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Trial 2") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.6, .8)

trial3 <- plot(mm(subset(dfpcjoint, trial==3), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Trial 3") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.6, .8)

trial4 <- plot(mm(subset(dfpcjoint, trial==4), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Trial 4") + theme(legend.position="none")+
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.6, .8)

trial5 <- plot(mm(subset(dfpcjoint, trial==5), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Trial 5") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.6, .8)

png(file="output/beattrump_by_trial.png", width=1200, height=600, res = 150)
egg::ggarrange(trial1, trial2, trial3, trial4, trial5, nrow=1)
dev.off()


#### By profile ####
prof1 <- plot(mm(subset(dfpcjoint, profile==1), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Profile 1") + theme(legend.position="none") + xlim(.6, .8)

prof2 <- plot(mm(subset(dfpcjoint, profile==2), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Profile 2") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.6, .8)

png(file="output/beattrump_by_profile.png", width=1200, height=600, res = 150)
egg::ggarrange(prof1, prof2, nrow=1)
dev.off()


#### By order ####
order1 <- plot(mm(subset(dfpcjoint, attribute_order==1), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Order 1") + theme(legend.position="none") + xlim(.6, .8)

order2 <- plot(mm(subset(dfpcjoint, attribute_order==2), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Order 2") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.6, .8)

order3 <- plot(mm(subset(dfpcjoint, attribute_order==3), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Order 3") + theme(legend.position="none") +
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.6, .8)

order4 <- plot(mm(subset(dfpcjoint, attribute_order==4), beattrump ~ Gender  + Race , id = ~caseid), vline = 0.726, feature_headers=FALSE, size=1.5)  + labs(title="Order 4") + theme(legend.position="none")+
  theme(axis.title.y = element_blank(),axis.text.y=element_blank())  + xlim(.6, .8)

png(file="output/beattrump_by_order.png", width=1200, height=600, res = 150)
egg::ggarrange(order1, order2, order3, order4, nrow=1)
dev.off()

