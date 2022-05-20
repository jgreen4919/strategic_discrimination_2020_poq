rm(list = ls())

set.seed(1111)

library(foreign)
library(dplyr)
library(car)
library(cjoint)
library(ltm)
library(cregg)
library(gridExtra)
library(patchwork)
library(here)

here::here()

dfp <- read.csv("data/FIRSTDEBATE_DATA.csv")

# Create  sexism scale that is just avearge ####
dfp$sexism1<-dplyr::recode(dfp$REMARKS, `5`=1, `4`=2, `3`=3, `2`=4, `1`=5)
dfp$sexism2<-dplyr::recode(dfp$OFFEND, `5`=1, `4`=2, `3`=3, `2`=4, `1`=5)
dfp$sexism3<-dplyr::recode(dfp$APPRECIATE,`5`=1, `4`=2, `3`=3, `2`=4, `1`=5)
dfp$sexism4<-dplyr::recode(dfp$CONTROL, `5`=1, `4`=2, `3`=3, `2`=4, `1`=5)

dfp$sexismavg <- rowMeans(subset(dfp, select=c("sexism1", "sexism2", "sexism3", "sexism4"), na.rm=T))

# Create  racism scale that is just average ####
dfp$rr1<-dplyr::recode(dfp$FAVORS, `5`=1, `4`=2, `3`=3, `2`=4, `1`=5)
dfp$rr2<-dplyr::recode(dfp$GENERATIONS, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5)

dfp$rravg <- rowMeans(subset(dfp, select=c("rr1", "rr2"), na.rm=T))

# Create FIRE scale
dfp$fire1<-dplyr::recode(dfp$SYSTEM, `5`=1, `4`=2, `3`=3, `2`=4, `1`=5)
dfp$fire2<-dplyr::recode(dfp$INSTITUTION, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5)
dfp$fire3<-dplyr::recode(dfp$EMPATHY, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5)

dfp$fireavg <- rowMeans(subset(dfp, select=c("fire1", "fire2", "fire3"), na.rm=T))


# Create variable for POC and Women
dfp$sex <- as.factor(car::recode(dfp$gender, "1='Men'; 2='Women'"))
dfp$POC <- as.factor(car::recode(dfp$race2, "1='Whites'; 2='POC'"))

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
trial1_a <- dplyr::select(dfp, "caseid", "cje_gen_01_a", "cje_age_01_a", "cje_rac_01_a", "cje_env_01_a", "cje_health_01_a", "cje_stra_01_a", "cje_back_01_a", "DEM_CONJOINT01", "DEM_CONJOINT_MOTIVA01", "beat_trump01_a", "survey_flag", "sexismavg", "birthyr", "sex", "POC", "rravg", "fireavg", "attribute_order" )
trial1_a <- dplyr::rename(trial1_a, "Gender"= "cje_gen_01_a", "Age"= "cje_age_01_a", 
                        "Race" = "cje_rac_01_a", "Health" = "cje_health_01_a", "Environment" = "cje_env_01_a",
                        "Strategy" = "cje_stra_01_a", "Background" = "cje_back_01_a",  "choice" = "DEM_CONJOINT01", "motivation"= "DEM_CONJOINT_MOTIVA01", "beattrump"="beat_trump01_a")
trial1_a$choice <- car::recode(trial1_a$choice, "2=0")
trial1_a$trial <- 1
trial1_a$profile <- 1

trial1_b <- dplyr::select(dfp, "caseid", "cje_gen_01_b", "cje_age_01_b", "cje_rac_01_b", "cje_env_01_b", "cje_health_01_b", "cje_stra_01_b", "cje_back_01_b", "DEM_CONJOINT01", "DEM_CONJOINT_MOTIVB01", "beat_trump01_b", "survey_flag", "sexismavg", "birthyr", "sex", "POC", "rravg", "fireavg", "attribute_order"  )
trial1_b <- dplyr::rename(trial1_b, "Gender"= "cje_gen_01_b", "Age"= "cje_age_01_b", 
                          "Race" = "cje_rac_01_b", "Health" = "cje_health_01_b", "Environment" = "cje_env_01_b",
                          "Strategy" = "cje_stra_01_b", "Background" = "cje_back_01_b",  "choice" = "DEM_CONJOINT01", "motivation"= "DEM_CONJOINT_MOTIVB01", "beattrump"="beat_trump01_b")
trial1_b$choice <- car::recode(trial1_b$choice, "1=0; 2=1")
trial1_b$trial <- 1
trial1_b$profile <- 2


trial2_a <- dplyr::select(dfp, "caseid", "cje_gen_02_a", "cje_age_02_a", "cje_rac_02_a", "cje_env_02_a", "cje_health_02_a", "cje_stra_02_a", "cje_back_02_a", "DEM_CONJOINT02", "DEM_CONJOINT_MOTIVA02", "beat_trump02_a", "survey_flag", "sexismavg", "birthyr", "sex", "POC", "rravg", "fireavg", "attribute_order"  )
trial2_a <- dplyr::rename(trial2_a, "Gender"= "cje_gen_02_a", "Age"= "cje_age_02_a", 
                          "Race" = "cje_rac_02_a", "Health" = "cje_health_02_a", "Environment" = "cje_env_02_a",
                          "Strategy" = "cje_stra_02_a", "Background" = "cje_back_02_a",  "choice" = "DEM_CONJOINT02", "motivation"= "DEM_CONJOINT_MOTIVA02", "beattrump"="beat_trump02_a")
trial2_a$choice <- car::recode(trial2_a$choice, "2=0")
trial2_a$trial <- 2
trial2_a$profile <- 1

trial2_b <- dplyr::select(dfp, "caseid", "cje_gen_02_b", "cje_age_02_b", "cje_rac_02_b", "cje_env_02_b", "cje_health_02_b", "cje_stra_02_b", "cje_back_02_b", "DEM_CONJOINT02", "DEM_CONJOINT_MOTIVB02", "beat_trump02_b", "survey_flag" , "sexismavg", "birthyr", "sex", "POC", "rravg", "fireavg", "attribute_order" )
trial2_b <- dplyr::rename(trial2_b, "Gender"= "cje_gen_02_b", "Age"= "cje_age_02_b", 
                          "Race" = "cje_rac_02_b", "Health" = "cje_health_02_b", "Environment" = "cje_env_02_b",
                          "Strategy" = "cje_stra_02_b", "Background" = "cje_back_02_b",  "choice" = "DEM_CONJOINT02", "motivation"= "DEM_CONJOINT_MOTIVB02", "beattrump"="beat_trump02_b")
trial2_b$choice <- car::recode(trial2_b$choice, "1=0; 2=1")
trial2_b$trial <- 2
trial2_b$profile <- 2


trial3_a <- dplyr::select(dfp, "caseid", "cje_gen_03_a", "cje_age_03_a", "cje_rac_03_a", "cje_env_03_a", "cje_health_03_a", "cje_stra_03_a", "cje_back_03_a", "DEM_CONJOINT03", "DEM_CONJOINT_MOTIVA03", "beat_trump03_a", "survey_flag", "sexismavg", "birthyr", "sex", "POC", "rravg", "fireavg", "attribute_order"  )
trial3_a <- dplyr::rename(trial3_a, "Gender"= "cje_gen_03_a", "Age"= "cje_age_03_a", 
                          "Race" = "cje_rac_03_a", "Health" = "cje_health_03_a", "Environment" = "cje_env_03_a",
                          "Strategy" = "cje_stra_03_a", "Background" = "cje_back_03_a",  "choice" = "DEM_CONJOINT03", "motivation"= "DEM_CONJOINT_MOTIVA03", "beattrump"="beat_trump03_a")
trial3_a$choice <- car::recode(trial3_a$choice, "2=0")
trial3_a$trial <- 3
trial3_a$profile <- 1

trial3_b <- dplyr::select(dfp, "caseid", "cje_gen_03_b", "cje_age_03_b", "cje_rac_03_b", "cje_env_03_b", "cje_health_03_b", "cje_stra_03_b", "cje_back_03_b", "DEM_CONJOINT03", "DEM_CONJOINT_MOTIVB03", "beat_trump03_b", "survey_flag", "sexismavg", "birthyr", "sex", "POC", "rravg", "fireavg", "attribute_order"  )
trial3_b <- dplyr::rename(trial3_b, "Gender"= "cje_gen_03_b", "Age"= "cje_age_03_b", 
                          "Race" = "cje_rac_03_b", "Health" = "cje_health_03_b", "Environment" = "cje_env_03_b",
                          "Strategy" = "cje_stra_03_b", "Background" = "cje_back_03_b",  "choice" = "DEM_CONJOINT03", "motivation"= "DEM_CONJOINT_MOTIVB03", "beattrump"="beat_trump03_b")
trial3_b$choice <- car::recode(trial3_b$choice, "1=0; 2=1")
trial3_b$trial <- 3
trial3_b$profile <- 2


trial4_a <- dplyr::select(dfp, "caseid", "cje_gen_04_a", "cje_age_04_a", "cje_rac_04_a", "cje_env_04_a", "cje_health_04_a", "cje_stra_04_a", "cje_back_04_a", "DEM_CONJOINT04", "DEM_CONJOINT_MOTIVA04", "beat_trump04_a", "survey_flag", "sexismavg", "birthyr", "sex", "POC", "rravg", "fireavg", "attribute_order"  )
trial4_a <- dplyr::rename(trial4_a, "Gender"= "cje_gen_04_a", "Age"= "cje_age_04_a", 
                          "Race" = "cje_rac_04_a", "Health" = "cje_health_04_a", "Environment" = "cje_env_04_a",
                          "Strategy" = "cje_stra_04_a", "Background" = "cje_back_04_a",  "choice" = "DEM_CONJOINT04", "motivation"= "DEM_CONJOINT_MOTIVA04", "beattrump"="beat_trump04_a")
trial4_a$choice <- car::recode(trial4_a$choice, "2=0")
trial4_a$trial <- 4
trial4_a$profile <- 1

trial4_b <- dplyr::select(dfp, "caseid", "cje_gen_04_b", "cje_age_04_b", "cje_rac_04_b", "cje_env_04_b", "cje_health_04_b", "cje_stra_04_b", "cje_back_04_b", "DEM_CONJOINT04", "DEM_CONJOINT_MOTIVB04", "beat_trump04_b", "survey_flag", "sexismavg", "birthyr", "sex", "POC", "rravg", "fireavg", "attribute_order"  )
trial4_b <- dplyr::rename(trial4_b, "Gender"= "cje_gen_04_b", "Age"= "cje_age_04_b", 
                          "Race" = "cje_rac_04_b", "Health" = "cje_health_04_b", "Environment" = "cje_env_04_b",
                          "Strategy" = "cje_stra_04_b", "Background" = "cje_back_04_b",  "choice" = "DEM_CONJOINT04", "motivation"= "DEM_CONJOINT_MOTIVB04", "beattrump"="beat_trump04_b")
trial4_b$choice <- car::recode(trial4_b$choice, "1=0; 2=1")
trial4_b$trial <- 4
trial4_b$profile <- 2


trial5_a <- dplyr::select(dfp, "caseid", "cje_gen_05_a", "cje_age_05_a", "cje_rac_05_a", "cje_env_05_a", "cje_health_05_a", "cje_stra_05_a", "cje_back_05_a", "DEM_CONJOINT05", "DEM_CONJOINT_MOTIVA05", "beat_trump05_a", "survey_flag", "sexismavg", "birthyr", "sex", "POC", "rravg", "fireavg", "attribute_order"  )
trial5_a <- dplyr::rename(trial5_a, "Gender"= "cje_gen_05_a", "Age"= "cje_age_05_a", 
                          "Race" = "cje_rac_05_a", "Health" = "cje_health_05_a", "Environment" = "cje_env_05_a",
                          "Strategy" = "cje_stra_05_a", "Background" = "cje_back_05_a",  "choice" = "DEM_CONJOINT05", "motivation"= "DEM_CONJOINT_MOTIVA05", "beattrump"="beat_trump05_a")
trial5_a$choice <- car::recode(trial5_a$choice, "2=0")
trial5_a$trial <- 5
trial5_a$profile <- 1

trial5_b <- dplyr::select(dfp, "caseid", "cje_gen_05_b", "cje_age_05_b", "cje_rac_05_b", "cje_env_05_b", "cje_health_05_b", "cje_stra_05_b", "cje_back_05_b", "DEM_CONJOINT05", "DEM_CONJOINT_MOTIVB05", "beat_trump05_b", "survey_flag", "sexismavg", "birthyr", "sex", "POC", "rravg", "fireavg", "attribute_order"  )
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
dfpcjoint$prepost <- as.factor(car::recode(dfpcjoint$survey_flag, "1='Pre wave'; 2='Post wave'"))
dfpcjoint$rage <- as.factor(car::recode(dfpcjoint$birthyr, "1985:2000='18 - 34'; 1960:1984='35 - 60'; 1922:1959='60 +'"))

dfpcjoint$Inter[dfpcjoint$Gender=="Male" & dfpcjoint$Race=="Latino"] <- "Latino Male"
dfpcjoint$Inter[dfpcjoint$Gender=="Female" & dfpcjoint$Race=="Latino"] <- "Latina Female"
dfpcjoint$Inter[dfpcjoint$Gender=="Male" & dfpcjoint$Race=="Black"] <- "Black Male"
dfpcjoint$Inter[dfpcjoint$Gender=="Female" & dfpcjoint$Race=="Black"] <- "Black Female"
dfpcjoint$Inter[dfpcjoint$Gender=="Male" & (dfpcjoint$Race=="White")] <- "White Male"
dfpcjoint$Inter[dfpcjoint$Gender=="Female" & (dfpcjoint$Race=="White")] <- "White Female"
dfpcjoint$Inter <- as.factor(dfpcjoint$Inter)

# Create sexist variable
dfpcjoint$sexist <- as.factor(car::recode(dfpcjoint$sexismav, "1:2.25='(1) Low sexism';  2.5:3.5='(2) Medium sexism'; 3.75:5='(3) High sexism'"))
dfpcjoint$sexist <- ordered(dfpcjoint$sexist, levels=c("(1) Low sexism", "(2) Medium sexism", "(3) High sexism"))

# Create rr variable
dfpcjoint$racist <- as.factor(car::recode(dfpcjoint$rravg, "1:2.25='(1) Low resentment';  2.5:3.5='(2) Medium resentment'; 3.75:5='(3) High resentment'"))
dfpcjoint$racist <- ordered(dfpcjoint$racist, levels=c("(1) Low resentment", "(2) Medium resentment", "(3) High resentment"))

# Create FIRE variable
dfpcjoint$denial <- as.factor(car::recode(dfpcjoint$fireavg, "1:2.25='(1) Low denial';  2.5:3.5='(2) Medium denial'; 3.75:5='(3) High denial'"))
dfpcjoint$denial <- ordered(dfpcjoint$denial, levels=c("(1) Low denial", "(2) Medium denial", "(3) High denial"))


#### Plot Marginal Means for sex and race ####
choice <- plot(mm(dfpcjoint, choice ~ Gender  + Race , id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5) +
  ggplot2::facet_wrap(~feature, ncol=1, scales = "free_y", strip.position = "right") + labs(title="Prefer candidate as nominee") + theme(legend.position="none")

beattrump <- plot(mm(dfpcjoint, beattrump ~ Gender  + Race , id = ~caseid), vline = .726, feature_headers=FALSE, size=1.5) +
  ggplot2::facet_wrap(~feature, ncol=1, scales = "free_y", strip.position = "right") + labs(title="Candidate can beat Trump") + theme(legend.position="none")

figure_1 <- choice + beattrump

ggsave(figure_1, file = "output/figure_1.pdf", width = 8, height = 4)

#### Plot Marginal Means for all attributes ####
choice <- plot(mm(dfpcjoint, choice ~ Gender + Health + Race + Environment + Age + Strategy + Background, id = ~caseid), vline = 0.5, feature_headers=FALSE, size=1.5) +
  ggplot2::facet_wrap(~feature, ncol=1, scales = "free_y", strip.position = "right") + labs(title="Prefer candidate as nominee") + theme(legend.position="none")

beattrump <- plot(mm(dfpcjoint, beattrump ~ Gender + Health + Race + Environment + Age + Strategy + Background, id = ~caseid), vline = .726, feature_headers=FALSE, size=1.5) +
  ggplot2::facet_wrap(~feature, ncol=1, scales = "free_y", strip.position = "right") + labs(title="Candidate can beat Trump") + theme(legend.position="none")

png(file="output/cjresultsSI.png", width=1200, height=800, res = 150)
grid.arrange(choice, beattrump, nrow=1)
dev.off()


#### Plot effects by low vs. high sexism ####

choice <- cj(dfpcjoint, choice ~ Gender , id = ~caseid, by=~sexist, estimate="mm")
choice <- plot(choice, group = "BY", vline = 0.5, feature_headers=FALSE, size=1.5) + 
  ggplot2::facet_wrap(~sexist, ncol=1, scales = "free_y", strip.position = "right") + 
  labs(title="Prefer candidate as nominee") + theme(legend.position = "none")

beattrump <- cj(dfpcjoint, beattrump ~ Gender , id = ~caseid, by=~sexist, estimate="mm")
beattrump <- plot(beattrump, group = "BY", vline = 0.73, feature_headers=FALSE, size=1.5) + 
  ggplot2::facet_wrap(~sexist, ncol=1, scales = "free_y", strip.position = "right") + 
  labs(title="Candidate can beat Trump") + theme(legend.position = "none")

figure_2a <- choice + beattrump

ggsave(figure_2a, file = "output/figure_2a.pdf", width = 8, height = 6)

#### Plot effects by low vs. high racism ####

choice <- cj(dfpcjoint, choice ~ Race , id = ~caseid, by=~racist, estimate="mm")
choice <- plot(choice, group = "BY", vline = 0.5, feature_headers=FALSE, size=1.5) + 
  ggplot2::facet_wrap(~racist, ncol=1, scales = "free_y", strip.position = "right") + 
  labs(title="Prefer candidate as nominee") + theme(legend.position = "none")

beattrump <- cj(dfpcjoint, beattrump ~ Race , id = ~caseid, by=~racist, estimate="mm")
beattrump <- plot(beattrump, group = "BY", vline = 0.73, feature_headers=FALSE, size=1.5) + 
  ggplot2::facet_wrap(~racist, ncol=1, scales = "free_y", strip.position = "right") + 
  labs(title="Candidate can beat Trump") + theme(legend.position = "none")

figure_2b <- choice + beattrump

ggsave(figure_2b, file = "output/figure_2b.pdf", width = 8, height = 6)

#### Plot effects by low vs. high denial of racism ####

choice <- cj(dfpcjoint, choice ~ Race , id = ~caseid, by=~denial, estimate="mm")
choice <- plot(choice, group = "BY", vline = 0.5, feature_headers=FALSE, size=1.5) + 
  ggplot2::facet_wrap(~denial, ncol=1, scales = "free_y", strip.position = "right") + 
  labs(title="Prefer candidate as nominee") + theme(legend.position = "none")

beattrump <- cj(dfpcjoint, beattrump ~ Race , id = ~caseid, by=~denial, estimate="mm")
beattrump <- plot(beattrump, group = "BY", vline = 0.73, feature_headers=FALSE, size=1.5) + 
  ggplot2::facet_wrap(~denial, ncol=1, scales = "free_y", strip.position = "right") + 
  labs(title="Candidate can beat Trump") + theme(legend.position = "none")

png(file="output/cj_bydenial.png", width=1200, height=800, res = 150)
grid.arrange(choice, beattrump, nrow=1)
dev.off()


#### Plot inter-sectional effects ####

choice <- cj(dfpcjoint, choice ~ Inter , id = ~caseid, estimate="mm")
choice <- plot(choice, vline = 0.5, feature_headers=FALSE, size=1.5) + 
  ggplot2::facet_wrap(~feature, ncol=1, scales = "free_y", strip.position = "right") + labs(title="Prefer candidate as nominee") + theme(legend.position="none", strip.background = element_blank(), strip.text = element_blank())

beattrump <- cj(dfpcjoint, beattrump ~ Inter , id = ~caseid, estimate="mm")
beattrump <- plot(beattrump, vline = 0.73, feature_headers=FALSE, size=1.5) + 
  ggplot2::facet_wrap(~feature, ncol=1, scales = "free_y", strip.position = "right") + labs(title="Candidate can beat Trump") + theme(legend.position="none", strip.background = element_blank(), strip.text = element_blank())

figure_3 <- choice + beattrump

ggsave(figure_3, file = "output/figure_3.pdf", width = 8, height = 4)


# Mediation analysis ####
library(mediation)

dfpcjoint <- subset(dfpcjoint, !is.na(beattrump) & !is.na(choice))

dfpcjoint$Female <- 0
dfpcjoint$Female[dfpcjoint$Gender=="Female"] <- 1
dfpcjoint$POC <- 1
dfpcjoint$POC[dfpcjoint$Race=="White"] <- 0

# Gender

model.m <- glm(beattrump ~ Female,
               data = dfpcjoint, family = binomial(link = "probit"))
model.y <- lm(choice ~ Female + beattrump , data = dfpcjoint)

out.gender <- mediate(model.m, model.y, sims = 1000,
                 boot = TRUE, treat = "Female", mediator = "beattrump")
sens.out <- medsens(out.gender, rho.by = 0.1, effect.type = "indirect",
                    sims = 100)

png(file="output/pmed_gender.png", width=1275, height=1013, res = 165)
plot(sens.out, sens.par = "rho", ylim = c(-0.04, 0.04))
dev.off()             

save(model.m, model.y, out.gender, sens.out, file = "output/mediation_gender.rdata")

# Race

model.m <- glm(beattrump ~ POC,
               data = dfpcjoint, family = binomial(link = "probit"))
model.y <- lm(choice ~ POC + beattrump , data = dfpcjoint)

out.race <- mediate(model.m, model.y, sims = 1000,
                 boot = TRUE, treat = "POC", mediator = "beattrump")
sens.out <- medsens(out.race, rho.by = 0.1, effect.type = "indirect",
                    sims = 100)
png(file="output/pmed_race.png", width=1275, height=1013, res = 165)
plot(sens.out, sens.par = "rho", ylim = c(-0.06, 0.06))
dev.off() 

save(model.m, model.y, out.race, sens.out, file = "output/mediation_race.rdata")

