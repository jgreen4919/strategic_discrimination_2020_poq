# Mediation analysis ####
library(mediation)
library(data.table)
library(tidyverse)
library(dtplyr)
library(dplyr)
library(car)
library(cjoint)
library(ltm)
library(cregg)
library(gridExtra)
library(srvyr)

here::here()

# Read in and merge data ####
wave1 <- read.csv("data/FIRSTDEBATE_DATA.csv", stringsAsFactors = FALSE)

wave1$id <- 1:nrow(wave1)

# Create preference vs. magic wand by gender in each dataset ####
women_num <- c(9, 10, 12, 15, 22, 23)

wave1$votegender <- as.numeric(wave1$DEMPRIM20_horserace %in% women_num)
wave1$votegender[wave1$DEMPRIM20_horserace == 97] <- NA
wave1$wandgender <- as.numeric(wave1$MAGICWAND20 %in% women_num)
wave1$wandgender[wave1$DEMPRIM20_horserace == 97] <- NA

#car::recode(wave1$DEMPRIM20_horserace,"'Amy Klobuchar=1; 'Elizabeth Warren'=1; 'Kamala Harris'=1; 'Kirsten Gillibrand'=1; 'Marianne Williamson'=1; 'Tulsi Gabbard'=1; 'None of these'=NA; else=0")
#car::recode(wave1$MAGICWAND20,"'Amy Klobuchar'=1; 'Elizabeth Warren'=1; 'Kamala Harris'=1; 'Kirsten Gillibrand'=1; 'Marianne Williamson'=1; 'Tulsi Gabbard'=1; 'None of these'=NA; else=0")
  
wave1$group[wave1$votegender==0 & wave1$wandgender==0] <- "Man/Man"
wave1$group[wave1$votegender==1 & wave1$wandgender==1] <- "Woman/Woman"
wave1$group[wave1$votegender==0 & wave1$wandgender==1] <- "Man/Woman"
wave1$group[wave1$votegender==1 & wave1$wandgender==0] <- "Woman/Man"
wave1$group[wave1$DEMPRIM20_horserace==wave1$MAGICWAND20] <- "Same"

# Create preference vs. magic wand by POC in WAVE 1 ####
poc_num <- c(3, 6, 9, 12, 16, 24)

wave1$votePOC <- as.numeric(wave1$DEMPRIM20_horserace %in% poc_num)
wave1$votePOC[wave1$DEMPRIM20_horserace == 97] <- NA
wave1$wandPOC <- as.numeric(wave1$MAGICWAND20 %in% poc_num)
wave1$wandPOC[wave1$DEMPRIM20_horserace == 97] <- NA

#wave1$votePOC <- car::recode(wave1$DEMPRIM20_horserace,"'Andrew Yang'=1; 'Cory Booker'=1; 'Kamala Harris'=1; 'Julián Castro'=1; 'Wayne Messam'=1; 'Tulsi Gabbard'=1; 'None of these'=NA; else=0")
#wave1$wandPOC <- car::recode(wave1$MAGICWAND20,"'Andrew Yang'=1; 'Cory Booker'=1; 'Kamala Harris'=1; 'Julián Castro'=1; 'Wayne Messam'=1; 'Tulsi Gabbard'=1; 'None of these'=NA; else=0")

wave1$groupPOC[wave1$votePOC==0 & wave1$wandPOC==0] <- "White/White"
wave1$groupPOC[wave1$votePOC==1 & wave1$wandPOC==1] <- "POC/POC"
wave1$groupPOC[wave1$votePOC==0 & wave1$wandPOC==1] <- "White/POC"
wave1$groupPOC[wave1$votePOC==1 & wave1$wandPOC==0] <- "POC/White"
wave1$groupPOC[wave1$DEMPRIM20_horserace==wave1$MAGICWAND20] <- "Same"

# Create binaries for ones I want to test
wave1$manwoman <- 0
wave1$manwoman[wave1$group=="Man/Woman"] <- 1
wave1$womanman <- 0
wave1$womanman[wave1$group=="Woman/Man"] <- 1

# Proportion in  each category (Tables 1 and 2) ####
svywave1 <- svydesign(id=~id, weights=~weight_fullsample, data=wave1)

wave1.s <- srvyr::as_survey(wave1, weights = weight_fullsample)

### magic wand stuff ###
t <- data.frame(round(prop.table(svytable(~group, svywave1)), 3))
names(t) <- c("Vote/Wand","Proportion")

write.csv(t, file = "output/votewand_table.csv")

votewand_cands <- 
  wave1.s %>%
  filter(!is.na(MAGICWAND20)) %>%
  mutate(MAGICWAND20 = factor(MAGICWAND20)) %>%
  group_by(MAGICWAND20) %>%
  survey_tally() %>%
  mutate(prop_wand = n/sum(n)) %>%
  dplyr::select(MAGICWAND20, prop_wand) %>%
  rename(cand = MAGICWAND20) %>%
  left_join(wave1.s %>%
              filter(!is.na(DEMPRIM20_horserace)) %>%
              mutate(DEMPRIM20_horserace = factor(DEMPRIM20_horserace)) %>%
              group_by(DEMPRIM20_horserace) %>%
              survey_tally() %>%
              mutate(prop_cand = n/sum(n)) %>%
              rename(cand = DEMPRIM20_horserace) %>%
              dplyr::select(cand, prop_cand),
            by = "cand") %>%
  arrange(desc(prop_cand))

votewand_cands_plot <- 
  votewand_cands %>%
  filter(prop_wand > .01 & prop_cand > .01) %>%
  arrange(desc(prop_cand)) %>%
  mutate(candf = as.factor(as.character(cand))) %>%
  ggplot(aes(x = fct_rev(fct_inorder(candf)), 
             xend = fct_rev(fct_inorder(candf))))+
  geom_point(aes(y = prop_wand, col = "1wand"))+
  geom_point(aes(y = prop_cand, col = "2vote"))+
  geom_segment(aes(y = prop_wand, yend = prop_cand),
               arrow = arrow(length = unit(.2, "cm")))+
  scale_color_manual(name = "Preference Type",
                     breaks = c("1wand","2vote"),
                     values = c("grey","blue"),
                     labels = c("Magic Wand",
                                "Vote Intention"))+
  scale_x_discrete(name = "",
                   breaks = c(2,22,20,12,5,97,18,3,24),
                   labels = c("Joe Biden","Elizabeth Warren","Bernie Sanders",
                              "Kamala Harris","Pete Buttigieig","None of These",
                              "Beto O'Rourke","Cory Booker","Andrew Yang"))+
  coord_flip()+
  labs(title = "Magic Wand vs. Vote Intention",
       subtitle = "Among candidates receiving at least 1% in both measures of candidate preference",
       y = "Proportion")+
  theme_bw()+
  theme(text = element_text(family = "serif", size = 14),
        plot.title = element_text(face = "bold", size = 20))
ggsave(votewand_cands_plot, file = "output/votewand_plot.png", width = 10, height = 5)


### Mediation analysis stuff ###

# Create  sexism scale that is just avearge ####
wave1$sexism1<-dplyr::recode(wave1$REMARKS, `5`=1, `4`=2, `3`=3, `2`=4, `1`=5)
wave1$sexism2<-dplyr::recode(wave1$OFFEND, `5`=1, `4`=2, `3`=3, `2`=4, `1`=5)
wave1$sexism3<-dplyr::recode(wave1$APPRECIATE,`5`=1, `4`=2, `3`=3, `2`=4, `1`=5)
wave1$sexism4<-dplyr::recode(wave1$CONTROL, `5`=1, `4`=2, `3`=3, `2`=4, `1`=5)

wave1$sexismavg <- rowMeans(subset(wave1, select=c("sexism1", "sexism2", "sexism3", "sexism4"), na.rm=T))

# Create  racism scale that is just avearge ####
wave1$rr1<-dplyr::recode(wave1$FAVORS, `5`=1, `4`=2, `3`=3, `2`=4, `1`=5)
wave1$rr2<-dplyr::recode(wave1$GENERATIONS, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5)

wave1$rravg <- rowMeans(subset(wave1, select=c("rr1", "rr2"), na.rm=T))

# Show distribution of sexism/RR scores with cutoffs
ggplot(wave1, aes(x=sexismavg)) + geom_bar(prop) 

sexism  <- ggplot(wave1) + 
  geom_bar(aes(x = sexismavg, y = ..prop.., group = 1), stat = "count", width=.2) + ylim(0,.3) +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Percent") +  xlab("Average agreement with\nhostile sexism statements") +
  theme_classic() + 
  geom_vline(xintercept=2.4) + 
  annotate("text", x=1.75, y=.3, label= "Low\nsexism") +
  geom_vline(xintercept=3.6) + 
  annotate("text", x=3, y=.3, label= "Medium\nsexism") +
  annotate("text", x=4.25, y=.3, label= "High\nsexism") 

resentment  <- ggplot(wave1) + 
  geom_bar(aes(x = rravg, y = ..prop.., group = 1), stat = "count",  width=.2) + 
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Percent") +  xlab("Average agreement with\nracial resentment statements") +
  theme_classic() + 
  geom_vline(xintercept=2.4) + 
  annotate("text", x=1.75, y=.3, label= "Low\nresentment") +
  geom_vline(xintercept=3.6) + 
  annotate("text", x=3, y=.3, label= "Medium\nresentment") +
  annotate("text", x=4.25, y=.3, label= "High\nresentment") 

png(file="output/scales.png", width=1200, height=650, res = 150)
grid.arrange(sexism, resentment, ncol=2)
dev.off()

### Mediation analysis summaries

# Gender

load("output/mediation_gender.rdata")

summary(out.gender)

# Race

load("output/mediation_race.rdata")

summary(out.race)


