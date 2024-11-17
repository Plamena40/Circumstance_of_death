library(REDCapR)
library(DT)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)  
library(plotly) 
library(gtsummary) 
library(circlize)
library(MatchIt) 
library(lmtest)
library(sandwich)
library(emmeans)
library(survival)
library(dplyr)
library(tidyr)
library(MLeval)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(caret)
library(pROC)
library(ROCR)
library(ROSE)
library(xgboost)
library(randomForest)
library(ResourceSelection)
library(PresenceAbsence)
library(ggthemes)
library(gridExtra)
library(sda)
library(gam)
library(flextable, warn.conflicts = FALSE)
library(rms)
library(CalibrationCurves)



df <- read.csv("/Users/ppetrova/Desktop/Organ retrieval/opd.csv")

df$authorized_bin <- ifelse(df$authorized == "True", 1,
                     ifelse(df$authorized == "False", 0, NA))

df$authorized_cat <- ifelse(df$authorized == "True", "Consent to donate",
                     ifelse(df$authorized == "False", "Did not consent", NA))

df$Black <- ifelse(df$Race == "Black / African American", 1, 0)

df$Age <- ifelse(df$Age == 100, NA, df$Age)

df$self_inflicted <- ifelse(df$Circumstances_of_Death == "Suicide" |
                            df$Circumstances_of_Death == "Alleged Suicide", 1, 0)

df$self_inflicted_cat <- ifelse(df$Circumstances_of_Death == "Suicide" |
                            df$Circumstances_of_Death == "Alleged Suicide", "suicide", "not suicide")

df$homicide <- ifelse(df$Circumstances_of_Death == "Homicide" |
                      df$Circumstances_of_Death == "Alleged Homicide", 1, 0)

df$Other_circumstance <- ifelse(df$Circumstances_of_Death == "Accident, Non-MVA" |
                                df$Circumstances_of_Death == "None of the Above", 1, 0)

df$Circumstances_of_Death2 <- ifelse(df$Circumstances_of_Death == "Suicide", "Suicide",
                              ifelse(df$Circumstances_of_Death == "Homicide", "Homicide",
                              ifelse(df$Circumstances_of_Death == "Accident, Non-MVA" |
                                     df$Circumstances_of_Death == "None of the Above", "Other", NA)))

df$TBI_Mechanism <- ifelse(df$Mechanism_of_Death == "Gun Shot Wound" | 
                           df$Mechanism_of_Death == "Gunshot Wound", "Gun shot wound",
                    ifelse(df$Mechanism_of_Death == "Blunt Injury", "Blunt injury", NA))

df$Circumstances_of_Death2 <- relevel(factor(df$Circumstances_of_Death2), "Other")

df$Race <- relevel(factor(df$Race), "White / Caucasian")

df$Gender <- ifelse(df$Gender == "F", "Female",
             ifelse(df$Gender == "M", "Male", NA))

df$days_to_approach <- as.Date(df$time_approached)-as.Date(df$time_brain_death)

df$approach_at_bd <- ifelse(df$days_to_approach == 0, 1,
                     ifelse(df$days_to_approach != 0, 0, NA))

df$days_to_procure <- as.Date(df$time_procured)-as.Date(df$time_brain_death)

df$hours_to_approach <- difftime(ymd_hms(df$time_approached), ymd_hms(df$time_brain_death), units = "hours")

df$Circumstances_of_Death2 <- ifelse(df$Circumstances_of_Death=="Suicide" |
                                                  df$Circumstances_of_Death=="Alleged Suicide", "Suicide",
                                           ifelse(df$Circumstances_of_Death=="Motor Vehicle Accident" |
                                                  df$Circumstances_of_Death=="MVA", "Motor vehicle accident",
                                           ifelse(df$Circumstances_of_Death=="Non-Motor Vehicle Accident"|
                                                  df$Circumstances_of_Death=="Accident, Non-MVA", 
                                                                                    "Non-motor vehicle accident",
                                           ifelse(df$Circumstances_of_Death=="Homicide" |
                                                  df$Circumstances_of_Death=="Alleged Homicide", "Homicide",
                                           ifelse(df$Circumstances_of_Death=="Natural Causes", 
                                                                                    "Natural Causes",
                                           ifelse(df$Circumstances_of_Death=="None of the Above",
                                                  "None of the Above", NA))))))

df$Mechanism <- ifelse(df$Mechanism_of_Death == "ICH/Stroke" |
                         df$Mechanism_of_Death == "Intracranial Hemmorrhage / Stroke",
                         "ICH/Stroke",
                         ifelse(df$Mechanism_of_Death == "Blunt Injury", "Blunt Injury",
                         ifelse(df$Mechanism_of_Death == "Gun Shot Wound" |
                         df$Mechanism_of_Death == "Gunshot Wound",
                         "Gunshot Wound",NA)))

df$authorized_bin2 <- ifelse(df$authorized_bin == 1, "Yes",
                              ifelse(df$authorized_bin == 0, "No", NA))

approached <- df %>% 
                     filter(brain_death == "True") %>%
                     filter(approached == "True") %>%
                     filter(Age >=18) %>% 
                     filter(hours_to_approach <= 168 & hours_to_approach >=0)

approached2 <- approached %>% filter(Circumstances_of_Death2 == "Homicide" |
                       Circumstances_of_Death2 == "Motor vehicle accident" |
                       Circumstances_of_Death2 == "Non-motor vehicle accident" |
                       Circumstances_of_Death2 == "Suicide" | Circumstances_of_Death2 == "Natural Causes") %>% 
                              filter(Cause_of_Death_UNOS == "Head Trauma" |
                            Cause_of_Death_UNOS == "CVA/Stroke" |
                            Cause_of_Death_UNOS == "ICB / ICH" |
                            Cause_of_Death_UNOS == "SAH"
                            ) %>% filter(!is.na(authorized_bin2) & !is.na(hours_to_approach) &
                                         !is.na(Race) & !is.na(Circumstances_of_Death2))


## Table 1
approached2$authorized2 <- ifelse(approached2$authorized == "True", "Authorize",
                          ifelse(approached2$authorized == "False", "Failure to authorize", NA))

approached2$hoursss <- as.numeric(approached2$hours_to_approach)
subset(approached2, select = c(authorized2,
                              Age, Gender, Race, Cause_of_Death_UNOS,
                              Circumstances_of_Death2, hours_to_approach
                              )) %>%
gtsummary::tbl_summary(
                  by = authorized2,
                  percent = "column", 
                  statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                     all_categorical() ~ "{n} ({p}%)"),
                  missing = "no",
                  missing_text = "NA",
                  digits=list(all_continuous() ~ c(0, 1), all_categorical() ~ c(0, 1)),
                  label = list(
                      Circumstances_of_Death2 ~ "Circumstance of death",
                      Cause_of_Death_UNOS ~ "Cause of death",
                      Circumstances_of_Death2 ~ "Circumstance of death",
                      hours_to_approach ~ "Time to approach (hours)"
                  )) %>%
                gtsummary::modify_header(label = "**Characteristics**") %>%
                gtsummary::modify_header(all_stat_cols() ~ "**{level}**, N = {n}") %>%
                gtsummary::bold_labels() %>%
                gtsummary::add_p() %>%
                gtsummary::add_overall() %>%
                gtsummary::as_flex_table() %>% 
                flextable::align(align = "left", part = "all")

## Model building. 
approached2$Circumstances_of_Death2 <- relevel(approached2$Circumstances_of_Death2, ref = "Natural Causes")

glm <- glm(formula = authorized_bin ~ Age + Race + Circumstances_of_Death2 + Mechanism +
    hours_to_approach + Circumstances_of_Death2:hours_to_approach, family = binomial(link = "logit"), 
    data = approached2)

glm_table <- glm %>% 
                gtsummary::tbl_regression(
                  exponentiate = TRUE, 
                  label = list(
                   Circumstances_of_Death2 ~ "Circumstance of death",
                   hours_to_approach ~ "Time to approach (hours)"
                   )) %>%
                gtsummary::bold_labels() %>%
                gtsummary::add_n() %>%
                gtsummary::as_flex_table() %>% 
                flextable::align(align = "left", part = "all")



## Determining proportions
summary <- approached2 %>%
   group_by(Circumstances_of_Death2, Hours_cat) %>%
   summarise(n = n(), mean = round(mean(pred)*100,1),
             sd = sd(pred)*100,
             lower=  round((mean(pred)-1.96*sd(pred)/sqrt(n())),1)*100,
             upper=  round((mean(pred)+1.96*sd(pred)/sqrt(n())),1)*100)

summary$CI <-  paste("(", summary$lower, ", ", summary$upper, ")", sep = "")

summary %>% filter(Circumstances_of_Death2 == "Natural Causes" & Hours_cat == "0") %>% 
  subset(select=c(Circumstances_of_Death2, Hours_cat, n, mean, sd))

summary %>% filter(Circumstances_of_Death2 == "Natural Causes" & Hours_cat == "1-6") %>% 
  subset(select=c(Circumstances_of_Death2, Hours_cat, n, mean, sd))

summary %>% filter(Circumstances_of_Death2 == "Natural Causes" & Hours_cat == "6-12") %>% 
  subset(select=c(Circumstances_of_Death2, Hours_cat, n, mean, sd))

summary %>% filter(Circumstances_of_Death2 == "Natural Causes" & Hours_cat == "12-24") %>% 
  subset(select=c(Circumstances_of_Death2, Hours_cat, n, mean, sd))

summary %>% filter(Circumstances_of_Death2 == "Natural Causes" & Hours_cat == "24+") %>% 
  subset(select=c(Circumstances_of_Death2, Hours_cat, n, mean, sd))

## Determining the different and 95% CI
fun = function(p1, se1, p2, se2) {
  lower<-round((p2-p1)-1.96*sqrt(se1^2+se2^2), 1)
  upper<-round((p2-p1)+1.96*sqrt(se1^2+se2^2),1)
  CI <-  paste("(", lower, ", ", upper, ")", sep = "")
  Diff <- p2-p1
  print(Diff)
  print(CI)
}

fun(p2=61, se2=14.44651, p1=81.9, se1=8.060445)

















