
#============================================================================

# EDA - PREDICTING MENTAL HEALTH ILLNESS

#============================================================================
# Packages to be installed 

if (!require("readxl")){ install.packages("readxl")}
if (!require("readr")){ install.packages("(readr")}
if (!require("dplyr")){ install.packages("dplyr")}
if (!require("ggplot2")){ install.packages("ggplot2")}
if (!require("tidyverse")){ install.packages("tidyverse")}
if (!require("lawstat")){ install.packages("lawstat")}
if (!require("moments")){ install.packages("moments")}
if (!require("PHEindicatormethods")){ install.packages("PHEindicatormethods")}
if (!require("writexl")){ install.packages("writexl")}
if (!require("finalfit")){ install.packages("finalfit")}
if (!require("scales")){ install.packages("scales")}
if (!require("summarytools")){ install.packages("summarytools")}
if (!require("broom")){ install.packages("broom")}
if (!require("broom.helpers")){ install.packages("broom.helpers")}
if (!require("rlang")){ install.packages("rlang")}
if (!require("flextable")){ install.packages("flextable")}

# Load packages: Run each line of code to load the library

library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lawstat)
library(moments)
library(PHEindicatormethods)
library("writexl")
library(finalfit)
library(scales)
library(summarytools)
library(DescTools)
library(gmodels)
library(stargazer)
library(broom)
library(gtsummary)
library(dplyr)
library(tidyverse)
library(broom.helpers)
library(rlang)
library(flextable)


# LOAD DATA

depression <- read_csv("../data/raw/depression_data.csv")

#Exploring and understanding the data

head(depression)

str(depression)

sum(is.na(head(depression)))

# Function to clean column names
clean_colnames <- function(df) {
  # Convert column names to lowercase
  colnames(df) <- tolower(colnames(df))
  # Replace spaces with hyphens
  colnames(df) <- gsub(" ", "_", colnames(df))
  
  return(df)
}

# Apply the cleaning function
depression <- clean_colnames(depression)

names(depression)

# check for duplicates based on names

sum(table(depression$name))

# Check for duplicate rows
depression$dup <- duplicated(depression$name)
prop.table(table(depression$dup))


depression <- depression %>%
  group_by(name) %>%
  mutate(duplicate_tag = if_else(n() > 1, "Duplicate", "Unique"))

prop.table(table(depression$duplicate_tag))

# identify patients that has the latest diagnosis regardless of diagnosis
depression <- depression %>%  
  group_by(name) %>% arrange(age) %>%
  mutate(
    Max_Age = max(age),
    Flag = ifelse(age == Max_Age, 1, 0))%>% # This flag the laset diagnosis age whichj should me more recent record of the patient
  mutate(
    Flag2 = case_when(
      Flag == 1 & (history_of_mental_illness == "Yes" | history_of_mental_illness == "No") ~ 1,
      TRUE ~ 0
    ))

# visualizing a subset of the data 
subset <- depression %>% filter(name == "Jacqueline Lewis") %>% arrange(age) %>% select(name,age, Max_Age,Flag, Flag2 ,history_of_mental_illness)

subset1 <- depression %>% filter(name == "Michael Rich") %>% arrange(age) %>% select(name,age, Max_Age,Flag, Flag2 ,history_of_mental_illness) 

subset2 <- depression %>% filter(name == "John Smith") %>% arrange(age) %>% select(name,age, Max_Age,Flag, Flag2 ,history_of_mental_illness)

# retain patients with latest diagnosis age 

analysis_set <- depression %>% filter(Flag2 ==1)

subset2 <- analysis_set %>% filter(name == "John Smith") %>% arrange(age) %>% select(name,age, Max_Age,Flag, Flag2 ,history_of_mental_illness)

# some patients have conflicting diagnosis at the same age, this flags up these patients
analysis_set <- analysis_set %>%
  group_by(name, age) %>%
  mutate(age_flag = if_else(n() > 1, TRUE, FALSE))

table(analysis_set$age_flag)

analysis_set %>% filter(name == "Michael Rich") %>% arrange(age) %>% select(name,age, Max_Age,Flag, Flag2,age_flag ,history_of_mental_illness) 

analysis_set %>% filter(name == "John Smith") %>% arrange(age) %>% select(name,age, Max_Age,Flag, Flag2,age_flag ,history_of_mental_illness) 


# patients cannot have yes/no diagnosis at the same age, drop patients with conflicting diagnosis at the same age

analysis <- analysis_set %>% filter(age_flag == FALSE)

# check for duplicates

analysis <- analysis %>% 
   group_by(name) %>% 
   mutate(dup = n()) %>% 
  ungroup

table(analysis$name)

table(analysis$dup)

# drop other columns

analysis <- analysis %>% select(-c(name,Flag, Flag2, Max_Age, age_flag,dup,duplicate_tag))




# converting number of children as a factor
analysis$number_of_children  <- as.factor(analysis$number_of_children)

# baseline summary data


dependent <-  "history_of_mental_illness"
explanatory <- c("age" ,"income", "marital_status", "education_level","number_of_children","smoking_status","physical_activity_level","employment_status",
                 "alcohol_consumption", "dietary_habits","sleep_patterns","history_of_substance_abuse","family_history_of_depression", "chronic_medical_conditions")
summary_data <- analysis %>% 
  summary_factorlist(dependent, explanatory,
                     add_dependent_label = TRUE)

write_xlsx(summary_data, "../data/processed/summary_data.xlsx")


medianincome <- analysis%>%
  group_by(history_of_mental_illness) %>%
  summarise(
    mean = mean(income),
    median = median(income),
    lower_quartile = quantile(income, 0.25),
    upper_quartile = quantile(income, 0.75),
    IQR = IQR(income)
  )


# Data Mining

# what is the association between patients with history of mental illness and smoking status


table <- table(analysis$smoking_status,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2) 
chisq.test(table(analysis$smoking_status, analysis$history_of_mental_illness))

# counting the number of patients with history of mental illness split by smoking status
numerator0 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(smoking_status) %>%  
  summarise(cases = n(),.groups = 'drop')

# counting the denominator split bt smoking status
denominator0 <- analysis %>%
  group_by(smoking_status) %>%
  summarise(pop = n(),.groups = 'drop')

df <- denominator0 %>% left_join(numerator0,by = c("smoking_status")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) 

df$smoking_status <- as.factor(df$smoking_status) 


fig1 <- ggplot(df, aes(x=smoking_status, y=value, fill = smoking_status))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with Mental Health Illness and Smoking Status"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/smoking.png",fig1,width = 14, height = 8)
fig1



# what is the association between marital_status and history of mental illness

table <- table(analysis$marital_status,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$marital_status, analysis$history_of_mental_illness))


numerator1 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(marital_status) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator1 <- analysis %>%
  group_by(marital_status) %>%  
  summarise(pop = n(),.groups = 'drop')

df2 <- denominator1 %>% left_join(numerator1,by = c("marital_status")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = value/100,2) %>%
  mutate(pct = percent(pct)) 

df2$marital_status <- as.factor(df2$marital_status) 


fig2 <- ggplot(df2, aes(x=marital_status, y=value, fill = marital_status))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with Mental Health Illness and Marital Status"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/marital.status.png",fig2,width = 14, height = 8)
fig2

#====================================================================================

# what is the association between  alcohol_consumption and history of mental illness 

table <- table(analysis$alcohol_consumption,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$alcohol_consumption, analysis$history_of_mental_illness))

numerator2 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(alcohol_consumption) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator2 <- analysis %>%
  group_by(alcohol_consumption) %>%  
  summarise(pop = n(),.groups = 'drop')

df2 <- denominator2 %>% left_join(numerator2,by = c("alcohol_consumption")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) 



fig2 <- ggplot(df2, aes(x=alcohol_consumption, y=value, fill = alcohol_consumption))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with Mental Health Illness and Alcohol consumption"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/alcohol.png",fig2,width = 14, height = 8)
fig2

# =============================================================================

# what is the association between  education_level and history of mental illness 

table <- table(analysis$education_level,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$education_level, analysis$history_of_mental_illness))

numerator3 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(education_level) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator3 <- analysis %>%
  group_by(education_level) %>%  
  summarise(pop = n(),.groups = 'drop')

df3 <- denominator3 %>% left_join(numerator3,by = c("education_level")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) 


fig3 <- ggplot(df3, aes(x=education_level, y=value, fill = education_level))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between Educational Levels and patients with Mental Health Illness"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/education.png",fig3,width = 16, height = 8)
fig3

#==============================================================================


# what is the association between  employment_status and history of mental illness 

table <- table(analysis$employment_status,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$employment_status, analysis$history_of_mental_illness))


numerator4 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(employment_status) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator4 <- analysis %>%
  group_by(employment_status) %>%  
  summarise(pop = n(),.groups = 'drop')

df4 <- denominator4 %>% left_join(numerator4,by = c("employment_status")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) 




fig4 <- ggplot(df4, aes(x=employment_status, y=value, fill = employment_status))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between Levels of Employment and patients with Mental Health Illness"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/employment.png",fig4,width = 16, height = 8)
fig4

#=================================================================================
# what is the association between  "history_of_substance_abuse and history of mental illness 

table <- table(analysis$history_of_substance_abuse,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$history_of_substance_abuse, analysis$history_of_mental_illness))

# There is no association between history_of_substance_abuse and mental health ilness
#==============================================================================

# what is the association between  family_history_of_depression and history of mental illness 

table <- table(analysis$family_history_of_depression,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$family_history_of_depression, analysis$history_of_mental_illness))


numerator5 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(family_history_of_depression) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator5 <- analysis %>%
  group_by(family_history_of_depression) %>%  
  summarise(pop = n(),.groups = 'drop')

df5 <- denominator5 %>% left_join(numerator5,by = c("family_history_of_depression")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) %>% mutate(family_history_of_depression = case_when(
                family_history_of_depression == "Yes" ~ "Depression",
              family_history_of_depression == "No" ~ "Not Depressed"))



fig5 <- ggplot(df5, aes(x=family_history_of_depression, y=value, fill = family_history_of_depression))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with a history of depression and Mental Health Illness"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/depression.png",fig5,width = 16, height = 8)
fig5

# what is the association between  chronic_medical_conditions and history of mental illness 

table <- table(analysis$chronic_medical_conditions,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$chronic_medical_conditions, analysis$history_of_mental_illness))


numerator6 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(chronic_medical_conditions) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator6 <- analysis %>%
  group_by(chronic_medical_conditions) %>%  
  summarise(pop = n(),.groups = 'drop')

df6 <- denominator6 %>% left_join(numerator6,by = c("chronic_medical_conditions")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100) %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) %>%
   mutate(chronic_medical_conditions = case_when(
    chronic_medical_conditions == "Yes" ~ "Present",
    chronic_medical_conditions == "No" ~ "Absent")) 




fig6 <- ggplot(df6, aes(x=chronic_medical_conditions, y=value, fill = chronic_medical_conditions))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with chronic medical conditions and Mental Health Illness"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/chronic.png",fig6,width = 16, height = 8)
fig6

# what is the association between  sleep_patterns and history of mental illness 

table <- table(analysis$sleep_patterns,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$sleep_patterns, analysis$history_of_mental_illness))


numerator7 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(sleep_patterns) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator7 <- analysis %>%
  group_by(sleep_patterns) %>%  
  summarise(pop = n(),.groups = 'drop')

df7 <- denominator7 %>% left_join(numerator7,by = c("sleep_patterns")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) 




fig7 <- ggplot(df7, aes(x=sleep_patterns, y=value, fill = sleep_patterns))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with varying sleep patterns and Mental Health Illness"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/sleep.png",fig7,width = 14, height = 8)
fig7


# what is the association between  physical_activity_level and history of mental illness 

table <- table(analysis$physical_activity_level,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$physical_activity_level, analysis$history_of_mental_illness))

numerator8 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(physical_activity_level) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator8 <- analysis %>%
  group_by(physical_activity_level) %>%  
  summarise(pop = n(),.groups = 'drop')

df8 <- denominator8 %>% left_join(numerator8,by = c("physical_activity_level")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) 




fig8 <- ggplot(df8, aes(x=physical_activity_level, y=value, fill = physical_activity_level))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with varying levels of physiical activity and Mental Health Illness"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/p.activity.png",fig8,width = 16, height = 8)
fig8


# what is the association between  dietary_habits and history of mental illness 

table <- table(analysis$dietary_habits,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$dietary_habits, analysis$history_of_mental_illness))

numerator9 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(dietary_habits) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator9 <- analysis %>%
  group_by(dietary_habits) %>%  
  summarise(pop = n(),.groups = 'drop')

df9 <- denominator9 %>% left_join(numerator9,by = c("dietary_habits")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) 




fig9 <- ggplot(df9, aes(x=dietary_habits, y=value, fill = dietary_habits))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with varying diatery habits and Mental Health Illness"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/dietary.png",fig9,width = 16, height = 8)
fig9

# what is the association between  number_of_children and history of mental illness 

table <- table(analysis$number_of_children,analysis$history_of_mental_illness)
table2 <- round(prop.table((table), margin=1) *100,2)
table2
chisq.test(table(analysis$number_of_children, analysis$history_of_mental_illness))

numerator10 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(number_of_children) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator10 <- analysis %>%
  group_by(number_of_children) %>%  
  summarise(pop = n(),.groups = 'drop')

df10 <- denominator10 %>% left_join(numerator10,by = c("number_of_children")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct))  %>%
  mutate(number_of_children = case_when(
    number_of_children == 0 ~ "None",
    number_of_children == 1 ~ "One",
    number_of_children == 2 ~ "Two",
    number_of_children == 3 ~ "Three",
    number_of_children == 4 ~ "Four")) 


fig10 <- ggplot(df10, aes(x=number_of_children, y=value, fill = number_of_children))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with different number of children and Mental Health Illness"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/children.png",fig10,width = 16, height = 8)
fig10

# # what is the association between  age and history of mental illness 


fit1 <- glm(as.factor(history_of_mental_illness) ~ age, data=analysis, family=binomial)
summary(fit1)
coef(fit1) %>% exp()
confint(fit1) %>% exp()

hist(analysis$age)
table((analysis$age))

median(analysis$age)

# categorizing age based on median age

analysis <- analysis %>% 
  mutate(agecat = case_when( 
    age  <= 57  ~ "Age <=57",
    age   >-58 ~ "Age >=58"))


fit1 <- glm(as.factor(history_of_mental_illness) ~ agecat, data=analysis, family=binomial)
summary(fit1)
coef(fit1) %>% exp()
confint(fit1) %>% exp()


numerator11 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(agecat) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator11 <- analysis %>%
  group_by(agecat) %>%  
  summarise(pop = n(),.groups = 'drop')

df11 <- denominator11 %>% left_join(numerator11,by = c("agecat")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) 


fig11 <- ggplot(df11, aes(x=agecat, y=value, fill = agecat))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with Mental Health Illness and Age Category"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/agecat.png",fig11,width = 14, height = 8)
fig11

# # what is the association between  income and history of mental illness 


fit1 <- glm(as.factor(history_of_mental_illness) ~ income, data=analysis, family=binomial)
summary(fit1)
coef(fit1) %>% exp()
confint(fit1) %>% exp()

hist(analysis$income)


median(analysis$income)

# categorical income based on median income

analysis <- analysis %>% 
  mutate(incomecat = case_when( 
    income  <= 35880  ~ "Income <=Median",
    income   > 35880 ~ "Income > Median"))


fit1 <- glm(as.factor(history_of_mental_illness) ~ incomecat, data=analysis, family=binomial)
summary(fit1)
coef(fit1) %>% exp()
confint(fit1) %>% exp()


numerator12 <- analysis %>%
  filter(history_of_mental_illness == "Yes") %>%
  group_by(incomecat) %>%  
  summarise(cases = n(),.groups = 'drop')

denominator12 <- analysis %>%
  group_by(incomecat) %>%  
  summarise(pop = n(),.groups = 'drop')

df12 <- denominator12 %>% left_join(numerator12,by = c("incomecat")) %>% 
  phe_proportion(cases, pop, confidence = 0.95, multiplier = 100)  %>%
  mutate(pct = round(value/100,2)) %>%
  mutate(pct = percent(pct)) 


fig12 <- ggplot(df12, aes(x=incomecat, y=value, fill = incomecat))+
  geom_bar(stat = "identity",width = 0.5,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Association between patients with Mental Health Illness and Income Level"),
       subtitle = '',
       x = "", 
       y = "% Population",
       caption ="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=20),
        plot.subtitle = element_text(size = 30)) + theme(legend.position='none') +
  geom_text(aes(label = pct),vjust = 0.5,hjust= 3, size = 8,position = position_dodge(.5)) +   coord_flip() 
ggsave("../reports/figures/incomecat.png",fig12,width = 14, height = 8)
fig12


# adjusted model

#=========================================================
 # model selection
 
 fit <- glm(as.factor(history_of_mental_illness) ~ incomecat + as.factor(education_level) + as.factor(employment_status) + as.factor(sleep_patterns)  +
              as.factor(number_of_children) + as.factor(agecat),  data=analysis, family=binomial)
 model_summary <-fit %>% tidy(conf.int = TRUE, exp = TRUE)
 
 
 fit1 <- glm(as.factor(history_of_mental_illness) ~ incomecat + as.factor(education_level) + as.factor(employment_status) + as.factor(sleep_patterns)  +
              as.factor(agecat),  data=analysis, family=binomial)
 model_summary <-fit1 %>% tidy(conf.int = TRUE, exp = TRUE)
 
 fit2 <- glm(as.factor(history_of_mental_illness) ~ incomecat + as.factor(education_level) + as.factor(employment_status) + as.factor(sleep_patterns),  data=analysis, family=binomial)
 model_summary <-fit2 %>% tidy(conf.int = TRUE, exp = TRUE)
 
 
 fit2 <- glm(as.factor(history_of_mental_illness) ~ incomecat + as.factor(education_level) + as.factor(employment_status),  data=analysis, family=binomial)
 model_summary <-fit2 %>% tidy(conf.int = TRUE, exp = TRUE)
 
 # regroup smoking status
 
 analysis <-analysis %>% 
   mutate(smoking2cat = case_when(
     smoking_status == "Current" ~ "Current",
     smoking_status == "Former" | smoking_status == "Non-smoker"  ~ "Former/Non-Smoker"
   )) %>% 
   mutate(alcohol.cat = case_when(
     alcohol_consumption == "High" ~ "High",
     alcohol_consumption == "Low" |  alcohol_consumption == "Moderate"~ "Low/Moderate"))
 
 
 fit3 <- glm(as.factor(history_of_mental_illness) ~ incomecat + as.factor(education_level) + as.factor(employment_status) + as.factor(smoking2cat) + as.factor(alcohol.cat),  data=analysis, family=binomial)
 model_summary <-fit3 %>% tidy(conf.int = TRUE, exp = TRUE)
 
 
 
 fitted <- glm(as.factor(history_of_mental_illness) ~ incomecat + as.factor(education_level) + as.factor(employment_status),  data=analysis, family=binomial)
 model_summary <-fitted %>% tidy(conf.int = TRUE, exp = TRUE)
 
 write_xlsx(model_summary ,"../data/processed/lr.model.xlsx")
 
 
 # preparing data for ML
 
 #converting variables to numeric
 
 analysis_df <-analysis %>% 
   mutate(smoking_status = case_when(
     smoking_status == "Current" ~ 1,
     smoking_status == "Former" ~ 2,
     smoking_status == "Non-smoker" ~ 3,
     TRUE ~ 4
   )) %>%  
   # ethnicity
   mutate(marital_status = case_when(
     marital_status == "Divorced" ~ 4,
     marital_status == "Single" ~ 1,
     marital_status == "Married" ~ 2,
     marital_status == "Widowed" ~ 3,
     TRUE ~ 5)) %>% 
   mutate(education_level = case_when(
     education_level== "High School" ~ 1,
     education_level == "Associate Degree" ~ 2,
     education_level == "Bachelor's Degree"~ 3,
     education_level == "Master's Degree"~ 4,
     education_level == "PhD"~ 5,
     TRUE ~ 5))%>%
   mutate(physical_activity_level = case_when(
     physical_activity_level == "Active" ~ 1,
     physical_activity_level == "Moderate"  ~ 2,
     physical_activity_level== "Sedentary"  ~ 3,
     TRUE ~ 4
   ))%>% 
   mutate(alcohol_consumption = case_when(
     alcohol_consumption == "High" ~ 3,
     alcohol_consumption == "Low" ~ 2,
     alcohol_consumption == "Moderate" ~ 1,
     TRUE ~ 4
   ))%>%
   mutate(employment_status = case_when( 
     employment_status  == "Employed"  ~ 1,
     employment_status  == "Unemployed" ~ 0)) %>%
   mutate(history_of_mental_illness = case_when( 
     history_of_mental_illness  == "Yes"  ~ 1,
     history_of_mental_illness  == "No" ~ 0)) %>%
   mutate(family_history_of_depression = case_when( 
     family_history_of_depression  == "Yes"  ~ 1,
     family_history_of_depression  == "No" ~ 0)) %>%
   mutate(dietary_habits = case_when( 
     dietary_habits  == "Healthy"  ~ 2,
     dietary_habits  == "Moderate" ~ 1,
     dietary_habits  == "Unhealthy" ~ 0)) %>%
   mutate(chronic_medical_conditions = case_when( 
     chronic_medical_conditions  == "No"  ~ 0,
     chronic_medical_conditions  == "Yes" ~ 1))%>%
   mutate(history_of_substance_abuse = case_when( 
     history_of_substance_abuse  == "No"  ~ 0,
     history_of_substance_abuse  == "Yes" ~ 1)) %>%
   mutate(sleep_patterns = case_when( 
     sleep_patterns  == "Fair"  ~ 1,
     sleep_patterns  == "Good" ~ 2,
     sleep_patterns  == "Poor" ~ 3))
 
 write.csv(analysis_df ,"../data/processed/analysis_data_set.csv")
 

 #=================================================

# load data for odds ratio
 df<- read_excel("../data/processed/lr.OR.xlsx")
 
 
 ################################################################################
 # Add Significance
 df <- df %>% mutate(sig = case_when(LC>1 ~ "Higher",
                                     UC<1 ~ "Lower",
                                     TRUE ~ "No Difference"))

 
 plot <-  ggplot(df, aes(y = Level, x = OR, colour=sig))+
   geom_point(aes(colour= sig),shape = 18, size = 2)+  
   scale_colour_manual(values = c("Higher" = "red", "Lower" = "green","No Difference" = "black"))+
   ggstance::geom_pointrangeh(aes(xmin = LC, xmax = UC))+
   geom_vline(xintercept = 1, color = "blue", linetype = "dashed", cex = 1, alpha = 0.5) +
   labs(x= "Odds Ratio",
        title = paste0("Association between patients with history of mental health illness and selected factors: \nOdds Ratios"),
        subtitle = "Logistic Regression Analysis",
        y = '') +
   # xlim(0,12) +
   geom_text(aes(label = paste0(format(round(as.numeric(OR),2),nsmall=0),"")), vjust = 0, colour = "black",
             nudge_y = 0.1)+
   theme_bw()+
   theme(axis.text.y =  element_text(size = 20),
         axis.title.y = element_text(size = 20),
         axis.text.x =  element_text(size = 20),
         axis.title.x = element_text(size = 20),
         title=element_text(size=22))+
   theme(legend.position = "bottom",
         legend.title=element_blank())+
   facet_grid(Factors ~.,scales="free",space="free") 
 
 ggsave("../reports/PlotOR.png",plot,width = 17, height = 8)
 
 