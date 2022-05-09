
#Load packages

library(foreign) 
library(dplyr) 
library(downloader) 
library(srvyr) 
library(stringr) 
library(haven)

#Import data
df_main = read_dta("COVID19_Mexico_13.04.2020_version1.dta")

summary(df_main)
##The length of the df is 38670, so there are 38670 individuals have been tested.

#Rename the columns
df_main = df_main %>% 
  rename('result' = 'resultado')

df_main = df_main %>% 
  rename('gender' = 'sexo', 'age'='edad')
  
#mutate a new column to get the proportion 
df_main = df_main %>% 
  mutate(pos_dummy = as.numeric(result == 1))

df_main %>% 
  summarize(
    pos_mean = mean(pos_dummy))

#The proportion is about 13%.


df_main %>% 
  group_by(gender) %>% 
  summarize(
    gender_pos_mean = mean(pos_dummy))

#The proportion for positive male is 10.5%, while the female is around 15.6%

#Rename the columns
df_main = df_main %>% 
  rename('testing date' = 'FECHA_INGRESO')

df_daily = df_main  %>% 
  group_by(`testing date`) %>% 
  summarize(daily_pos_prop = mean(pos_dummy))

##As we can see in the df_daily, the proportion of testing positive 
##was basically increasing over time(from zero to around fifteen percent)


#create a new df and mutate a new dummy
df_pos = df_main %>% 
  filter(result == 1)

df_pos = df_pos %>% 
  mutate(dead_dummy = as.numeric(FECHA_DEF != '9999-99-99'))

df_pos %>% 
  summarize(dead_porp = mean(dead_dummy))

df_pos %>% 
  group_by(gender) %>% 
  summarize(dead_porp = mean(dead_dummy))

#The proportion of positive passed away is about 6.62%
#For male is about 4.67% and for female is around 8.03%


#Create separate df

reg = df_pos %>% 
  select(dead_dummy, age, gender, neumonia, diabetes, asma, inmusupr, hipertension, OTRA_CON,
         tabaquismo, cardiovascular, obesidad, RENAL_CRONICA)


lm(dead_dummy~age + gender + neumonia + diabetes+ asma+ inmusupr+ hipertension+ OTRA_CON+
   tabaquismo+cardiovascular+obesidad+RENAL_CRONICA, data = reg)

#The regression results can be found as followed:
# Coefficients:
#   (Intercept)             age          gender        neumonia        diabetes  
# 0.2469670       0.0019809       0.0170475      -0.1683386      -0.0001816  
# asma        inmusupr    hipertension        OTRA_CON      tabaquismo  
# -0.0006277       0.0010285      -0.0006139      -0.0020306       0.0019790  
# cardiovascular        obesidad   RENAL_CRONICA  
# 0.0050814      -0.0057807      -0.0012061  


# From the results, we can find that the older the positive testing individual is, 
# the higher mortality he/she is facing; The mortality of male (which is '1' in the df) is also higher than that of female.
# A series of pre-existing diseases such as hypertension and diabetes also 
# have an impact on the mortality of positive testing individual. In general, 
# the less these diseases they have (from '2' to '1' in the df), the lower probability of passing away for them.



  














