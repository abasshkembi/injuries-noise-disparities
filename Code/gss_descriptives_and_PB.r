library(tidyverse)
library(gssr)
library(lme4)
library(survey)
library(lmerTest)
library(srvyr)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

#### read in GSS data
data("gss_all")

#keep only years 02, 06, 10, 14, 18
gssyears <- subset(gss_all, subset=c(year == 2002 | year == 2006 | year == 2010 | year == 2014 | year == 2018))

#keep only if answered Q1 about work type
gssQ1 <- subset(gssyears, subset=c(wrktype == 1 | wrktype == 2 | wrktype == 3 | wrktype ==4 | wrktype==5))

#keep those over 18 
gss18 <- subset(gssQ1, subset = c (age >= 18))

# keep the variables we care about 
gssfinal <- gss18 %>% 
  select(id, year, wtss, wtssnr, wtssall, vstrat, vpsu,
         occ10, age, degree, sex, race, racecen1,
         hurtatwk) %>%
  na.omit() # we are going to keep complete cases - remove all rows with missing data

# read in isco-08 to soc 2010 soc code
crosswalk <- readxl::read_xlsx("Data/2018-occupation-code-list-and-crosswalk.xlsx", sheet = 4, skip = 3)
crosswalk <- crosswalk[1:3] %>% na.omit()
colnames(crosswalk) <- c("soc10", "occ10", "occ_title")

# crosswalk occupational codes
gssfinal2 <- gssfinal %>%
  arrange(occ10) %>%
  mutate(occ10 = ifelse(str_length(occ10) == 2, paste0("00", occ10), occ10)) %>%
  mutate(occ10 = ifelse(str_length(occ10) == 3, paste0("0", occ10), occ10)) %>% 
  left_join(crosswalk, by = "occ10") %>%
  mutate(major_soc = str_extract(soc10, "^\\d\\d")) %>%
  filter(!is.na(major_soc)) %>% # drop 27 military and 7 undefined occupational codes
  filter(major_soc != "55")

# clean up injury and race data
gssfinal3 <- gssfinal2 %>%
  mutate(inj_ever = ifelse(hurtatwk > 0, 1, 0),
         inj_more_once = ifelse(hurtatwk > 1, 1, 0)) %>%
  mutate(race_ethnicity = 
           case_when(
             racecen1 == 1 ~ "awhite",
             racecen1 == 2 ~ "bblack",
             racecen1 >= 3 & racecen1 <= 14 ~ "casian_pacific",
             racecen1 == 15 ~ "dmulti",
             racecen1 == 16 ~ "ehispanic"
           )) %>%
  mutate(race = as.factor(race))

# create a crosswalk for soc codes to large occupational categories
major_soc_crosswalk <- tibble(
  major_soc = as.character(seq(11, 53, by = 2)),
  occ = c(rep("mbsa", 11),
          rep("service", 4),
          rep("sales", 2),
          rep("nrc", 3),
          rep("pt", 2))
) %>%
  mutate(group = factor(occ, 
                        levels = c("mbsa", "service", "sales", "nrc", "pt"),
                        labels = c("Management, business, science, and arts",
                                   "Service",
                                   "Sales &\noffice",
                                   "Natural\nresources &\nconstruction",
                                   "Production &\ntransportation")
  ))

# crosswalk data
gssfinal4 <- gssfinal3 %>%
  left_join(major_soc_crosswalk, by = c("major_soc")) %>%
  mutate(lessHS = ifelse(degree == 0, 1, 0)) %>%
  filter(race_ethnicity %in% c("awhite", "bblack", "ehispanic")) %>%
  mutate(raceethn_lessHS = interaction(race_ethnicity, lessHS),
         race_lessHS = interaction(race, lessHS))

# read onet data on noise
onet_broad <- readRDS("Data/process_onet_noise_2019.rds") %>%
  mutate(broad_soc = str_replace(soc_2010_code, "\\d$", "0")) %>%
  group_by(broad_soc) %>%
  summarise(Texposed_hrs = mean(Texposed_hrs))

# merge in onet noise data to gss
gssfinal5 <- gssfinal4 %>%
  mutate(broad_soc = str_replace(soc10, "(\\d|\\X)$", "0")) %>%
  left_join(onet_broad, by = c("broad_soc")) %>% 
  na.omit()

# assign weighting for complex survey design
#### all participants
gssdsn<-svydesign(id=~vpsu, strata=~vstrat, weights=~wtssall, data=gssfinal5, nest=T)
#### nh white
gssdsn_white<-svydesign(id=~vpsu, strata=~vstrat, weights=~wtssall, data=gssfinal5 %>% filter(race_ethnicity == "awhite"), nest=T)
#### nh black
gssdsn_black<-svydesign(id=~vpsu, strata=~vstrat, weights=~wtssall, data=gssfinal5 %>% filter(race_ethnicity == "bblack"), nest=T)
#### hispanic
gssdsn_hispanic<-svydesign(id=~vpsu, strata=~vstrat, weights=~wtssall, data=gssfinal5 %>% filter(race_ethnicity == "ehispanic"), nest=T)





### descriptive statistics ----- Table 1

nrow(gssfinal5) # number of participants

gssfinal5 %>% count(race_ethnicity, occ) # no. participants by race

# overall summary statistics
gssfinal5 %>%
  mutate(sex = ifelse(sex == 1, 1, 0),
         age = ifelse(age < 35, 1, 0)) %>%
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  summarise(inj = survey_mean(hurtatwk),
            noise = survey_mean(Texposed_hrs)/24,
            educ = survey_mean(lessHS),
            age = survey_mean(age),
            sex = survey_mean(sex))

# by occupational categories
gssfinal5 %>%
  mutate(sex = ifelse(sex == 1, 1, 0),
         age = ifelse(age < 35, 1, 0)) %>%
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(occ) %>%
  summarise(inj = survey_mean(hurtatwk),
            noise = survey_mean(Texposed_hrs)/24)

# by race
gssfinal5 %>%
  mutate(sex = ifelse(sex == 1, 1, 0),
         age = ifelse(age < 35, 1, 0)) %>%
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(race_ethnicity) %>%
  summarise(inj = survey_mean(hurtatwk),
            noise = survey_mean(Texposed_hrs)/24,
            educ = survey_mean(lessHS),
            age = survey_mean(age),
            sex = survey_mean(sex))

# by race and occupational categories
gssfinal5 %>%
  mutate(sex = ifelse(sex == 1, 1, 0),
         age = ifelse(age < 35, 1, 0)) %>%
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(race_ethnicity, occ) %>%
  summarise(inj = survey_mean(hurtatwk),
            noise = survey_mean(Texposed_hrs)/24)



### peters-belson approach

# no noise, but major SOC and sociodemographic characteristics

model_white_noNoise <- svyglm(hurtatwk ~ as.factor(major_soc) +
                                as.factor(year) +
                                as.factor(lessHS) +
                                age + as.factor(sex), 
                              family = "poisson", 
                              design = gssdsn_white)
summary(model_white_noNoise)

## extract RRs -- Table 2
summary(model_white_noNoise)$coef %>%
  as_tibble() %>% .[c(1, 2)] %>%
  mutate(variable = rownames(summary(model_white_noNoise)$coef)) %>%
  mutate(rr = round(exp(Estimate), 2),
         ll = round(exp(Estimate - 1.96*`Std. Error`), 2),
         ul = round(exp(Estimate + 1.96*`Std. Error`), 2)) %>%
  print(n = nrow(.))

# model with noise

model_white <- svyglm(hurtatwk ~ as.factor(major_soc) +
                        Texposed_hrs +
                        as.factor(year) +
                        as.factor(lessHS) +
                        age + as.factor(sex), 
                      family = "poisson", 
                      design = gssdsn_white)
summary(model_white)

## extract RRs -- Table 2
summary(model_white)$coef %>%
  as_tibble() %>% .[c(1, 2)] %>%
  mutate(variable = rownames(summary(model_white)$coef)) %>%
  mutate(rr = round(exp(Estimate), 2),
         ll = round(exp(Estimate - 1.96*`Std. Error`), 2),
         ul = round(exp(Estimate + 1.96*`Std. Error`), 2)) %>%
  print(n = nrow(.))

## RR for noise per 10 day increase
round(exp(240*0.000334), 2)
round(exp(240*(0.000334 + c(-1.96, 1.96)*0.000187)), 2)


obs_white <- gssfinal5 %>%
  filter(race_ethnicity == "awhite") %>% 
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(occ) %>%
  summarise(mean = survey_mean(hurtatwk))


expected_white_noNoise <- gssfinal5 %>% 
  filter(race_ethnicity == "awhite") %>% 
  mutate(pred_hurtatwk = predict(model_white_noNoise, newdata = ., type = "response")) %>%
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(occ) %>%
  summarise(mean = survey_mean(pred_hurtatwk))

expected_white <- gssfinal5 %>% 
  filter(race_ethnicity == "awhite") %>% 
  mutate(pred_hurtatwk = predict(model_white, newdata = ., type = "response")) %>%
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(occ) %>%
  summarise(mean = survey_mean(pred_hurtatwk))


## black

obs_black <- gssfinal5 %>%
  filter(race_ethnicity == "bblack") %>% 
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(occ) %>%
  summarise(mean = survey_mean(hurtatwk))

expected_black_noNoise <- gssfinal5 %>% 
  filter(race_ethnicity == "bblack") %>% 
  mutate(pred_hurtatwk = predict(model_white_noNoise, newdata = ., type = "response")) %>%
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(occ) %>%
  summarise(mean = survey_mean(pred_hurtatwk))

expected_black <- gssfinal5 %>% 
  filter(race_ethnicity == "bblack") %>% 
  mutate(pred_hurtatwk = predict(model_white, newdata = ., type = "response")) %>%
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(occ) %>%
  summarise(mean = survey_mean(pred_hurtatwk))


## hispanic

obs_hispanic <- gssfinal5 %>% 
  filter(race_ethnicity == "ehispanic") %>% 
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(occ) %>%
  summarise(mean = survey_mean(hurtatwk))

expected_hispanic_noNoise <- gssfinal5 %>% 
  filter(race_ethnicity == "ehispanic") %>% 
  mutate(pred_hurtatwk = predict(model_white_noNoise, newdata = ., type = "response")) %>%
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(occ) %>%
  summarise(mean = survey_mean(pred_hurtatwk))

expected_hispanic <- gssfinal5 %>% 
  filter(race_ethnicity == "ehispanic") %>% 
  mutate(pred_hurtatwk = predict(model_white, newdata = ., type = "response")) %>%
  as_survey_design(ids = vpsu, strata = vstrat, weights = wtssall, nest = T) %>%
  group_by(occ) %>%
  summarise(mean = survey_mean(pred_hurtatwk))


#### explained disparity -- Table 3

#### observed conditions
######### expected under observed conditions, no noise
expected_black_noNoise %>% ## nh black
  mutate(race = "black",
         prediction = "expected_noNoise") %>%
  rbind(
    expected_hispanic_noNoise %>% ## hispanic
      mutate(race = "hispanic",
             prediction = "expected_noNoise")
  ) %>%
  ######### expected under observed conditions, with noise
  rbind(
    expected_black %>%
      mutate(race = "black", ## nh black
             prediction = "expected_wNoise") %>%
      rbind(
        expected_hispanic %>%
          mutate(race = "hispanic", ## hispanic
                 prediction = "expected_wNoise")
      )
  ) %>%
  ######### observed injury rates of nh white
  left_join(
    obs_white %>%
      rename(obs_white = mean, obs_white_se = mean_se),
    by = "occ"
  ) %>%
  left_join(
    ######### observed injury rates of nh black
    obs_black %>%
      mutate(race = "black") %>%
      ######### observed injury rates of hispanic
      rbind(
        obs_hispanic %>%
          mutate(race = "hispanic")
      ) %>%
      rename(obs_minor = mean, obs_minor_se = mean_se),
    by = c("occ", "race")
  ) %>%
  ### calculate statistics
  mutate(obs_disparity = round(obs_minor - obs_white, 3),
         explained_disparity = round(mean - obs_white, 3)) %>%
  mutate(percent_explained = explained_disparity/obs_disparity) %>%
  arrange(-obs_disparity)





