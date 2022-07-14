##### 0. PACKAGES #####
library(tidyverse)

##### 1. IMPORT DATA #####
local_path <- "C:\\Users\\danie\\Documents\\Masters\\3. THESIS\\Analysis"
#Source: <https://survey.ucd.ie/crimeattitudes/index.php/635612?lang=en> as at July 2022
surveyRaw <- readxl::read_excel(paste0(local_path,
                              "\\results-survey635612.xlsx"))

##### 2. EXCLUDE RESPONSES #####
surveyEligible <- surveyRaw[surveyRaw$CONSENT=="Yes" & surveyRaw$AGE>=18,] #remove ineligible responses (429-25=404)
surveyComplete <- surveyEligible %>% drop_na(submitdate) #remove incomplete surveys (404-21=283)
surveySubmit <- surveyComplete[surveyComplete$SUBMIT=="Yes",] #remove non-submitted surveys (283-1=282)

##### 3. RECODE DEMOGRAPHIC VARIABLES #####
survey <- surveySubmit %>%
  #EDUATION
  mutate(EDUCATION_levels=case_when(
    EDUCATION=="Primary" ~ 0,
    EDUCATION=="Lower Secondary" ~ 1,
    EDUCATION=="Upper Secondary" ~ 2,
    EDUCATION=="Apprenticeship" ~ 3,       
    EDUCATION=="Undergraduate" ~ 4,
    EDUCATION=="Postgraduate" ~ 5, 
    )
  ) %>%
  #NATIONALITY
  rename(NATIONALITY_PRIMARY=NATIONALITY1) %>%
  rename(NATIONALITY_SECONDARY=NATIONALITY2) %>%
  mutate_at(vars(NATIONALITY_SECONDARY),
    ~na_if(.,"Not applicable")
  ) %>%
  #POLITICS
  rename(POLITICS=`POLITICS[POL]`) %>%
  mutate(POLITICS_levels=case_when(
    POLITICS=="Liberal" ~ 0,
    POLITICS=="Centre-Left" ~ 1,
    POLITICS=="Centrist" ~ 2,
    POLITICS=="Centre-Right" ~ 3,
    POLITICS=="Conservative" ~ 4
    )
  ) %>%
  #RACE
  mutate(RACE=case_when(
    `RACE[RACE3]`=="Yes" ~ "White",
    `RACE[RACE2]`=="Yes" ~ "Black",
    `RACE[RACE1]`=="Yes" ~ "Asian",
    (`RACE[RACE1]`=="Yes" & `RACE[RACE2]`=="Yes") |
    (`RACE[RACE1]`=="Yes" & `RACE[RACE3]`=="Yes") |
    (`RACE[RACE2]`=="Yes" & `RACE[RACE3]`=="Yes") ~ "Mixed",
    `RACE[RACE1]`=="No" & `RACE[RACE2]`=="No" & `RACE[RACE3]`=="No" ~ "Other"
    )
  ) %>%
  #CRIMINAL JUSTICE SYSTEM EXPERIENCE
  rename(CRIME_FEAR=`CJSEXPERIENCE[EXP1]`) %>% mutate_at(vars(CRIME_FEAR), ~ifelse(CRIME_FEAR=="Yes", 1, 0)) %>%
  rename(CRIME_WITNESS=`CJSEXPERIENCE[EXP2]`) %>% mutate_at(vars(CRIME_WITNESS), ~ifelse(CRIME_WITNESS=="Yes", 1, 0)) %>%
  rename(CRIME_VICTIM=`CJSEXPERIENCE[EXP3]`) %>% mutate_at(vars(CRIME_VICTIM), ~ifelse(CRIME_VICTIM=="Yes", 1, 0)) %>%
  rename(CRIME_CONVICT=`CJSEXPERIENCE[EXP4]`) %>% mutate_at(vars(CRIME_CONVICT), ~ifelse(CRIME_CONVICT=="Yes", 1, 0)) %>%
  rename(CRIME_JURY=`CJSEXPERIENCE[EXP5]`) %>% mutate_at(vars(CRIME_JURY), ~ifelse(CRIME_JURY=="Yes", 1, 0)) %>%
  rename(CRIME_WORK=`CJSEXPERIENCE[EXP6]`) %>% mutate_at(vars(CRIME_WORK), ~ifelse(CRIME_WORK=="Yes", 1, 0)) %>%
  rowwise() %>% mutate(CRIME_EXPERIENCES_SUM=sum(c_across(CRIME_FEAR:CRIME_WORK))) %>% ungroup() %>%
  # CRIMINAL JUSTICE SYSTEM PURPOSE
  rename(CJS_PURPOSE_FIRST=`CJSPURPOSE[1]`, CJS_PURPOSE_SECOND=`CJSPURPOSE[2]`, CJS_PURPOSE_THIRD=`CJSPURPOSE[3]`, CJS_PURPOSE_FOURTH=`CJSPURPOSE[4]`) %>%
  mutate(CJS_PURPOSE_REHABILITATION_rank=case_when(
    CJS_PURPOSE_FIRST=="Rehabilitation" ~ 1, CJS_PURPOSE_SECOND=="Rehabilitation" ~ 2, CJS_PURPOSE_THIRD=="Rehabilitation" ~ 3, CJS_PURPOSE_FOURTH=="Rehabilitation" ~ 4)) %>%
  mutate(CJS_PURPOSE_PROTECTION_rank=case_when(
    CJS_PURPOSE_FIRST=="Protecting the Public" ~ 1, CJS_PURPOSE_SECOND=="Protecting the Public" ~ 2, CJS_PURPOSE_THIRD=="Protecting the Public" ~ 3, CJS_PURPOSE_FOURTH=="Protecting the Public" ~ 4)) %>%
  mutate(CJS_PURPOSE_DETERRENCE_rank=case_when(
    CJS_PURPOSE_FIRST=="Deterrence of Crime" ~ 1, CJS_PURPOSE_SECOND=="Deterrence of Crime" ~ 2, CJS_PURPOSE_THIRD=="Deterrence of Crime" ~ 3, CJS_PURPOSE_FOURTH=="Deterrence of Crime" ~ 4)) %>%
  mutate(CJS_PURPOSE_PUNISHMENT_rank=case_when(
    CJS_PURPOSE_FIRST=="Punishment" ~ 1, CJS_PURPOSE_SECOND=="Punishment" ~ 2, CJS_PURPOSE_THIRD=="Punishment" ~ 3, CJS_PURPOSE_FOURTH=="Punishment" ~ 4)) %>%
  #MEDIA
  rename(MEDIA_GENERAL=`MEDIA[MEDIA1]`) %>%
  rename(MEDIA_CRIME=`MEDIA[MEDIA2]`) %>%
  rowwise() %>% mutate(MEDIA_SUM=sum(c_across(MEDIA_GENERAL:MEDIA_CRIME))) %>% ungroup() %>%
  #CURRENCY: <https://data.worldbank.org/indicator/PA.NUS.PPP>, 2021 data.
  mutate(HOUSEHOLD_MONTHLY_INCOME_USD_PPP=case_when(
    grepl("AUD", CURRENCY) ~ INCOME/1.4389790,
    grepl("CAD", CURRENCY) ~ INCOME/1.2530660,
    grepl("GBP", CURRENCY) ~ INCOME/0.6928020,
    grepl("INR", CURRENCY) ~ INCOME/23.1381376,
    grepl("SEK", CURRENCY) ~ INCOME/8.7088530,
    grepl("SGD", CURRENCY) ~ INCOME/0.8395717,
    grepl("TRY", CURRENCY) ~ INCOME/2.7818510,
    grepl("ZAR", CURRENCY) ~ INCOME/7.1680967,
    grepl("EUR", CURRENCY) ~ INCOME/0.667415 #<https://stats.oecd.org/viewhtml.aspx?datasetcode=SNA_TABLE4&lang=en#>, 2021 data.
    )
  ) %>%
  #RENAME MISC. VARIABLES
  rename(HOUSEHOLD_SIZE=SIZE, HOUSEHOLD_MONTHLY_INCOME=INCOME) %>%
  #'OTHER' RESPONSES
  mutate(EDUCATION_levels=
    ifelse(grepl("chartered|profession|master|accountant",
      `EDUCATION[other]`, ignore.case=TRUE), 5,
    ifelse(grepl("dip|college|bachelor|plc", 
      `EDUCATION[other]`, ignore.case=TRUE), 4,
    EDUCATION_levels
    ))
  ) %>%
  mutate(RELIGION=
    ifelse(grepl("catholic",
      `RELIGION[other]`, ignore.case=TRUE), "Christian",
    RELIGION
    )
  ) %>%
  mutate(RACE=case_when(
    !is.na(`RACE[other]`) ~ "Mixed",
    TRUE ~ RACE
    )
  ) %>%
  mutate(NATIONALITY_SECONDARY=case_when(
    !is.na(`NATIONALITY2[other]`) ~ `NATIONALITY2[other]`,
    TRUE ~ NATIONALITY_SECONDARY
    )
  )
    
##### 4. RECODE CRIME VARIABLES #####
# SEVERITY FUNCTION
crime_severity_likert <- function(df, old_column, new_column) {
  df %>% 
    mutate("{{new_column}}":=case_when(
      old_column=="Not At All Severe" ~ 1,
      old_column=="Somewhat Severe" ~ 2,
      old_column=="Moderately Severe" ~ 3,
      old_column=="Very Severe" ~ 4,
      old_column=="Extremely Severe" ~ 5
    )
  )
}
#SEVERITY CONVERSION
survey <- survey %>%
  rename(SEVERITY_DRUGTRAFFIC=`DRUGTRAFFICSEVERITY[DRUG0A]`) %>%
  rename(SEVERITY_DUI=`DUISEVERITY[DUI0A]`) %>%
  rename(SEVERITY_FRAUD=`FRAUDSEVERITY[FRAUD0A]`) %>%
  rename(SEVERITY_ARSON=`ARSONSEVERITY[ARSON3A]`) %>%
  rename(SEVERITY_BURGLARY=`BURGLARYSEVERITY[BURG0A]`) %>%
  rename(SEVERITY_DOMESTIC=`DOMESTICSEVERITY[DV0A]`) %>%
  rename(SEVERITY_SEXUAL=`SEXUALSEVERITY[SEXUAL0A]`) %>%
  rename(SEVERITY_MURDER=`HOMICIDESEVERITY[HOM0A]`) %>%
  crime_severity_likert(.$SEVERITY_DRUGTRAFFIC, SEVERITY_DRUGTRAFFIC_likert) %>%
  crime_severity_likert(.$SEVERITY_DUI, SEVERITY_DUI_likert) %>%
  crime_severity_likert(.$SEVERITY_FRAUD, SEVERITY_FRAUD_likert) %>%
  crime_severity_likert(.$SEVERITY_ARSON, SEVERITY_ARSON_likert) %>%
  crime_severity_likert(.$SEVERITY_BURGLARY, SEVERITY_BURGLARY_likert) %>%
  crime_severity_likert(.$SEVERITY_DOMESTIC, SEVERITY_DOMESTIC_likert) %>%
  crime_severity_likert(.$SEVERITY_SEXUAL, SEVERITY_SEXUAL_likert) %>%
  crime_severity_likert(.$SEVERITY_MURDER, SEVERITY_MURDER_likert)
  
#'CERTAINTY' & UNCERTAINTY CONVERSIONS
survey <- survey %>%
  mutate(CERTAINTY_DRUGTRAFFIC = ifelse(RANDOMISER==1, `DRUGTRAFFIC1[DRUG1A]`, 100 - `DRUGTRAFFICK2[DRUG2A]`),
         CERTAINTY_DUI = ifelse(RANDOMISER==1, `DUI1[DUI1A]`, 100 - `DUI2[DUI2A]`),
         CERTAINTY_FRAUD = ifelse(RANDOMISER==1, `FRAUD1[FRAUD1A]`, 100 - `FRAUD2[FRAUD2A]`),
         CERTAINTY_ARSON = ifelse(RANDOMISER==1, `ARSON1[ARSON1A]`, 100 - `ARSON2[ARSON2A]`),
         CERTAINTY_BURGLARY = ifelse(RANDOMISER==1, `BURGLARY1[BURG1A]`, 100 - `BURGLARY2[BURG2A]`),
         CERTAINTY_DOMESTIC = ifelse(RANDOMISER==1, `DOMESTIC1[DV1A]`, 100 - `DOMESTIC2[DV2A]`),
         CERTAINTY_SEXUAL = ifelse(RANDOMISER==1, `SEXUAL1[SEXUAL1A]`, 100 - `SEXUAL2[SEXUAL2A]`),
         CERTAINTY_MURDER = ifelse(RANDOMISER==1, `HOMICIDE1[HOM1A]`, 100 - `HOMICIDE2[HOM2A]`)
  ) %>%
  mutate(UNCERTAINTY_DRUGTRAFFIC = 100 - CERTAINTY_DRUGTRAFFIC,
         UNCERTAINTY_DUI = 100 - CERTAINTY_DUI,
         UNCERTAINTY_FRAUD = 100 - CERTAINTY_FRAUD,
         UNCERTAINTY_ARSON = 100 - CERTAINTY_ARSON,
         UNCERTAINTY_BURGLARY = 100 - CERTAINTY_BURGLARY,
         UNCERTAINTY_DOMESTIC = 100 - CERTAINTY_DOMESTIC,
         UNCERTAINTY_SEXUAL = 100 - CERTAINTY_SEXUAL,
         UNCERTAINTY_MURDER = 100 - CERTAINTY_MURDER
  )

##### 5. TIDY UP DATA #####
#REMOVE VARIABLES
survey <- survey[-c(1:4,6,9,11,13,15,16:19,21,24,28,41,
                    43:44,46:47,49:50,52:53,55:56,58:59,61:62,64:65,
                    68:71)]
#RE-ORDER VARIABLES
survey <- survey %>%
  relocate(c(colnames(.)[grepl("_",colnames(.))]), 
           .after = last_col()) %>%
  relocate(c(44,43),
           .after = 15) %>%
  relocate(c(colnames(.)[grepl("levels",colnames(.))]),
           .after = 19) %>%
  relocate(c(colnames(.)[grepl("CRIME_",colnames(.))]),
           .after = 25) %>%
  relocate(c(colnames(.)[grepl("CJS",colnames(.))]),
           .after = 32) %>%
  relocate(c(colnames(.)[grepl("SEVERITY",colnames(.))]),
           .after = 40) %>%
  relocate(c(colnames(.)[grepl("CERTAINTY",colnames(.))]),
           .after = last_col()) %>%
  relocate(9, .after = 1) %>%
  relocate(9, .after = 16) %>%
  relocate(9, .after = 1)

##### 6. EXPORT DATA #####
write.csv(survey, file=paste0(local_path,"\\results.csv"), row.names = FALSE)
