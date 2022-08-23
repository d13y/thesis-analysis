##### 0A. PACKAGES #####
library(tidyverse)
library(EnvStats)

##### 0B. FUNCTIONS #####
source("functions.R")

##### 1. IMPORT DATA #####
local_path <- "C:\\Users\\danie\\Documents\\Masters\\3. THESIS\\Analysis"
#Source: <https://survey.ucd.ie/crimeattitudes/index.php/635612?lang=en> as at July 2022
surveyImport <- readxl::read_excel(paste0(local_path, "\\results-survey635612.xlsx"))

##### 2. SURVEY FLAGS #####
#Survey Progress
surveyFlag <- surveyImport %>%
  mutate(ELIGIBLE=case_when(CONSENT=="Yes" & AGE>=18 ~ 1, TRUE ~ 0),
         EXPERIMENT=case_when(!is.na(RANDOMISER) ~ 1, TRUE ~ 0),
         COMPLETE=case_when(!is.na(submitdate) ~ 1, TRUE ~ 0),
         SUBMIT=case_when(SUBMIT=="Yes" ~ 1, TRUE ~ 0)) %>%
  mutate(ATTRITION=case_when(EXPERIMENT==1 & SUBMIT==0 ~ 0,
                             EXPERIMENT==1 & SUBMIT==1 ~ 1,
                             TRUE ~ NA_real_))

##### 3. RENAME VARIABLES #####
surveyRename <- surveyFlag %>%
  #Nationality
  rename(NATIONALITY_PRIMARY=NATIONALITY1,
         NATIONALITY_SECONDARY=NATIONALITY2) %>%
  #RACE
  rename(ASIAN=`RACE[RACE1]`,
         BLACK=`RACE[RACE2]`,
         WHITE=`RACE[RACE3]`,
         RACE_OTHER=`RACE[other]`) %>%
  #Political View
  rename(POLITICS=`POLITICS[POL]`) %>%
  #Household Info
  rename(HOUSEHOLD_SIZE=SIZE, 
         HOUSEHOLD_MONTHLY_INCOME=INCOME) %>%
  #Media Consumption
  rename(MEDIA_GENERAL=`MEDIA[MEDIA1]`,
         MEDIA_CRIME=`MEDIA[MEDIA2]`) %>%
  #Experiences with Crime & the Criminal Justice System
  rename(CRIME_FEAR=`CJSEXPERIENCE[EXP1]`,
         CRIME_WITNESS=`CJSEXPERIENCE[EXP2]`,
         CRIME_VICTIM=`CJSEXPERIENCE[EXP3]`,
         CRIME_CONVICT=`CJSEXPERIENCE[EXP4]`,
         CRIME_JURY=`CJSEXPERIENCE[EXP5]`,
         CRIME_WORK=`CJSEXPERIENCE[EXP6]`) %>% 
  #Purpose of the Criminal Justice System
  rename(CJS_PURPOSE_FIRST=`CJSPURPOSE[1]`,
         CJS_PURPOSE_SECOND=`CJSPURPOSE[2]`, 
         CJS_PURPOSE_THIRD=`CJSPURPOSE[3]`, 
         CJS_PURPOSE_FOURTH=`CJSPURPOSE[4]`) %>%
  #Crime Severity
  rename(SEVERITY_DRUGTRAFFIC=`DRUGTRAFFICSEVERITY[DRUG0A]`,
         SEVERITY_DUI=`DUISEVERITY[DUI0A]`,
         SEVERITY_FRAUD=`FRAUDSEVERITY[FRAUD0A]`,
         SEVERITY_ARSON=`ARSONSEVERITY[ARSON3A]`, #inconsistent label!
         SEVERITY_BURGLARY=`BURGLARYSEVERITY[BURG0A]`,
         SEVERITY_DOMESTIC=`DOMESTICSEVERITY[DV0A]`,
         SEVERITY_SEXUAL=`SEXUALSEVERITY[SEXUAL0A]`,
         SEVERITY_MURDER=`HOMICIDESEVERITY[HOM0A]`) %>%
  #Certainty of Guilt
  rename(CERTAINTY_DRUGTRAFFIC=`DRUGTRAFFIC1[DRUG1A]`,
           CERTAINTY_DUI=`DUI1[DUI1A]`,
           CERTAINTY_FRAUD=`FRAUD1[FRAUD1A]`,
           CERTAINTY_ARSON=`ARSON1[ARSON1A]`,
           CERTAINTY_BURGLARY=`BURGLARY1[BURG1A]`,
           CERTAINTY_DOMESTIC=`DOMESTIC1[DV1A]`,
           CERTAINTY_SEXUAL=`SEXUAL1[SEXUAL1A]`,
           CERTAINTY_MURDER=`HOMICIDE1[HOM1A]`) %>%
  #Uncertainty of Guilt
  rename(UNCERTAINTY_DRUGTRAFFIC=`DRUGTRAFFICK2[DRUG2A]`, #inconsistent spelling!
         UNCERTAINTY_DUI=`DUI2[DUI2A]`,
         UNCERTAINTY_FRAUD=`FRAUD2[FRAUD2A]`,
         UNCERTAINTY_ARSON=`ARSON2[ARSON2A]`,
         UNCERTAINTY_BURGLARY=`BURGLARY2[BURG2A]`,
         UNCERTAINTY_DOMESTIC=`DOMESTIC2[DV2A]`,
         UNCERTAINTY_SEXUAL=`SEXUAL2[SEXUAL2A]`,
         UNCERTAINTY_MURDER=`HOMICIDE2[HOM2A]`)

##### 4. DEMOGRAPHIC VARIABLES: 'OTHER' ADJUSTMENTS #####
surveyDemo <- surveyRename %>%
  #Adjust Nationality responses
  mutate_at(vars(NATIONALITY_PRIMARY),
            ~ifelse(!is.na(`NATIONALITY1[other]`),
                    `NATIONALITY1[other]`, NATIONALITY_PRIMARY)) %>%
  mutate_at(vars(NATIONALITY_SECONDARY), ~na_if(.,"Not applicable")) %>%
  mutate_at(vars(NATIONALITY_SECONDARY),
            ~ifelse(!is.na(`NATIONALITY2[other]`),
                    `NATIONALITY2[other]`, NATIONALITY_SECONDARY)) %>%
  mutate_at(vars(NATIONALITY_SECONDARY),
            ~ifelse(NATIONALITY_PRIMARY==NATIONALITY_SECONDARY, NA, .)) %>%
  #Adjust Religion responses (manually re-coded on adhoc basis)
  mutate_at(vars(RELIGION),  
            ~ifelse(grepl("catholic",`RELIGION[other]`, ignore.case=TRUE),
                    "Christian", .)) %>%
  #Adjust Education responses (manually re-coded on adhoc basis)
  mutate_at(vars(EDUCATION),
            ~ifelse(grepl("chartered|profession|master|accountant",
                          `EDUCATION[other]`, ignore.case=TRUE),
                          "Postgraduate",
              ifelse(grepl("dip|college|bachelor|plc", 
                           `EDUCATION[other]`, ignore.case=TRUE), 
                            "Undergraduate", .)))

##### 5. DEMOGRAPHIC VARIABLES: RE-CODING #####
surveyDemo <- surveyDemo %>%
  #Education (Levels)
  mutate(levels_EDUCATION=case_when(
    EDUCATION=="Primary" ~ 0,
    EDUCATION=="Lower Secondary" ~ 1,
    EDUCATION=="Upper Secondary" ~ 2,
    EDUCATION=="Apprenticeship" ~ 3,       
    EDUCATION=="Undergraduate" ~ 4,
    EDUCATION=="Postgraduate" ~ 5)) %>%
  #Politics (Levels)
  mutate(levels_POLITICS=case_when(
    POLITICS=="Liberal" ~ 0,
    POLITICS=="Centre-Left" ~ 1,
    POLITICS=="Centrist" ~ 2,
    POLITICS=="Centre-Right" ~ 3,
    POLITICS=="Conservative" ~ 4)) %>%
  #Media Consumption (Sum)
  rowwise() %>% mutate(MEDIA_SUM=sum(c_across(MEDIA_GENERAL:MEDIA_CRIME))) %>% ungroup() %>%
  #Income
    #Purchasing Power Parity (PPP) <https://data.worldbank.org/indicator/PA.NUS.PPP>, 2021 data.
    mutate(HOUSEHOLD_MONTHLY_INCOME_PPP_USD=case_when(
      grepl("AUD", CURRENCY) ~ HOUSEHOLD_MONTHLY_INCOME/1.4389790,
      grepl("CAD", CURRENCY) ~ HOUSEHOLD_MONTHLY_INCOME/1.2530660,
      grepl("GBP", CURRENCY) ~ HOUSEHOLD_MONTHLY_INCOME/0.6928020,
      grepl("INR", CURRENCY) ~ HOUSEHOLD_MONTHLY_INCOME/23.1381376,
      grepl("SEK", CURRENCY) ~ HOUSEHOLD_MONTHLY_INCOME/8.7088530,
      grepl("SGD", CURRENCY) ~ HOUSEHOLD_MONTHLY_INCOME/0.8395717,
      grepl("TRY", CURRENCY) ~ HOUSEHOLD_MONTHLY_INCOME/2.7818510,
      grepl("ZAR", CURRENCY) ~ HOUSEHOLD_MONTHLY_INCOME/7.1680967,
      grepl("EUR", CURRENCY) ~ HOUSEHOLD_MONTHLY_INCOME/0.667415)) %>% #<https://stats.oecd.org/viewhtml.aspx?datasetcode=SNA_TABLE4&lang=en#>, 2021 data.
    #Annualised PPP
    mutate(HOUSEHOLD_ANNUAL_INCOME_PPP_USD=12*HOUSEHOLD_MONTHLY_INCOME_PPP_USD) %>%
    #Equivalised Income, PPP
    mutate(HOUSEHOLD_ANNUAL_EQUIVALISED_INCOME_PPP_USD=HOUSEHOLD_ANNUAL_INCOME_PPP_USD/sqrt(HOUSEHOLD_SIZE)) %>%
    #Log of Equivalised Income
    mutate(LOG_HOUSEHOLD_ANNUAL_EQUIVALISED_INCOME_PPP_USD=case_when(
            HOUSEHOLD_ANNUAL_EQUIVALISED_INCOME_PPP_USD==0 ~ NA_real_,
            TRUE ~ log(HOUSEHOLD_ANNUAL_EQUIVALISED_INCOME_PPP_USD))) %>%
  #Race: Relabel
  mutate(RACE=case_when(
    #Mixed Race
    grepl("mixed", RACE_OTHER, ignore.case = TRUE) |
    grepl("irish chinese", RACE_OTHER, ignore.case = TRUE) | #manual recode on adhoc basis
    (ASIAN=="Yes" & BLACK=="Yes") |
    (ASIAN=="Yes" & WHITE=="Yes") |
    (BLACK=="Yes" & WHITE=="Yes") ~ "Mixed",
    #Single Race
    ASIAN=="Yes" ~ "Asian",
    BLACK=="Yes" ~ "Black",
    WHITE=="Yes" ~ "White",
    #Other
    (ASIAN=="No" & BLACK=="No" & WHITE=="No") ~ "Other")) %>%
  #Race: Recode
  mutate_at(vars(ASIAN), ~ifelse(.=="Yes", 1, 0)) %>%
  mutate_at(vars(BLACK), ~ifelse(.=="Yes", 1, 0)) %>%
  mutate_at(vars(WHITE), ~ifelse(.=="Yes", 1, 0)) %>%
  mutate_at(vars(RACE_OTHER), ~ifelse(RACE=="Other", 1, 0)) %>%
  mutate(MIXED=case_when(RACE=="Mixed" ~ 1, TRUE ~ 0)) %>%
  #CJS Purpose
  mutate(DETERRENCE_rank=case_when(CJS_PURPOSE_FIRST=="Deterrence of Crime" ~ 1, CJS_PURPOSE_SECOND=="Deterrence of Crime" ~ 2, CJS_PURPOSE_THIRD=="Deterrence of Crime" ~ 3, CJS_PURPOSE_FOURTH=="Deterrence of Crime" ~ 4)) %>%
  mutate(PROTECTION_rank=case_when(CJS_PURPOSE_FIRST=="Protecting the Public" ~ 1, CJS_PURPOSE_SECOND=="Protecting the Public" ~ 2, CJS_PURPOSE_THIRD=="Protecting the Public" ~ 3, CJS_PURPOSE_FOURTH=="Protecting the Public" ~ 4)) %>%
  mutate(PUNISHMENT_rank=case_when(CJS_PURPOSE_FIRST=="Punishment" ~ 1, CJS_PURPOSE_SECOND=="Punishment" ~ 2, CJS_PURPOSE_THIRD=="Punishment" ~ 3, CJS_PURPOSE_FOURTH=="Punishment" ~ 4)) %>%
  mutate(REHABILITATION_rank=case_when(CJS_PURPOSE_FIRST=="Rehabilitation" ~ 1, CJS_PURPOSE_SECOND=="Rehabilitation" ~ 2, CJS_PURPOSE_THIRD=="Rehabilitation" ~ 3, CJS_PURPOSE_FOURTH=="Rehabilitation" ~ 4)) %>%
  #Crime & Criminal Justice System Experiences
  mutate(across(c(which(grepl("CRIME_",colnames(surveyDemo), ignore.case = TRUE)==TRUE)), ~ifelse(.=="Yes", 1, 0))) %>%
  rowwise() %>% mutate(CRIME_SUM=sum(c_across(CRIME_FEAR:CRIME_WORK))) %>% ungroup()

##### 6. CRIME VARIABLES: RECODING #####
surveyCrime <- surveyDemo %>%
  #Recode Randomiser into 0 & 1
  mutate_at(vars(RANDOMISER), ~ifelse(RANDOMISER==1,0, ifelse(RANDOMISER==2,1, NA))) %>%
  #Severity conversion into numerical Likert scale responses (uses custom function)
  severity_likert(.$SEVERITY_DRUGTRAFFIC, likert_SEVERITY_DRUGTRAFFIC) %>%
  severity_likert(.$SEVERITY_DUI, likert_SEVERITY_DUI) %>%
  severity_likert(.$SEVERITY_FRAUD, likert_SEVERITY_FRAUD) %>%
  severity_likert(.$SEVERITY_ARSON, likert_SEVERITY_ARSON) %>%
  severity_likert(.$SEVERITY_BURGLARY, likert_SEVERITY_BURGLARY) %>%
  severity_likert(.$SEVERITY_DOMESTIC, likert_SEVERITY_DOMESTIC) %>%
  severity_likert(.$SEVERITY_SEXUAL, likert_SEVERITY_SEXUAL) %>%
  severity_likert(.$SEVERITY_MURDER, likert_SEVERITY_MURDER) %>%
  #Re-scale certainty/uncertainty into [0,1] (i.e. unit bounded interval)
  mutate(across(c(which(grepl("certainty",colnames(surveyDemo), ignore.case = TRUE)==TRUE)), ~ ./100)) %>%
  #Certainty conversion
  mutate_at(vars(CERTAINTY_DRUGTRAFFIC), ~ifelse(RANDOMISER==1, 1-UNCERTAINTY_DRUGTRAFFIC, .)) %>%
  mutate_at(vars(CERTAINTY_DUI), ~ifelse(RANDOMISER==1, 1-UNCERTAINTY_DUI, .)) %>%
  mutate_at(vars(CERTAINTY_ARSON), ~ifelse(RANDOMISER==1, 1-UNCERTAINTY_ARSON, .)) %>%
  mutate_at(vars(CERTAINTY_BURGLARY), ~ifelse(RANDOMISER==1, 1-UNCERTAINTY_BURGLARY, .)) %>%
  mutate_at(vars(CERTAINTY_FRAUD), ~ifelse(RANDOMISER==1, 1-UNCERTAINTY_FRAUD, .)) %>%
  mutate_at(vars(CERTAINTY_DOMESTIC), ~ifelse(RANDOMISER==1, 1-UNCERTAINTY_DOMESTIC, .)) %>%
  mutate_at(vars(CERTAINTY_SEXUAL), ~ifelse(RANDOMISER==1, 1-UNCERTAINTY_SEXUAL, .)) %>%
  mutate_at(vars(CERTAINTY_MURDER), ~ifelse(RANDOMISER==1, 1-UNCERTAINTY_MURDER, .)) %>%
  #Uncertainty conversion
  mutate_at(vars(UNCERTAINTY_DRUGTRAFFIC), ~ifelse(RANDOMISER==0, 1-CERTAINTY_DRUGTRAFFIC, .)) %>%
  mutate_at(vars(UNCERTAINTY_DUI), ~ifelse(RANDOMISER==0, 1-CERTAINTY_DUI, .)) %>%
  mutate_at(vars(UNCERTAINTY_ARSON), ~ifelse(RANDOMISER==0, 1-CERTAINTY_ARSON, .)) %>%
  mutate_at(vars(UNCERTAINTY_BURGLARY), ~ifelse(RANDOMISER==0, 1-CERTAINTY_BURGLARY, .)) %>%
  mutate_at(vars(UNCERTAINTY_FRAUD), ~ifelse(RANDOMISER==0, 1-CERTAINTY_FRAUD, .)) %>%
  mutate_at(vars(UNCERTAINTY_DOMESTIC), ~ifelse(RANDOMISER==0, CERTAINTY_DOMESTIC, .)) %>%
  mutate_at(vars(UNCERTAINTY_SEXUAL), ~ifelse(RANDOMISER==0, 1-CERTAINTY_SEXUAL, .)) %>%
  mutate_at(vars(UNCERTAINTY_MURDER), ~ifelse(RANDOMISER==0, 1-CERTAINTY_MURDER, .))

##### 7. REGRESSION PREPERATION #####
surveyReg <- surveyCrime %>%
  #Participant error correction (identified manually/adhoc)
  mutate_at(vars(HOUSEHOLD_SIZE), ~ifelse(seed==1934967069, NA, HOUSEHOLD_SIZE)) %>% #4800 people
  mutate_at(vars(MEDIA_SUM), ~ifelse(seed==1030954631, NA, MEDIA_SUM)) %>% #24hrs/day
  mutate_at(vars(MEDIA_SUM), ~ifelse(seed==1536059225, NA, MEDIA_SUM)) %>% #24hrs/day
  #Gender (dummy variable: 0 = male; 1 = female)
  mutate(d_GENDER =
    ifelse(grepl("Male", GENDER), 0,
    ifelse(grepl("Female", GENDER), 1, 
    NA))) %>%
  #Education (dummy variable: 1 = degree or higher education)
  mutate(d_EDUCATION = case_when(
    levels_EDUCATION <=2 ~ 0,
    levels_EDUCATION >= 3 ~ 1,
    TRUE ~ NA_real_)) %>%
  #Race (dummy variable: 0 = white; 1 = BAME)
  mutate(d_RACE =
    ifelse(grepl("White", RACE), 0,
    ifelse(grepl("Asian|Black|Mixed|Other", RACE), 1,
    NA))) %>%
  #Religion (dummy variable: 0 = non-religious; 1 = religious)
  mutate(d_RELIGION =
    ifelse(grepl("No religion", RELIGION), 0,
    ifelse(grepl("Buddhist|Christian|Hindu|Jewish|Muslim|Sikh|Other", RELIGION), 1,
    NA))) %>%
  #Age (dummy variable: 0 = below median; 1 = above median)
  mutate(d_AGE = case_when(
    AGE >= median(AGE, na.rm=TRUE) ~ 1,
    AGE < median(AGE, na.rm=TRUE) ~ 0,
    TRUE ~ NA_real_)) %>%
  #Media Consumption (dummy variable: 0 = below median; 1 = above median)
  mutate(d_MEDIA_SUM = case_when(
    MEDIA_SUM >= median(MEDIA_SUM, na.rm=TRUE) ~ 1,
    MEDIA_SUM < median(MEDIA_SUM, na.rm=TRUE) ~ 0,
    TRUE ~ NA_real_)) %>%
  #Politics (factors/levels)
  mutate(f_POLITICS = case_when(
    levels_POLITICS <= 1 ~ 2, #left leaning
    levels_POLITICS >= 3 ~ 3, #right leaning
    levels_POLITICS == 2 ~ 1, #centrist
    TRUE ~ NA_real_)) %>%
  #Employment (factors/levels)
  mutate(f_EMPLOYMENT = 
    ifelse(grepl("Full time|Part time|Self employed", EMPLOYMENT), 1, #employed
    ifelse(grepl("Unemployed", EMPLOYMENT), 2, #unemployed
    ifelse(grepl("Retired|Student|Other", EMPLOYMENT), 3, #economically inactive
    NA)))) %>%
  #Primary Purpose of CJS
    #Deterrence
    mutate(d_DETERRENCE=case_when(
      DETERRENCE_rank==1 ~ 1,
      DETERRENCE_rank>=2 ~ 0,
      TRUE ~ NA_real_)) %>%
    #Protection
    mutate(d_PROTECTION=case_when(
      PROTECTION_rank==1 ~ 1,
      PROTECTION_rank>=2 ~ 0,
      TRUE ~ NA_real_)) %>%
    #Punishment
    mutate(d_PUNISHMENT=case_when(
      PUNISHMENT_rank==1 ~ 1,
      PUNISHMENT_rank>=2 ~ 0,
      TRUE ~ NA_real_)) %>%
    #Rehabilitation
    mutate(d_REHABILITATION=case_when(
      REHABILITATION_rank==1 ~ 1,
      REHABILITATION_rank>=2 ~ 0,
      TRUE ~ NA_real_)) %>%
  #Aggregated explanatory outcomes (severity & certainty)
    #Statutory crimes
    mutate(CERTAINTY_STATUTORY=rowMeans(.[,c("CERTAINTY_DRUGTRAFFIC","CERTAINTY_DUI")],na.rm = TRUE)) %>%
    mutate(UNCERTAINTY_STATUTORY=1-CERTAINTY_STATUTORY) %>%
    mutate(likert_SEVERITY_STATUTORY=rowMeans(.[,c("likert_SEVERITY_DRUGTRAFFIC","likert_SEVERITY_DUI")],na.rm = TRUE)) %>%
    #Crimes against property
    mutate(CERTAINTY_PROPERTY=rowMeans(.[,c("CERTAINTY_ARSON","CERTAINTY_BURGLARY","CERTAINTY_FRAUD")],na.rm = TRUE)) %>%
    mutate(UNCERTAINTY_PROPERTY=1-CERTAINTY_PROPERTY) %>%
    mutate(likert_SEVERITY_PROPERTY=rowMeans(.[,c("likert_SEVERITY_ARSON","likert_SEVERITY_BURGLARY","likert_SEVERITY_FRAUD")],na.rm = TRUE)) %>%
    #Crimes against persons
    mutate(CERTAINTY_PERSONS=rowMeans(.[,c("CERTAINTY_DOMESTIC","CERTAINTY_MURDER","CERTAINTY_SEXUAL")],na.rm = TRUE)) %>%
    mutate(UNCERTAINTY_PERSONS=1-CERTAINTY_PERSONS) %>%
    mutate(likert_SEVERITY_PERSONS=rowMeans(.[,c("likert_SEVERITY_DOMESTIC","likert_SEVERITY_MURDER","likert_SEVERITY_SEXUAL")],na.rm = TRUE)) %>%
    #All Crimes
    mutate(CERTAINTY_ALL=rowMeans(.[,c("CERTAINTY_DRUGTRAFFIC","CERTAINTY_DUI",
                                        "CERTAINTY_ARSON","CERTAINTY_BURGLARY","CERTAINTY_FRAUD",
                                        "CERTAINTY_DOMESTIC","CERTAINTY_MURDER","CERTAINTY_SEXUAL")],na.rm = TRUE)) %>%
    mutate(UNCERTAINTY_ALL=1-CERTAINTY_ALL) %>%
    mutate(likert_SEVERITY_ALL=rowMeans(.[,c("likert_SEVERITY_DRUGTRAFFIC","likert_SEVERITY_DUI",
                                              "likert_SEVERITY_ARSON","likert_SEVERITY_BURGLARY","likert_SEVERITY_FRAUD",
                                              "likert_SEVERITY_DOMESTIC","likert_SEVERITY_MURDER","likert_SEVERITY_SEXUAL")],na.rm = TRUE))

##### 8. SURVEY TRANSFORMATIONS #####
surveyTransform <- surveyReg %>% filter(SUBMIT==1) %>%
  #Outcome Adjustments (uses custom function; account for 0 & 1 values in logit model)
    #Certainty
    transform_SV(.$CERTAINTY_DRUGTRAFFIC, logit_CERTAINTY_DRUGTRAFFIC) %>%
    transform_SV(.$CERTAINTY_DUI, logit_CERTAINTY_DUI) %>%
    transform_SV(.$CERTAINTY_ARSON, logit_CERTAINTY_ARSON) %>%
    transform_SV(.$CERTAINTY_BURGLARY, logit_CERTAINTY_BURGLARY) %>%
    transform_SV(.$CERTAINTY_FRAUD, logit_CERTAINTY_FRAUD) %>%
    transform_SV(.$CERTAINTY_DOMESTIC, logit_CERTAINTY_DOMESTIC) %>%
    transform_SV(.$CERTAINTY_MURDER, logit_CERTAINTY_MURDER) %>%
    transform_SV(.$CERTAINTY_SEXUAL, logit_CERTAINTY_SEXUAL) %>%
    transform_SV(.$CERTAINTY_STATUTORY, logit_CERTAINTY_STATUTORY) %>%
    transform_SV(.$CERTAINTY_PROPERTY, logit_CERTAINTY_PROPERTY) %>%
    transform_SV(.$CERTAINTY_PERSONS, logit_CERTAINTY_PERSONS) %>%
    transform_SV(.$CERTAINTY_ALL, logit_CERTAINTY_ALL) %>%
    #Uncertainty
    transform_SV(.$UNCERTAINTY_DRUGTRAFFIC, logit_UNCERTAINTY_DRUGTRAFFIC) %>%
    transform_SV(.$UNCERTAINTY_DUI, logit_UNCERTAINTY_DUI) %>%
    transform_SV(.$UNCERTAINTY_ARSON, logit_UNCERTAINTY_ARSON) %>%
    transform_SV(.$UNCERTAINTY_BURGLARY, logit_UNCERTAINTY_BURGLARY) %>%
    transform_SV(.$UNCERTAINTY_FRAUD, logit_UNCERTAINTY_FRAUD) %>%
    transform_SV(.$UNCERTAINTY_DOMESTIC, logit_UNCERTAINTY_DOMESTIC) %>%
    transform_SV(.$UNCERTAINTY_MURDER, logit_UNCERTAINTY_MURDER) %>%
    transform_SV(.$UNCERTAINTY_SEXUAL, logit_UNCERTAINTY_SEXUAL) %>%
    transform_SV(.$UNCERTAINTY_STATUTORY, logit_UNCERTAINTY_STATUTORY) %>%
    transform_SV(.$UNCERTAINTY_PROPERTY, logit_UNCERTAINTY_PROPERTY) %>%
    transform_SV(.$UNCERTAINTY_PERSONS, logit_UNCERTAINTY_PERSONS) %>%
    transform_SV(.$UNCERTAINTY_ALL, logit_UNCERTAINTY_ALL) %>%
  #Outliers (re: primary explanatory variables)
    #Identify outliers
    outlier_flag(.$CERTAINTY_DRUGTRAFFIC, outlier_DRUGTRAFFIC, limit = 30) %>%
    outlier_flag(.$CERTAINTY_DUI, outlier_DUI, limit = 30) %>%
    outlier_flag(.$CERTAINTY_ARSON, outlier_ARSON, limit = 30) %>%
    outlier_flag(.$CERTAINTY_BURGLARY, outlier_BURGLARY, limit = 30) %>%
    outlier_flag(.$CERTAINTY_FRAUD, outlier_FRAUD, limit = 30) %>%
    outlier_flag(.$CERTAINTY_DOMESTIC, outlier_DOMESTIC, limit = 30) %>%
    outlier_flag(.$CERTAINTY_MURDER, outlier_MURDER, limit = 30) %>%
    outlier_flag(.$CERTAINTY_SEXUAL, outlier_SEXUAL, limit = 30) %>%
    outlier_flag(.$CERTAINTY_STATUTORY, outlier_STATUTORY, limit = 30) %>%
    outlier_flag(.$CERTAINTY_PROPERTY, outlier_PROPERTY, limit = 30) %>%
    outlier_flag(.$CERTAINTY_PERSONS, outlier_PERSONS, limit = 30) %>%
    outlier_flag(.$CERTAINTY_ALL, outlier_ALL, limit = 30) %>%
    rowwise() %>% mutate(OUTLIER_COUNT=sum(c_across(outlier_DRUGTRAFFIC:outlier_SEXUAL))) %>% ungroup() %>%
    mutate(OUTLIER=case_when(outlier_ALL == 1 ~ 1, TRUE ~ 0))

##### 9. FINAL STEPS #####
#Re-combine data frames
surveyFinal <- full_join(surveyReg, surveyTransform)
#Export results
write.csv(surveyFinal, file=paste0(local_path,"\\results.csv"), row.names = FALSE)
#Tidy environment
#rm(list=setdiff(ls(), "surveyFinal"))

