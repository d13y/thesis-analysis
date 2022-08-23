##### 0. PACKAGES #####
library(tidyverse)
library(EnvStats)

##### 1. INITIAL ZERO REMOVAL #####
rm0 <- function(x, digits = 3) { 
  ncode <- paste0("%.", digits, "f")
  sub("^(-?)0.", "\\1.", sprintf(ncode, x))
}

##### 2. <DATA-CLEAN.R> SEVERITY LIKERT #####
severity_likert <- function(df, old_column, new_column) {
  df %>% 
    mutate("{{new_column}}":=case_when(
      old_column=="Not At All Severe" ~ 1,
      old_column=="Somewhat Severe" ~ 2,
      old_column=="Moderately Severe" ~ 3,
      old_column=="Very Severe" ~ 4,
      old_column=="Extremely Severe" ~ 5
    ))
}

##### 3. <DATA-CLEAN.R> OUTLIER IDENTIFICATION #####
outlier_flag <- function(df, old_column, new_column, limit) {
  
  outliers <- rosnerTest(old_column, k=limit, alpha = 0.05, warn = FALSE)
  largest_outlier_value <- max(outliers$all.stats$Value[which(outliers$all.stats$Outlier == TRUE)])
  
  df <- df %>%
    mutate("{{new_column}}":=case_when(
      old_column <= largest_outlier_value ~ 1,
      old_column > largest_outlier_value ~ 0,
      TRUE ~ NA_real_
    ))
}

##### 4. <DATA-CLEAN.R> VARIABLE TRANSFORMATION <Smithson & Verkuilen, 2006> #####
transform_SV <- function(df, old_column, new_column) {
  df <- df %>%
    mutate("{{new_column}}":= ((old_column*(nrow(df)-1))+0.5)/nrow(df))
}

##### 5. <*.Rmd> ROBUST STANDARD ERRORS #####
robust_se <- function(model) {
  return(sqrt(diag(vcovHC(model, type = "HC1"))))
}

##### 6. <*.Rmd WALD TEST CONVERSION #####
waldTest <- function(model, vars) {
  
  WT <- wald.test(Sigma=vcov(model), b=coef(model), Terms=vars)
  
  WT_stars <- ifelse(WT$result$chi2[3]<0.01, "***", 
                ifelse(WT$result$chi2[3]<0.05, "**", 
                ifelse(WT$result$chi2[3]<0.1, "*", "")))
  
  WT_string <- paste0(rm0(WT$result$chi2[1]), WT_stars, " (df = ", #chi2
                      rm0(WT$result$chi2[2], digits=0),"; ",      #df
                      model$df.residual+1, ")"                    #obs
                      )
  return(WT_string)
}

##### 7. <ANALYSIS-ROBUSTNESS.Rmd> HETEROGENOUS EFFECTS #####
heterogenous_effects <- function(surveyData, formulaOriginal, 
                                 interactionHeterogenous){
  
  formulaHeterogenous <- as.formula(paste(c(formulaOriginal, 
                                             interactionHeterogenous), 
                                           collapse = " + "))
  olsHeterogenous <- lm(data=surveyData,
                        formula=formulaHeterogenous)
  
  return(olsHeterogenous)
  
}

##### 8. <*.Rmd> 'CRIME'GAZER WRAPPER #####
crimegazer <- function(model1, model2, model3, 
                       d1, d2, d3,
                       wald1, wald2, wald3,
                       caption) {
  
  return(
    stargazer(model1, model2, model3,
              
              title = caption,
              type = "html",
              
              se = list(robust_se(model1), robust_se(model2), robust_se(model3)),
              report = "vcs*",
              keep.stat = c("n", "f"),
              add.lines = list(c("Cohen's D<sup>&#8224;</sup>", 
                                 d1, d2, d3),
                               c("Wald Test<sup>&#8225;</sup>",
                                 wald1, wald2, wald3)
              ),
              
              dep.var.caption = "Certainty of Guilt Required to Convict (%)",
              dep.var.labels = c("Statutory", "Property", "Persons"),
              
              covariate.labels = c("Treatment", "Severity", 
                                   "Age", "Media Consumption",
                                   "Gender", "Race", "Education",
                                   "Punitive Attitude", "Fear of Crime"),
              order = c(1:2, 3, 5:7, 4, 8:9),
              
              intercept.top = FALSE,
              float = TRUE,
              digits = 3,
              digits.extra = 0,
              initial.zero = FALSE,
              
              align = TRUE,
              no.space = TRUE,
              single.row = FALSE,
              font.size = "small",
              
              notes = c("Robust standard errors in parentheses.",
                        "<sup>&#8224;</sup>Cohen's D is reported for the Treatment variable.",
                        "<sup>&#8225;</sup>Wald Test is reported for joint significance of all explanatory variables, excluding Treatment."
              ),
              notes.append = TRUE,
              notes.align = "l"
    )
  )
  
}

##### 9. <*.Rmd> 'SOLO'GAZER WRAPPER #####
sologazer <- function(model1, 
                       columnSearch) {
  
  columnCrime <- which(colnames(survey)==columnSearch)
  d1 <- cohens_d(surveyControl[[columnCrime]],
                 surveyTreatment[[columnCrime]])
  
  w1 <- waldTest(model1, vars=2:9)
  
  return(
    stargazer(model1,
              
              title = columnSearch,
              type = "html",
              
              se = list(robust_se(model1)),
              report = "vcs*",
              keep.stat = c("n", "f"),
              add.lines = list(c("Cohen's D<sup>&#8224;</sup>", 
                                 rm0(d1$Cohens_d)),
                               c("Wald Test<sup>&#8225;</sup>",
                                 w1)
              ),
              
              dep.var.caption = "Certainty of Guilt Required to Convict (%)",
              
              covariate.labels = c("Treatment", "Severity", 
                                   "Age", "Media Consumption",
                                   "Gender", "Race", "Education",
                                   "Punitive Attitude", "Fear of Crime"),
              order = c(1:2, 3, 5:7, 4, 8:9),
              
              intercept.top = FALSE,
              float = TRUE,
              digits = 3,
              digits.extra = 0,
              initial.zero = FALSE,
              
              align = TRUE,
              no.space = TRUE,
              single.row = FALSE,
              font.size = "small",
              
              notes = c("Robust standard errors in parentheses.",
                        "<sup>&#8224;</sup>Cohen's D is reported for the Treatment variable.",
                        "<sup>&#8225;</sup>Wald Test is reported for joint significance of all explanatory variables, excluding Treatment."
              ),
              notes.append = TRUE,
              notes.align = "l"
    )
  )
  
}