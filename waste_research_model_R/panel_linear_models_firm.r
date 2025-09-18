#load libraries
library(plm)
library(lmtest)
library(sandwich)
library(tidyverse)
library(corrr)
library(car)

#load in the dataset that was prepared in 5_prepare_all_vars.ipynb
df <- read.csv("waste_research_model_R\\df_new.csv")

#select relevant variables and drop NAs
df_model <- df %>%
  select("country", "year",
         "GDPpc_log", "POPden_log", "URB", "EDU", "gov_right1",
         "EPS", "BIZprod", "BIZrec", "BIZred",
         "GENpac", "RECpac", "RECpac_percent",
         "GENplas", "RECplas", "RECplas_percent")

df_model <- na.omit(df_model)

View(df_model)

#check the structure of the data df_model
df_model_2 <- df %>%
  select("country", "year",
         "GDPpc_log", "POPden_log", "URB", "EDU", "gov_right1",
         "BIZprod", "BIZrec", "BIZred",
         "GENpac", "RECpac", "RECpac_percent",
         "GENplas", "RECplas", "RECplas_percent")

df_model_2 <- na.omit(df_model_2)

View(df_model_2)

corr <- df_model  %>%
  select("BIZprod", "BIZred", "BIZrec", "EPS",
         "GDPpc_log", "POPden_log", "URB", "EDU", "gov_right1",
         "GENpac", "RECpac", "RECpac_percent") %>%
  correlate(use = "pairwise.complete.obs")


print(corr, n = nrow(corr))


#FE model for generated packaging waste

fe_GENpac <- plm(GENpac ~ BIZrec  + BIZprod + EPS +
                   GDPpc_log + POPden_log +
                   URB + EDU + gov_right1,
                 data = df_model, model = "within", effect = "twoways")

summary(fe_GENpac)

#Wooldridge serial correlation test
pwartest(fe_GENpac)

#Pesaran CD test for cross-sectional dependence
pcdtest(fe_GENpac, test = "cd")

#Breusch and Pagan test for heteroskedasticity
plmtest(fe_GENpac, type = "bp")

#robust SEs (cluster by country)
coeftest(fe_GENpac, vcov = vcovHC(fe_GENpac, type = "sss"))

#RE model for generated packaging waste

re_GENpac <- plm(GENpac ~ BIZrec  + BIZprod + EPS +
                   GDPpc_log + POPden_log +
                   URB + EDU + gov_right1,
                 data = df_model, model = "random")

summary(re_GENpac)

coeftest(re_GENpac, vcov = vcovHC(re_GENpac, type = "sss"))

#Hausman test
phtest(fe_GENpac, re_GENpac)

#FE model for recycling percentage

fe_RECpac_percent <- plm(RECpac_percent ~ BIZrec + BIZprod + EPS +
                           GDPpc_log + POPden_log +
                           URB + EDU + gov_right1,
                          data = df_model, model = "within", effect = "twoways")

summary(fe_RECpac_percent)

#Wooldridge serial correlation test
pwartest(fe_RECpac_percent)

#Pesaran CD test for cross-sectional dependence
pcdtest(fe_RECpac_percent, test = "cd")

#Lagrange Multiplier (Breusch and Pagan) test for heteroskedasticity 
plmtest(fe_RECpac_percent, type = "bp")

coeftest(fe_RECpac_percent, vcov = vcovHC(fe_RECpac_percent, type = "sss"))

#RE for RECpac_percent
re_RECpac_percent <- plm(RECpac ~ BIZrec + BIZprod + EPS +
                           GDPpc_log + POPden_log +
                           URB + EDU + gov_right1,
                         data = df_model, model = "random")

summary(re_RECpac_percent)

coeftest(re_RECpac_percent, vcov = vcovHC(re_RECpac_percent, type = "sss"))

#Hausman test
phtest(fe_RECpac_percent, re_RECpac_percent)
