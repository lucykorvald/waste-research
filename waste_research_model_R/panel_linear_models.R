### panel data analysis of packaging waste generation and recycling ###
### R has a better built-in suite of packages for panel data analysis than Python ###
### this script uses the plm package to run fixed effects and random effects models ###

#load libraries
library(plm)
library(lmtest)
library(sandwich)
library(tidyverse)
library(corrr)
library(car)

#load in the dataset that was prepared in 5_prepare_all_vars.ipynb
df <- read.csv("~/Waste_research/df_new.csv")

#select relevant variables and drop NAs
df_model <- df %>%
  select("country", "year",
         "GDPpc_log", "POPden_log", "URB", "EDU", "gov_right1",
         "EPS", "INDimp", "INDrec", "INDred",
         "GENpac", "RECpac", "RECpac_percent",
         "GENplas", "RECplas", "RECplas_percent")

df_model <- na.omit(df_model)

View(df_model)

#correlation matrix
corr <- df_model  %>%
  select("INDrec", "INDred", "INDimp", "EPS",
         "GDPpc_log", "POPden_log", "URB", "EDU", "gov_right1",
         "GENpac", "RECpac", "RECpac_percent") %>%
  correlate(use = "pairwise.complete.obs")

corr_df <- as.data.frame(corr)
print(corr_df, row.names = FALSE)

#check VIF for multicollinearity
vif_model <- lm(RECpac ~ INDrec + INDred  + INDimp + EPS +
                  GDPpc_log + POPden_log +
                  URB + EDU + gov_right1, data = df_model)
vif(vif_model)

#set index for panel data
df_model <- pdata.frame(df_model, index = c("country", "year"))

#FE model for generated packaging waste

fe_GENpac <- plm(GENpac ~ INDrec + INDred  + INDimp + EPS +
                   GDPpc_log + POPden_log +
                   URB + EDU + gov_right1,
                 data = df_model, model = "within", effect = "twoways")

summary(fe_GENpac)

#Wooldridge serial correlation test
#(https://search.r-project.org/CRAN/refmans/plm/html/pwartest.html)
pwartest(fe_GENpac)

#Pesaran CD test for cross-sectional dependence
pcdtest(fe_GENpac, test = "cd")

#Breusch and Pagan test for heteroskedasticity
plmtest(fe_GENpac, type = "bp")

#robust SEs (cluster by country)
coeftest(fe_GENpac, vcov = vcovHC(fe_GENpac, type = "sss"))

#RE model for generated packaging waste

re_GENpac <- plm(GENpac ~ INDrec + INDred + INDimp + EPS +
                   GDPpc_log + POPden_log +
                   URB + EDU + gov_right1,
                 data = df_model, model = "random")

summary(re_GENpac)

coeftest(re_GENpac, vcov = vcovHC(re_GENpac, type = "sss"))

#Hausman test
phtest(fe_GENpac, re_GENpac)

#######--------------------------------------------------------------------

#FE model for recycled packaging waste

fe_RECpac <- plm(RECpac ~ INDrec + INDred + INDimp + EPS +
                   GDPpc_log + POPden_log +
                   URB + EDU + gov_right1,
                 data = df_model, model = "within", effect = "twoways")

summary(fe_RECpac)

#Wooldridge serial correlation test
pwartest(fe_RECpac)

#Pesaran CD test for cross-sectional dependence
pcdtest(fe_RECpac, test = "cd")

#Lagrange Multiplier (Breusch and Pagan) test for heteroskedasticity 
plmtest(fe_RECpac, type = "bp")

coeftest(fe_RECpac, vcov = vcovHC(fe_RECpac, type = "HC1"))

#RE model for generated packaging waste

re_RECpac <- plm(RECpac ~ INDrec + INDred + INDimp +
                   GDPpc_log + POPden_log +
                   URB + EDU + gov_right1,
                 data = df_model, model = "random")

summary(re_RECpac)

coeftest(re_RECpac, vcov = vcovHC(re_RECpac, type = "sss"))

#Hausman test
phtest(fe_RECpac, re_RECpac)

#######--------------------------------------------------------------------

#FE model for recycling percentage

df_model <- df_model %>% mutate(
  RECpac_percent = RECpac_percent * 100,
  RECplas_percent = RECplas_percent * 100
)

fe_RECpac_percent <- plm(RECpac_percent ~ INDrec + INDred + INDimp + EPS +
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
re_RECpac_percent <- plm(RECpac ~ INDrec + INDred + INDimp + EPS +
                           GDPpc_log + POPden_log +
                           URB + EDU + gov_right1,
                         data = df_model, model = "random")

summary(re_RECpac_percent)

coeftest(re_RECpac_percent, vcov = vcovHC(re_RECpac_percent, type = "sss"))

#Hausman test
phtest(fe_RECpac_percent, re_RECpac_percent)


#####---create a linear time trend to replace time dummies --------------

df_model <- df_model %>%
  mutate(year = as.numeric(as.character(year)),
         time_trend = year - min(year) + 1)

df_model <- df_model %>%
  mutate(time_trend2 = time_trend^2)

#check correlation matrix again with time trend variables

corr_1 <- df_model  %>%
  select("INDimp", "INDred", "INDrec", "EPS",
         "time_trend", "time_trend2",
         "gov_right1", "EDU", "URB", "GDPpc_log", "POPden_log",
         "GENpac", "RECpac") %>%
  correlate(use = "pairwise.complete.obs")

print(corr_1)

#check VIF for multicollinearity again
vif_model_1 <- lm(RECpac ~ INDrec + INDred  + INDimp + EPS +
                    time_trend + time_trend2 +
                    GDPpc_log + POPden_log +
                    URB + EDU + gov_right1, data = df_model)
vif(vif_model_1)

##---------------------------------------------------------------

#FE model for GENpac with time trend
fe_GENpac_trend <- plm(GENpac ~ INDrec + INDred  + INDimp + 
                          GDPpc_log + POPden_log +
                          URB + EDU + gov_right1 +
                          time_trend + time_trend2,
                        data = df_model, 
                        model = "within", effect = "individual")
summary(fe_GENpac_trend)

#Wooldridge serial correlation test
pwartest(fe_GENpac_trend)

#Pesaran CD test for cross-sectional dependence
pcdtest(fe_GENpac_trend, test = "cd")

#Lagrange Multiplier (Breusch and Pagan) test for heteroskedasticity
plmtest(fe_GENpac_trend, type = "bp")

coeftest(fe_GENpac_trend,
         vcov = vcovHC(fe_GENpac_trend, type = "sss"))

#RE model for GENpac with time trend
re_GENpac_trend <- plm(GENpac ~ INDrec + INDred + INDimp + EPS +
                          GDPpc_log + POPden_log +
                          URB + EDU + gov_right1 +
                          time_trend + time_trend2,
                        data = df_model, model = "random")
summary(re_GENpac_trend)

coeftest(re_GENpac_trend,
         vcov = vcovHC(re_GENpac_trend, type = "sss"))

#hausman test of fe vs re GENpac with time trend
phtest(fe_GENpac_trend, re_GENpac_trend)


##---------------------------------------------------------------

#FE model for RECpac with time trend
fe_RECpac_trend <- plm(RECpac ~ INDrec + INDred + INDimp + EPS +
                          GDPpc_log + POPden_log +
                          URB + EDU + gov_right1 +
                          time_trend + time_trend2,
                        data = df_model,
                        model = "within", effect = "individual")

summary(fe_RECpac_trend)

#Wooldridge serial correlation test
pwartest(fe_RECpac_trend)

#Pesaran CD test for cross-sectional dependence
pcdtest(fe_RECpac_trend, test = "cd")

#Lagrange Multiplier (Breusch and Pagan) test for heteroskedasticity 
plmtest(fe_RECpac_trend, type = "bp")

coeftest(fe_RECpac_trend,
         vcov = vcovHC(fe_RECpac_trend, type = "HC3"))

#RE for RECpac with time trend
re_RECpac_trend <- plm(RECpac ~ INDrec + EPS +
                          GDPpc_log + POPden_log +
                          URB + EDU + gov_right1 +
                          time_trend + time_trend2,
                        data = df_model, model = "random")

summary(re_RECpac_trend)

coeftest(re_RECpac_trend,
         vcov = vcovHC(re_RECpac_trend, type = "HC3"))

#Hausman test of fe vs re RECpac with time trend
phtest(fe_RECpac_trend, re_RECpac_trend)

###-----------------------------------------------------------------
#FE model for RECpac_percent with time trend 

fe_RECpac_percent_trend <- plm(RECpac_percent ~ INDrec + INDred + INDimp + EPS +
                                 GDPpc_log + POPden_log +
                                 URB + EDU + gov_right1 +
                                 time_trend + time_trend2,
                               data = df_model,
                               model = "within", effect = "individual")

summary(fe_RECpac_percent_trend)

#Wooldridge serial correlation test
pwartest(fe_RECpac_percent_trend)

#Pesaran CD test for cross-sectional dependence
pcdtest(fe_RECpac_percent_trend, test = "cd")

#Lagrange Multiplier (Breusch and Pagan) test for heteroskedasticity 
plmtest(fe_RECpac_percent_trend, type = "bp")

coeftest(fe_RECpac_percent_trend,
         vcov = vcovHC(fe_RECpac_percent_trend, type = "sss"))

#RE for RECpac_percent with time trend
re_RECpac_percent_trend <- plm(RECpac ~ INDrec + INDred + INDimp + EPS +
                                 GDPpc_log + POPden_log +
                                 URB + EDU + gov_right1 +
                                 time_trend + time_trend2,
                               data = df_model, model = "random")
summary(re_RECpac_percent_trend)

coeftest(re_RECpac_percent_trend,
         vcov = vcovHC(re_RECpac_percent_trend, type = "sss"))

#Hausman test
phtest(fe_RECpac_percent_trend, re_RECpac_percent_trend)

###-----------------------------------------------------------------

#FE model for GENplas
fe_GENplas <- plm(GENplas ~ INDrec + INDred  + INDimp + EPS +
                     GDPpc_log + POPden_log +
                     URB + EDU + gov_right1,
                   data = df_model, model = "within", effect = "twoways")

summary(fe_GENplas)

#Wooldridge serial correlation test
pwartest(fe_GENplas)

#Pesaran CD test for cross-sectional dependence
pcdtest(fe_GENplas, test = "cd")

#Breusch and Pagan test for heteroskedasticity
plmtest(fe_GENplas, type = "bp")

#robust SEs (cluster by country)
coeftest(fe_GENplas, vcov = vcovHC(fe_GENplas, type = "sss"))

#RE model for generated plastic waste 

re_GENplas <- plm(GENplas ~ INDrec + INDred + INDimp + EPS +
                     GDPpc_log + POPden_log +
                     URB + EDU + gov_right1,
                   data = df_model, model = "random")

summary(re_GENplas)

coeftest(re_GENplas, vcov = vcovHC(re_GENplas, type = "sss"))

#hausman test
phtest(fe_GENplas, re_GENplas)


###-----------------------------------------------------------------
#FE model for GENplas_trend
fe_GENplas_trend <- plm(GENplas ~ INDrec + INDred  + INDimp + EPS +
                          GDPpc_log + POPden_log +
                          URB + EDU + gov_right1 +
                          time_trend + time_trend2,
                        data = df_model, 
                        model = "within", effect = "individual")

summary(fe_GENplas_trend)

#Wooldridge serial correlation test
pwartest(fe_GENplas_trend)

#Pesaran CD test for cross-sectional dependence
pcdtest(fe_GENplas_trend, test = "cd")

#Lagrange Multiplier (Breusch and Pagan) test for heteroskedasticity
plmtest(fe_GENplas_trend, type = "bp")

coeftest(fe_GENplas_trend,
         vcov = vcovHC(fe_GENplas_trend, type = "HC3"))

#RE model for GENplas with time trend
re_GENplas_trend <- plm(GENplas ~ INDrec + INDred + INDimp + EPS + 
                          GDPpc_log + POPden_log +
                          URB + EDU + gov_right1 +
                          time_trend + time_trend2,
                        data = df_model, model = "random")

summary(re_GENplas_trend)

coeftest(re_GENplas_trend,
         vcov = vcovHC(re_GENplas_trend, type = "HC3"))

#hausman test of fe vs re GENplas with time trend
phtest(fe_GENplas_trend, re_GENplas_trend)  


###-----------------------------------------------------------------
#FE model for RECplas_percent
fe_RECplas_percent <- plm(RECplas_percent ~ INDrec + INDred + INDimp + EPS + 
                             GDPpc_log + POPden_log +
                             URB + EDU + gov_right1,
                           data = df_model,
                           model = "within", effect = "twoways")
summary(fe_RECplas_percent)

#Wooldridge serial correlation test
pwartest(fe_RECplas_percent)

#Pesaran CD test for cross-sectional dependence
pcdtest(fe_RECplas_percent, test = "cd")

#Breusch and Pagan test for heteroskedasticity
plmtest(fe_RECplas_percent, type = "bp")

#robust SEs (cluster by country)
coeftest(fe_RECplas_percent, vcov = vcovHC(fe_RECplas_percent, type = "sss"))

#RE model for recycled plastic waste percentage
re_RECplas_percent <- plm(RECplas_percent ~ INDrec + INDred + INDimp + 
                             GDPpc_log + POPden_log +
                             URB + EDU + gov_right1,
                           data = df_model, model = "random")
summary(re_RECplas_percent)

coeftest(re_RECplas_percent, vcov = vcovHC(re_RECplas_percent, type = "sss"))

#hausman test
phtest(fe_RECplas_percent, re_RECplas_percent)

###-----------------------------------------------------------------

# FE model for RECplas_percent with time trend
fe_RECplas_percent_trend <- plm(RECplas_percent ~ INDrec + INDred + INDimp + EPS + 
                                    GDPpc_log + POPden_log +
                                    URB + EDU + gov_right1 +
                                    time_trend + time_trend2,
                                  data = df_model,
                                  model = "within", effect = "individual")
summary(fe_RECplas_percent_trend)

# Woldridge serial correlation test
pwartest(fe_RECplas_percent_trend)
# Pesaran CD test for cross-sectional dependence
pcdtest(fe_RECplas_percent_trend, test = "cd")
# Lagrange Multiplier (Breusch and Pagan) test for heteroskedasticity
plmtest(fe_RECplas_percent_trend, type = "bp")
coeftest(fe_RECplas_percent_trend,
         vcov = vcovHC(fe_RECplas_percent_trend, type = "sss"))

# RE for RECplas_percent with time trend
re_RECplas_percent_trend <- plm(RECplas_percent ~ INDrec + INDred + INDimp + EPS + 
                                    GDPpc_log + POPden_log +
                                    URB + EDU + gov_right1 +
                                    time_trend + time_trend2,
                                  data = df_model, model = "random")

summary(re_RECplas_percent_trend)

coeftest(re_RECplas_percent_trend,
         vcov = vcovHC(re_RECplas_percent_trend, type = "sss"))

# Hausman test
phtest(fe_RECplas_percent_trend, re_RECplas_percent_trend)


