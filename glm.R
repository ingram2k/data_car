#document format
library(knitr) 
library(DT) 
#graphics
library(GGally) 
library(grid) 
library(ggpubr) 
library(ggExtra) 
#modeling
library(fitdistrplus) 
library(Hmisc)
library(caret) 
library(gbm) 
library(Amelia)
#general
library(tidyverse)
library(broom)
library(forcats)


car <- dataCar %>%
  mutate_at(c("clm", "agecat", "X_OBSTAT_", "veh_body"), as.factor) %>%
  #because ther are only a few roadsters, these are being grouped together as sports cars
  mutate(veh_body = as.character(veh_body),
         veh_body = as.factor(ifelse(veh_body %in% c("RDSTR", "CONVT"),
                                     yes = "SPRT", no = veh_body)))
car <- car %>%
  mutate(deductible = as.factor(ifelse(round(claimcst0, 0) %in% c(200, 354, 390, 345) 
                                       , "deductible","no_deductible")))
# 
# severity <- car %>%
#   filter(claimcst0 > 0) %>%
#   mutate(
#     avg_loss = claimcst0 / numclaims)

car <- car %>%
  mutate_if(is.factor, fct_infreq)

#avg_loss_cap <- quantile(severity$avg_loss, 0.95)

car$ID <- seq.int(nrow(car))


# severity %>%
#   select(large_loss) %>%
#   summary()

car.train<- subset(car, car$ID %in% dataCar.train.id$ID)

car.test<- subset(car, car$ID %in% dataCar.test.id$ID)


train_severity <- car.train %>% filter(claimcst0>0,claimcst0 / numclaims < avg_loss_cap)

train_severity <- train_severity %>% filter(deductible == "no_deductible")


# severity <- severity %>%
#   mutate(
#     avg_capped_losss = ifelse( avg_loss < avg_loss_cap, avg_loss, avg_loss_cap),
#     large_loss = as.factor(ifelse(avg_loss > avg_loss_cap, "Yes", "No"))) %>%
#   select(-clm, -X_OBSTAT_) # drop features which provide no additional information
# #

#using only the mean response
glm_baseline <- glm(
  formula = avg_capped_losss ~ 1,
  family = Gamma(link = "log"),
  data = train_severity,
  weights = train_severity$numclaims)
#using only the length of time of coverage
vars_1 <- "exposure"
glm_severity_1 <- glm(
  formula = avg_capped_losss ~ exposure,
  family = Gamma(link = "log"),
  data = train_severity,
  weights = train_severity$numclaims)
vars_2 <- "exposure, log(veh_value)"
glm_severity_2 <- glm(
  avg_capped_losss ~ exposure + log(veh_value + 0.001),
  family = Gamma(link = "log"),
  data = train_severity,
  weights = train_severity$numclaims)
vars_3 <- "exposure, log(veh_value), veh_age, gender"
glm_severity_3 <- glm(
  avg_capped_losss ~ exposure + log(veh_value + 0.001) + veh_age + gender,
  family = Gamma(link = "log"),
  data = train_severity,
  weights = train_severity$numclaims)

vars_4 <- "exposure, log(veh_value), numclaims, veh_age, gender"
glm_severity_4 <- glm(
  avg_capped_losss ~ log(veh_value + 0.001) + exposure + numclaims + veh_age + gender,
  family = Gamma(link = "log"),
  data = train_severity,
  weights = train_severity$numclaims)
vars_5 <- "veh_value, exposure, numclaims, veh_age, gender, area"
glm_severity_5 <- glm(
  avg_capped_losss ~ veh_value + exposure + numclaims + veh_age + gender + area,
  family = Gamma(link = "log"),
  data = train_severity,
  weights = train_severity$numclaims)

severity_glms <- list(glm_severity_1, glm_severity_2, glm_severity_3, glm_severity_4, glm_severity_5)

#Remove exposure



glm_severity_4a <- glm(
  avg_capped_losss ~ log(veh_value + 0.001) + numclaims + veh_age + gender,
  family = Gamma(link = "log"),
  data = train_severity,
  weights = train_severity$numclaims)

glm_sev_pred<-predict(glm_severity_4a,test_severity,type="response")

mean(glm_sev_pred)
sum(test_severity$claimcst0)/sum(test_severity$numclaims)

####################### Train GLM Frequency #######################



frequency_train_index <- createDataPartition(
  car$numclaims, times = 1, p = 0.7, list = FALSE) %>% as.vector()
train_frequency <- car %>%
  dplyr::slice(frequency_train_index)
train_frequency <- car %>%
  dplyr::slice(- frequency_train_index)

glm_severity_4a <- glm(
  avg_capped_losss ~ log(veh_value + 0.001) + numclaims + veh_age + gender,
  family = Gamma(link = "log"),
  data = train_severity,
  weights = train_severity$numclaims)