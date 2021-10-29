xgb.importance(model=model_freq)


cnts_pred_train<-predict(model_freq,features_train_freq,type="response")*pre.train_freq$exposure

sum(cnts_pred_train)

sum(pre.train_freq$exposure)

surr_data<-data.frame(features_train_freq,cnts_pred_train,exposure=pre.train_freq$exposure
                      ,actual_cnts=label_train_freq
                      )

surrogate_model<- glm(
  cnts_pred_train~ log(veh_value + 0.001) + veh_body_CONVT + veh_body_HBACK+
    veh_body_MCARA+ veh_body_PANVN+ veh_body_RDSTR+veh_body_SEDAN+
    veh_body_STNWG+veh_body_TRUCK+veh_body_UTE+veh_age+ gender_F+ gender_M+ area_A+
    area_B+ area_C+ area_D +area_E +area_F+ agecat,
  family = poisson(link = "log"),
  data = surr_data,
  offset = log(surr_data$exposure))


surrogate_model_1<- glm(
  cnts_pred_train~ veh_body_CONVT ,
  family = poisson(link = "log"),
  data = surr_data,
  offset = log(surr_data$exposure))

surr_data%>%group_by(veh_body_CONVT)%>%summarise(mean = mean(actual_cnts))

test_surr_1<-predict(surrogate_model_1,surr_data,type="response")

head(test_surr_1)
plot(surr_data$veh_value,surr_data$actual_cnts)

head(surr_data)


surr_pred<-predict(surrogate_model,surr_data,type="response")

y_xgb_mean<-mean(cnts_pred_train)

1-sum((surr_pred-surr_data$cnts_pred_train)^2)/sum((surr_data$cnts_pred_train-y_xgb_mean)^2)

