bacteria_sun<-data.frame(bacteria=c(1,1,1,1,1,0,0,0,0,0)
                         ,sun = c(1,2,3,4,5,1,2,3,4,5)
                         ,sb = c(1,2,3,4,5,0,0,0,0,0)*3.2 #add in the interaction
                         ,heights = c(51.4,63.6,75.8,88,100.2,44,53,62,71,80
                         )
                         )
bacteria_sun_test<-data.frame(bacteria=c(1,0)
                         ,sun = c(6,6)
                         ,sb = c(6,0)*3.2
                         ,heights = c(112.4,89
                         )
)
model<-lm(heights~bacteria+sun,data = bacteria_sun)

model1<-lm(heights~bacteria+sun+bacteria:sun,data = bacteria_sun)

model2<-lm(heights~bacteria+sun+sb,data = bacteria_sun)

bac_pred<-predict(model,bacteria_sun)

bacteria_sun<-bacteria_sun %>% mutate (pred=bac_pred)

bacteria_sun %>% group_by(bacteria) %>% 
  summarise (
    
    heights = sum(heights) /5
    ,heights_pred = sum(pred) /5
  )

#under predict sun , over predict not sun
bacteria_sun_test %>% mutate (pred=predict(model,bacteria_sun_test))
bacteria_sun_test %>% mutate (pred=predict(model2,bacteria_sun_test))

#