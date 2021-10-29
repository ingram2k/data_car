dataCar %>% group_by(veh_body) %>% 
  summarise (exposure = sum(exposure) , veh_value = mean(veh_value)
             ,numclaims = sum(numclaims) , claimcst0 = sum(claimcst0)
             ,freq = sum(numclaims) / sum(exposure)
             ,sev = sum(claimcst0) / sum(numclaims)
             ,PP = sum(claimcst0) /sum(exposure)
             )


pre.test_freq <- pre.test_freq %>% mutate(pred_claim_cst = pure_prem_pred*pre.test_freq$exposure)

pre.test_freq.1<- inner_join(pre.test_freq , dataCar , by = "ID")


pre.test_freq.1 %>% group_by(veh_body_SEDAN) %>% 
  summarise (
             PP = sum(claimcst0.x) /sum(exposure.x)
             ,PP_PRED = sum(pred_claim_cst) /sum(exposure.x)
  )

pre.test_freq.1 %>% group_by(gender_F) %>% 
  summarise (
    PP = sum(claimcst0.x) /sum(exposure.x)
    ,PP_PRED = sum(pred_claim_cst) /sum(exposure.x)
  )

pre.test_freq.1 %>% group_by(gender) %>% 
  summarise (
    
    PP = sum(claimcst0.x) /sum(exposure.x)
    ,PP_PRED = sum(pred_claim_cst) /sum(exposure.x)
  )



pre.test_freq.1 %>% group_by(veh_body) %>% 
  summarise (
    exposure.x = sum(exposure.x)
    ,PP = sum(claimcst0.x) /sum(exposure.x)
    ,PP_PRED = sum(pred_claim_cst) /sum(exposure.x)
  )