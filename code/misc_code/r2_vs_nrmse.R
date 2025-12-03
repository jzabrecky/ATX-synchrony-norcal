

rsquared <- read.csv("./data/predictive_models/rsquared.csv")
nrmse <- rbind(read.csv("./data/predictive_models/NRMSE_AC_atx.csv") %>% 
                 mutate(predicting = "AC_atx"),
               read.csv("./data/predictive_models/NRMSE_M_atx.csv") %>% 
                 mutate(predicting = "M_atx"),
               read.csv("./data/predictive_models/NRMSE_M_cover.csv") %>% 
                 mutate(predicting = "M_cover"),
               read.csv("./data/predictive_models/NRMSE_AC_cover.csv") %>% 
                 mutate(predicting = "AC_cover")) %>% 
  filter(model != "null")

together <- left_join(rsquared, nrmse, by = c("predicting", "model"))

ggplot(data = together, aes(x = coef_of_deter_lm, y = mean, color = predicting)) +
  geom_point()
