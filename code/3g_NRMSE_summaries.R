#### Getting NRMSE summaries for each model
### Jordan Zabrecky
## last edited: 10.27.2025

## This code reads in NRMSEs for all predictions for a type of model and 
## summarizes them via mean & 95% confidence interval

#### (1) Loading libraries ####

# loading libraries
lapply(c("tidyverse", "plyr"), require, character.only = T)

# different categories of what we are predicting
predicting <- c("M_cover", "AC_cover", "M_atx", "AC_atx")

#### (2) Reading in NRMSE vectors ####

# empty list for all NRMSEs
NRMSE_list <- list()

# pull all saved vectors for each "predicting"
for(i in 1:length(predicting)) {
  NRMSE_list[[i]] <- ldply(list.files(path = paste("./data/predictive_models/", predicting[i], 
                                         "_models/NRMSE_vectors/", sep = ""), 
                            pattern = "NRMSE"),
                 function(filename) {
                   # load csv
                   file <- read.csv(paste("./data/predictive_models/", predicting[i], 
                                            "_models/NRMSE_vectors/", filename, sep = ""))
                   
                   # add information
                   file$model <- str_extract(filename, ".+?(?=_SFE)") # model is before SFE label
                   if(grepl("cover", filename)) {
                     file$site_reach <- strsplit(filename, "_")[[1]][4] # after third underscore if no cover
                   } else {
                     file$site_reach <- strsplit(filename, "_")[[1]][2] # after first underscore if no cover
                   }

                   return(file)
                 })
  
}

# add names to list
names(NRMSE_list) <- predicting

#### (3) Summarizing NRMSEs for each model ####

# summarize NRMSEs
NRMSE_list_summary <- lapply(NRMSE_list, function(x) x <- x %>%
                       dplyr::group_by(model) %>% 
                       dplyr::summarize(mean = mean(x),
                                        lower_ci = quantile(x, 0.025),
                                        upper_ci = quantile(x, 0.975)))

#### (4) Add in the null NRMSE for each model and save ####

# read in null csv and add it into list
for(i in 1:length(predicting)) {
  null <- read.csv(paste("./data/predictive_models/", predicting[i], "_models/NRMSE_vectors/null.csv", sep = "")) %>% 
    dplyr::summarize(model = "null",
                     mean = mean(x),
                     # no point in 95% CI as there are only five points!
                     lower_ci = mean(x),
                     upper_ci = mean(x))
  NRMSE_list_summary[[i]] <- rbind(NRMSE_list_summary[[i]], null)
}

# save csv's
lapply(names(NRMSE_list_summary), function(x) write.csv(NRMSE_list_summary[[x]], paste("./data/predictive_models/NRMSE_", 
                                                                                       x, ".csv", sep = ""),
                                                        row.names = FALSE))

# for supplemental figure showing each prediction, summarize by site_reach
NRMSE_list_site_reach<- lapply(NRMSE_list, function(x) x <- x %>%
                               dplyr::group_by(model, site_reach) %>% 
                               dplyr::summarize(mean = mean(x),
                                                lower_ci = quantile(x, 0.025),
                                                upper_ci = quantile(x, 0.975)))

# save csv's
lapply(names(NRMSE_list_site_reach), function(x) write.csv(NRMSE_list_site_reach[[x]], paste("./data/predictive_models/NRMSE_", 
                                                                                       x, "_by_site_reach.csv", sep = ""),
                                                        row.names = FALSE))

#### (5) Posterior Parameter Comparisons ####

## (a) comparing M cover vs. AC cover
# (need to remove sites and reaches omitted from AC cover)
divergent_models <- divergent_models <- data.frame(model_name = c("all_SFE-M-1S", "biochemical_SFE-M-1S", "biological_SFE-M-1S",
                                                                  "ecohydrological_SFE-M-1S")) %>% 
  mutate(model = sub(paste0("_SFE", ".*"), "", model_name),
         site_reach = sub(paste0(".*", "_"), "", model_name))

# How many M_cover models outperformed AC_cover models?
# (as determined by the % of NRMSE posterior where M cover NRMSE is lower than AC cover NRMSE)
test_a <- count(eval((NRMSE_list$M_cover %>% filter(!(model %in% divergent_models$model 
                                      & site_reach %in% divergent_models$site_reach)))$x <
       NRMSE_list$AC_cover$x))
# 107545 false and 357455 true
test_a$freq[which(test_a$x == "TRUE")] / length(NRMSE_list$AC_cover$x) # 76.87% of M models outperform AC models

# join to one dataframe
test_a_data <- rbind((NRMSE_list$AC_cover %>% mutate(predicting = "AC_cover")),
                       (NRMSE_list$M_cover %>% mutate(predicting = "M_cover") %>% 
                         filter(!(model %in% divergent_models$model 
                                  & site_reach %in% divergent_models$site_reach))))

# density plot
ggplot(data = test_a_data, aes(x = x, fill = predicting)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~model, ncol = 1) +
  theme_bw()

# density plot 2
ggplot(data = test_a_data, aes(x = x, fill = interaction(model, predicting))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("#FEF9BE", "#d6d093", "#afa869", "#9c9455", "#786f2e", "#665d1a", "#544c03", 
                       "#d9e7ff", "#c0d7ff", "#86a2d6", "#6180bb", "#3b5fa1", "#034087", "#033264")) +
  #facet_wrap(~model) +
  labs(title = "cover prediction") +
  theme_bw()

# density plot 3
ggplot(data = test_a_data, aes(x = x, fill = predicting)) +
  geom_density(alpha = 0.6) +
  #facet_wrap(~model) +
  theme_bw()

## (b) comparing M atx vs. AC atx

# How many AC_atx models outperformed M_atx models?
# (as determined by the % of NRMSE posterior where AC atx NRMSE is lower than M atx NRMSE)
test_b <- count(eval((NRMSE_list$AC_atx$x < NRMSE_list$M_atx$x)))
# 400633 false and 649367 true
test_b$freq[which(test_b$x == "TRUE")] / length(NRMSE_list$M_atx$x) # 61.84% of AC atx models outperform M atx models

# Compare with a Mann Whitney test
# join to one dataframe
test_b_data <- rbind(NRMSE_list$AC_atx %>% mutate(predicting = "AC_atx"),
                       NRMSE_list$M_atx %>% mutate(predicting = "M_atx"))

# density plot
ggplot(data = test_b_data, aes(x = x, fill = predicting)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~model) +
  theme_bw()

# add base model (as to not have 14+ categories in plot!)
test_b_data <- test_b_data %>% 
  mutate(model_base = str_remove(model, "_w_cover"))

# density plot 2
ggplot(data = test_b_data, aes(x = x, fill = interaction(model_base, predicting))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("#FEF9BE", "#d6d093", "#afa869", "#9c9455", "#786f2e", "#665d1a", "#544c03", 
                               "#d9e7ff", "#c0d7ff", "#86a2d6", "#6180bb", "#3b5fa1", "#034087", "#033264")) +
  #facet_wrap(~model) +
  labs(title = "atx prediction") +
  theme_bw()

## (c) comparing M atx w & w/o cover as a predictor

# add column for with and without cover
NRMSE_list$M_atx <- NRMSE_list$M_atx %>% mutate(w_cover = case_when(grepl("w_cover", model) ~ TRUE,
                                                                     TRUE ~ FALSE))
NRMSE_list$AC_atx <- NRMSE_list$AC_atx %>% mutate(w_cover = case_when(grepl("w_cover", model) ~ TRUE,
                                                                     TRUE ~ FALSE))

# How many M_atx models w/ cover outperform M_atx models w/o cover
# (as determined by the % of NRMSE posterior where M atx w/cover NRMSE is lower than M atx w/o cover NRMSE)
test_c <- count(eval((NRMSE_list$M_atx$x[which(NRMSE_list$M_atx$w_cover == TRUE)] < 
                        NRMSE_list$M_atx$x[which(NRMSE_list$M_atx$w_cover == FALSE)])))
# 233675 false and 291325 true
test_c$freq[which(test_c$x == "TRUE")] / length(NRMSE_list$M_atx$x
                                                [which(NRMSE_list$M_atx$w_cover == TRUE)]) 
# 55.49% of M models w/ cover outperform M models w/o cover

# density plot
ggplot(data = NRMSE_list$M_atx %>% mutate(base = case_when(grepl("cover", model) ~
                                                             str_extract(model, ".+?(?=_)"),
                                                           TRUE ~ model)), aes(x = x, fill = w_cover)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~base) +
  theme_bw()

# density plot 2.
ggplot(data = NRMSE_list$M_atx, aes(x = x, fill = interaction(model, w_cover))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("#FEF9BE", "#d6d093", "#afa869", "#9c9455", "#786f2e", "#665d1a", "#544c03", 
                               "#d9e7ff", "#c0d7ff", "#86a2d6", "#6180bb", "#3b5fa1", "#034087", "#033264")) +
  #facet_wrap(~model) +
  labs(title = "M atx prediction") +
  theme_bw()

## (d) comparing AC atx w/ & w/o cover as a predictor

# How many AC_atx models w/ cover outperform AC_atx models w/o cover
# (as determined by the % of NRMSE posterior where AC atx w/cover NRMSE is lower than AC atx w/o cover NRMSE)
test_d <- count(eval((NRMSE_list$AC_atx$x[which(NRMSE_list$AC_atx$w_cover == TRUE)] < 
                        NRMSE_list$AC_atx$x[which(NRMSE_list$AC_atx$w_cover == FALSE)])))
# 128565 false and 396435 true
test_d$freq[which(test_c$x == "TRUE")] / length(NRMSE_list$M_atx$x
                                                [which(NRMSE_list$M_atx$w_cover == TRUE)]) 
# 75.51% of AC models w/ cover outperform M models w/o cover

# density plot
ggplot(data = NRMSE_list$AC_atx %>% mutate(base = case_when(grepl("cover", model) ~
                                                              str_extract(model, ".+?(?=_)"),
                                                            TRUE ~ model)), aes(x = x, fill = w_cover)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~base) +
  theme_bw()

# density plot 2
ggplot(data = NRMSE_list$AC_atx, aes(x = x, fill = interaction(model, w_cover))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("#FEF9BE", "#d6d093", "#afa869", "#9c9455", "#786f2e", "#665d1a", "#544c03", 
                               "#d9e7ff", "#c0d7ff", "#86a2d6", "#6180bb", "#3b5fa1", "#034087", "#033264")) +
  #facet_wrap(~model) +
  labs(title = "AC atx prediction") +
  theme_bw()
