#### Supplemental figures related to habitat occupied by benthic cyanobacteria
### Jordan Zabrecky
## last edited: 07.21.2025

# This figure shows the amount of 

#### (1) Loading libraries and anatoxins data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "ggtext", "cowplot"), require, character.only = T)

# loading survey data including each transect 
# (y if present within 7.5-m of transect where habitat was recorded)
surveys <- read.csv("./data/EDI_data_package/benthic_surveys.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  mutate(month = month(field_date)) %>% 
  mutate(year = year(field_date))

#### (2) Summarizing data ####

# replace "y" and "n" with numbers for presence within a transect
surveys$Micro_pres[surveys$Micro_pres == "y"] <- 1
surveys$Ana_Cyl_pres[surveys$Ana_Cyl_pres == "y"] <- 1
surveys$Micro_pres[surveys$Micro_pres == "n"] <- 0
surveys$Ana_Cyl_pres[surveys$Ana_Cyl_pres == "n"] <- 0

# convert column type to numeric
surveys$Micro_pres <- as.numeric(surveys$Micro_pres)
surveys$Ana_Cyl_pres <- as.numeric(surveys$Ana_Cyl_pres)

# make column with "y/1" or "n/0" to indicate presence within quadrat
surveys <- surveys %>% 
  mutate(Micro_quadrat = case_when(Microcoleus > 0 ~ 1,
                                   TRUE ~ 0),
         Anacyl_quadrat = case_when(Anabaena_Cylindrospermum > 0 ~ 1,
                                 TRUE ~ 0))

# group by month, riffle_rapid, and site and count number of taxa-specific
# observations
summarized <- surveys %>% 
  dplyr::group_by(site, month, riffle_rapid) %>% 
  dplyr::summarize(num_micro = sum(Micro_pres),
                   num_anacyl = sum(Ana_Cyl_pres),
                   quadrat_micro = sum(Micro_quadrat),
                   quadrat_anacyl = sum(Anacyl_quadrat))

# get total number of transects surveyed for each site and month
# regardless of habitat
total <- surveys %>% 
  dplyr::group_by(site, month) %>% 
  dplyr::summarize(total_transects = length(transect))

# left join the two
summarized <- left_join(summarized, total, by = c("site", "month"))

# calculate proportion of transects that have taxa-specific observations
summarized$prop_micro_transect <- summarized$num_micro / summarized$total_transects
summarized$prop_micro_quadrat <- summarized$quadrat_micro / summarized$total_transects
summarized$prop_anacyl_transect <- summarized$num_anacyl / summarized$total_transects
summarized$prop_anacyl_quadrat <- summarized$quadrat_anacyl / summarized$total_transects

# remove NAs for when no habitat was recorded
summarized <- na.omit(summarized)

##### (3) Making figures ####

# set universal theme for all plots
theme_set(theme_bw() + theme(legend.position = "bottom",
                             panel.grid.minor = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1),
                             axis.text.x = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.text.y = element_text(size = 10), strip.background = element_blank(),
                             plot.title = element_markdown(hjust = 0.5)))

# need to make month a character
summarized$month <- as.character(summarized$month)

# separate out micro data and anabaena data (so site without taxa does not plot!)
micro_summarized <- summarized %>% 
  filter(site != "RUS")
anacyl_summarized <- summarized %>% 
  # while one reach had a single anabaena siting, it was not in quadrat
  filter(site != "SAL")

## Trying with survey presence/absence

# microcoleus
presence_micro <- ggplot(data = micro_summarized, aes(x = month, y = prop_micro_transect, fill = riffle_rapid)) +
  geom_bar(position="stack", stat = "identity") +
  scale_x_discrete(labels = c("June", "July", "Aug", "Sept")) +
  scale_fill_manual("Habitat:", labels = c("Pool or Run", "Riffle or Rapid"),
                    values = c("#ebdf38", "#62a7f8")) +
  facet_wrap(~site, labeller =  as_labeller(c(`SAL` = "Salmon River", 
                                              `SFE-M`= "South Fork Eel River at Miranda",
                                              `SFE-SH` = "South Fork Eel River at Standish Hickey"))) +
  labs(x = NULL, y = "Proportion of Transects Present", title = "*Microcoleus* Presence")
presence_micro

# anabaena/cylindrospermum
presence_ana <- ggplot(data = anacyl_summarized, aes(x = month, y = prop_anacyl_transect, fill = riffle_rapid)) +
  geom_bar(position="stack", stat = "identity") +
  scale_x_discrete(labels = c("June", "July", "Aug", "Sept")) +
  scale_fill_manual("Habitat:", labels = c("Pool or Run", "Riffle or Rapid"),
                    values = c("#ebdf38", "#62a7f8")) +
  facet_wrap(~site, labeller =  as_labeller(c(`RUS` = "Russian River", 
                                              `SFE-M`= "South Fork Eel River at Miranda",
                                              `SFE-SH` = "South Fork Eel River at Standish Hickey"))) +
  labs(x = NULL, y = "Proportion of Transects Present", title = "*Anabaena/Cylindrospermum* Presence")
presence_ana

# I think that since this presence/absence includes up to ~7.5-m
# out from where quadrat was placed and habitat was recorded
# so habitat could have changed and overall it sends a
# less clear message than conveyed than using quadrat presence/absence

## Trying with quadrat presence absence data

# factor to get preferred ordering for ggplot
micro_summarized$site_f <- factor(micro_summarized$site, levels = c("SFE-M", "SFE-SH",
                                                                    "SAL"))
anacyl_summarized$site_f <- factor(anacyl_summarized$site, levels = c("SFE-M", "SFE-SH",
                                                                      "RUS"))

# microcoleus
quadrat_micro <- ggplot(data = micro_summarized, aes(x = month, y = prop_micro_quadrat, fill = riffle_rapid)) +
  geom_bar(position="stack", stat = "identity") +
  scale_x_discrete(labels = c("June", "July", "Aug", "Sept")) +
  scale_y_continuous(limits = c(0, 0.4)) +
  scale_fill_manual("Habitat:", labels = c("Pool or Run", "Riffle or Rapid"),
                    values = c("#ebdf38", "#62a7f8")) +
  facet_wrap(~site_f, labeller =  as_labeller(c(`SAL` = "Salmon River", 
                                              `SFE-M`= "South Fork Eel River Lower",
                                              `SFE-SH` = "South Fork Eel River Upper"))) +
  labs(x = NULL, y = NULL, title = "*Microcoleus*")
quadrat_micro

# anabaena
quadrat_anacyl <- ggplot(data = anacyl_summarized, aes(x = month, y = prop_anacyl_quadrat, fill = riffle_rapid)) +
  geom_bar(position="stack", stat = "identity") +
  scale_x_discrete(labels = c("June", "July", "Aug", "Sept")) +
  scale_y_continuous(limits = c(0, 0.4)) +
  scale_fill_manual("Habitat:", labels = c("Pool or Run", "Riffle or Rapid"),
                    values = c("#ebdf38", "#62a7f8")) +
  facet_wrap(~site_f, labeller =  as_labeller(c(`RUS` = "Russian River", 
                                              `SFE-M`= "South Fork Eel River Lower",
                                              `SFE-SH` = "South Fork Eel River Upper"))) +
  labs(x = NULL, y = NULL, title = "*Anabaena* / *Cylindrospermum*")
quadrat_anacyl

# putting togther plots
final <- plot_grid(quadrat_micro, quadrat_anacyl, ncol = 1)
final

# save
ggsave("./figures/sfig_habitats_notfinal.tiff", dpi = 600, 
       width=16, height=14, unit="cm")

#### (4) Misc. Q ####

## Q: how much of riffle is covered by Microcoleus?
q <- surveys %>% 
  filter(site == "SFE-M" & riffle_rapid == "y") %>% 
  dplyr::group_by(field_date) %>% 
  dplyr::summarize(micro_cover = mean(Microcoleus))
