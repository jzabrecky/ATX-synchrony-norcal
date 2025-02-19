#### Supplemental figures related to habitat occupied by benthic cyanobacteria
### Jordan Zabrecky
## last edited: 02.12.2025

# These figures show (1) the proportion of anatoxin congeners at each reach,
# (2) the difference between anatoxins normalized per organic matter vs. chl-a,
# (3) the difference in mat concentrations on our single day riffle experiment

#### (1) Loading libraries and anatoxins data ####

# loading libraries
lapply(c("tidyverse", "lubridate"), require, character.only = T)

# loading anatoxins data
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

# group by month, riffle_rapid, and site
summarized <- surveys %>% 
  dplyr::group_by(site, month, riffle_rapid) %>% 
  dplyr::summarize(num_micro = sum(Micro_pres),
                   num_anacyl = sum(Ana_Cyl_pres),
                   num_transects = length(transect)) %>%
  mutate(percent_micro = (num_micro / num_transects) * 100,
         percent_anacyl = (num_anacyl / num_transects) * 100)

##### (3) Making figures ####

# need to make month a character
summarized$month <- as.character(summarized$month)

# initial trial....
initial <- ggplot(data = summarized, aes(x = month, y = percent_micro, fill = riffle_rapid)) +
  geom_bar(position="dodge", stat = "identity") +
  facet_wrap(~site)
initial  

initial_ana <- ggplot(data = summarized, aes(x = month, y = percent_anacyl, fill = riffle_rapid)) +
  geom_bar(position="dodge", stat = "identity") +
  facet_wrap(~site)
initial_ana
