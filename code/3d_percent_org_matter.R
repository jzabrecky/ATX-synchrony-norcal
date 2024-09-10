#### calculating percent organic matter for TM and TAC samples
### Jordan Zabrecky
## last updated 09.10.2024

# This code calculates percent organic matter for TM (targeted-Microcoleus) and
# TAC (targeted-Anabaena and Cylindrospermum) using tin weight, dry weight,
# and combusted weight obtained day to week after sampling at UC-B Angelo Reserve

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

# reading in weights measured at UC-B Angelo Reserve Lab
data <- ldply(list.files(path = "./data/field_and_lab/raw_data/", 
                         pattern = "angelo"), function(filename) {
                           d <- read.csv(paste("data/field_and_lab/raw_data/", filename, sep = ""))
                           d$field_date <- mdy(d$field_date)
                           return(d)
                         })

#### (2) Calculating percent organic matter ####

# note: had two samples (6/29/2022 SFE-M-3 and SFE-M-4 that molded before we 
# had a dessicate container while we were waiting for oven use
# will likely just use SFE-M-1S weight for these

# calculate dry weight (both organic and inorganic matter) without tin weight
data$dry_sample_g <- data$dry_sample_tin_g - data$tin_g

# calculate AFDM (ash-free dry mass or organic matter)
data$afdm_g <- data$dry_sample_tin_g - data$combusted_sample_tin_g

# calculate percent organic matter (percent of matter that is organic/combusted)
data$per_org_matter <- data$afdm_g / data$dry_sample_g

# dealing with triplicate
# filter for triplicate; summarize by date
# remove tripliate and put summary into final data-frame
# order by date; (and then site reach if needed)
# need to fix this; guy next to me is making me feel uncomfortable
triplicates <- data %>% 
  filter(triplicate == "y") %>% 
  dplyr::group_by(field_date, site_reach, site, reach, sample_type) %>% 
  dplyr::summarize(mean = mean(per_org_matter),
         sd = sd(per_org_matter),
         rsd = sd * 100 / mean) %>% 
  ungroup() %>% 
  distinct()

# look at triplicate results
view(triplicates) # RSD max is 16%; only 5 > 10%
mean(triplicates$rsd) # average is 5.6%

# take dataset and select for columns we care about before merging
triplicates_final <- triplicates %>% 
  dplyr::rename(per_org_matter = mean) %>% 
  select(field_date, site_reach, site, reach, sample_type, per_org_matter)

# trying to put triplicates into normal dataset
final <- data %>% 
  filter(triplicate == "n") %>% 
  select(field_date, site_reach, site, reach, sample_type, per_org_matter)

# add processed/averaged triplicates back in
final <- rbind(final, triplicates_final)

# checking length of data frame (aka no sample left behind!)
# final dataset should be length of original dataset minus 2/3 the length of the averaged triplicate dataset
eval(nrow(final) == (nrow(data)-(2*nrow(triplicates)))) # yay!

# very last thing- addressing samples that molded, we will just use the percent org matter
# taken on the same date downstream at SFE-M-1S but keep an eye on it when we calculate ATX / org matter
final$per_org_matter[2] <- final$per_org_matter[1]
final$per_org_matter[3] <- final$per_org_matter[1]

# save csv output
write.csv(final, "./data/field_and_lab/cyano_percent_OM.csv")
