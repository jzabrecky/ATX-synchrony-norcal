#### plotting data to check timing

## Reading in model input data (from starting working directory)
model_inputs <- ldply(list.files(path = "./data/metab_model_inputs/", pattern = "modelinputs"), function(filename) {
  d <- read.csv(paste("data/metab_model_inputs/", filename, sep = ""))
  return(d)
})

# convert solar.time to POSIXct class
model_inputs$solar.time <- as.POSIXct(model_inputs$solar.time, format = "%Y-%m-%d %H:%M:%S")

# separating into a list based on site/year
inputs_prepped <- split(model_inputs, model_inputs$site_year)

ggplot(data = inputs_prepped$salmon_2023, aes(x = solar.time, y = DO.obs)) +
  geom_line(color = "blue") +
  geom_line(data = inputs_prepped$salmon_2023_karuk, aes(x = solar.time, y = DO.obs, color = "purple")) +
  scale_x_datetime(limits = as_datetime(c("2023-08-29 00:00:00", "2023-9-01 00:00:00")))

# 2022 data all lines up well...  