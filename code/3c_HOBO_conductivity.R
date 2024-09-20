#### cleaning and assembling HOBO U-24 sensor conductivity & temperature data
### Jordan Zabrecky
## DATE

# This code reads in csv's of conductivity data from HOBO-U24 sensors saved 
# from the HOBOware software and removes any outliers or periods where 
# the sensor was pulled out of water and saves it to a new csv

#### (1) Loading libraries and HOBO data ####

## steal from old scripts on desktop
## maybe use dyplots??