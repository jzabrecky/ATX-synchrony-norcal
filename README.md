# ATX-synchrony-norcal

This project contains the code for Zabrecky et al. "Predicting taxon-specific benthic cyanobacterial mat cover and anatoxin concentrations in northern California rivers".

All code scripts to complete analyses are found in the folder "code". All figures and code to create figures are found in the folder "figures". All data that can be publicly released are found in the folder "data". Note that dissolved oxygen data from the Salmon River was proprietary informtation that cannot be publicly shared. However, GPP estimates in the Salmon River produced from these analyses are publicly available within this release and the associated Environmental Data Initiative release (ID: edi.1974.3). 

Analyses code scripts follow this order:

1.  Modeling metabolism using *StreamMetabolizer*
2.  Processing field and lab data (benthic survey data, water chemistry data, anatoxin concentrations, and metabolism estimates)
3.  Predicting taxon-specific benthic cyanobacteria cover and anatoxin concentrations
4.  Quantifying the relative uncertainty contributions from parameter, process, and intial conditions error