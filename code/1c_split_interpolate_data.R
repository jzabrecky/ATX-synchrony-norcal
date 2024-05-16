#### data visualization for miniDOT data cleaning
### Original code author unknown? (from Joanna Blaszczak)
## modified 3.2.24

# This supporting code helps split and interpolate data to match 
# 5 minute intervals of miniDOT data
# Modification involved changing output date time field to be named
# "date_time" rather than "DateTime" for consistency with miniDOT data

Create_Filled_TS <- function(x, samp_freq, parameterValue){
  # x is the data frame
  # samp_freq is the frequency you want (e.g., if you want 5 minutes, type in "5M")
  # parameterValue is what you are trying to fill
  
  R <- x
  
  ## Create a dataset with the appropriate time values that you want
  spread_ts <- function(x, samp_freq){
    re = regexec("([0-9]+)([A-Z])",samp_freq)[[1]]
    if(-1%in%re){
      stop("Please enter a correct string")
    }else{
      ml = attr(re,"match.length")
      nn = as.numeric(substr(samp_freq, re[2], ml[2]))
      uu = substr(samp_freq, re[3], ml[1])
      if(uu=="D"){td = 24*60*60*nn
      }else if(uu=="H"){td = 60*60*nn
      }else if(uu=="M"){td = 60*nn
      }else if(uu=="S"){td = nn
      }else{stop("Please enter a correct string")}
    }
    seq(x[1], x[length(x)], by=td)
  }
  
  
  # Generate a full timeseries at a given interval
  DateTimeNew <- spread_ts(R$date_time, samp_freq)
  xspread <- tibble(date_time=DateTimeNew)
  # Create a new table with NAs for missing time steps
  Rnew <- merge(R, xspread, by="date_time", all.y=TRUE)
  
  ## Use Approx
  Rnew$Filled_Var <- na.approx(Rnew[,parameterValue])
  
  return(Rnew)
}
