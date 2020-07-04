install.packages("tidyverse")
library("lubridate")
library("stringr")

setwd("C:/Users/felix/Documents/GitHub/multivariate-methods")

data = read.csv("./Data/data.csv")


### Change dates and times to numerics

# formats "DD-MM-YY" to Number of days since 01.01.1970
date_var_names = list("Date.of.Birth","DisbursalDate")

#function to change cut-off year. Lubridate was changing 68 to 2068 not 1968
cut_off_year = function(x, year=1910){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

#change dates to days
for (name in date_var_names){
  data[, name] = as.numeric(
                  cut_off_year(
                    lubridate::dmy(data[,name])
                    )
)
}

# formats "XX yrs XX mon" to Number of Years in total
date_time_names = c("AVERAGE.ACCT.AGE", "CREDIT.HISTORY.LENGTH" )

get_year_months = function(vec){
  vec = as.character(vec)
  splitted = stringr::str_split_fixed(vec, 
                                      " ",
                                      2)
  Years = as.numeric(stringr::str_replace_all(splitted[,1],
                                              "yrs",
                                              "")
  )
  Months = as.numeric(stringr::str_replace_all(splitted[,2],
                                               "mon",
                                               "")
  )
  return(Years + (Months/12))
}

data[,date_time_names] = sapply(data[,date_time_names], get_year_months)




