setwd("C:/Users/felix/Documents/GitHub/multivariate-methods")

#install.packages("tidyverse")
#install.packages("magrittr")

library("lubridate")
library("stringr")
library("dplyr")
library("magrittr")

df = read.csv("./Data/data.csv")


### Change dates and times to numerics

# formats "DD-MM-YY" to Number of days since 01.01.1970
date_var_names = list("Date.of.Birth","DisbursalDate")

#function to change cut-off year. Lubridate was changing 68 to 2068 not 1968
cut_off_year = function(x, year=1921){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

#change dates to days
for (name in date_var_names){
  df[, name] = as.numeric(
                  cut_off_year(
                    lubridate::dmy(df[,name])
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

df[,date_time_names] = sapply(df[,date_time_names], get_year_months)

### change types to factor 
to_factor = c("branch_id", 
              "supplier_id",
              "manufacturer_id",
              "Current_pincode_ID",
              "Employment.Type",
              "State_ID", 
              "PERFORM_CNS.SCORE.DESCRIPTION",
              "Employee_code_ID",
              "loan_default")

df %<>% 
  mutate_at(to_factor, factor)

#drop unused (for now) columns (too many different ID's --> Need to find a solution)

df %<>%
  select(-c(UniqueID,supplier_id, branch_id, 
            Current_pincode_ID, Employee_code_ID, MobileNo_Avl_Flag)
)


###save file
save(df, file="./Code/Data_prep/preproc_data.Rdata")




