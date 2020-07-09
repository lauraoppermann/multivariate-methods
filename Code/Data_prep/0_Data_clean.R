setwd("C:/Users/Laura/Documents/Uni/MV/multivariate-methods")

#install.packages("tidyverse")
#install.packages("magrittr")

library("lubridate")
library("stringr")
library("dplyr")
library("magrittr")

df = read.csv("./Data/data.csv")

# formats "XX yrs XX mon" to Number of Years in total
get_year_months = function(vec) {
  vec = as.character(vec)
  splitted = stringr::str_split_fixed(vec,
                                      " ",
                                      2)
  Years = as.numeric(stringr::str_replace_all(splitted[, 1],
                                              "yrs",
                                              ""))
  Months = as.numeric(stringr::str_replace_all(splitted[, 2],
                                               "mon",
                                               ""))
  return(Years + (Months / 12))
}

#function to change cut-off year. Lubridate was changing 68 to 2068 not 1968
cut_off_year = function(x, year = 1921) {
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900 + m, 2000 + m)
  x
}

cleanUpData = function(df){
  ### Change dates and times to numerics
  
  # formats "DD-MM-YY" to Number of days since 01.01.1970
  date_var_names = list("Date.of.Birth", "DisbursalDate")
  
  #change dates to days
  for (name in date_var_names) {
    df[, name] = as.numeric(cut_off_year(lubridate::dmy(df[, name])))
  }
  
  # set Age and remove Disbursal & Date of Birth
  df['Age'] = round((df['DisbursalDate'] - df['Date.of.Birth']) / 365)
  df %<>%
    select(-c(DisbursalDate, Date.of.Birth))
  
  # format to numeric
  date_time_names = c("AVERAGE.ACCT.AGE", "CREDIT.HISTORY.LENGTH")
  df[, date_time_names] = sapply(df[, date_time_names], get_year_months)
  
  ### change types to factor
  to_factor = c(
    "branch_id",
    "supplier_id",
    "manufacturer_id",
    "Current_pincode_ID",
    "Employment.Type",
    "State_ID",
    "PERFORM_CNS.SCORE.DESCRIPTION",
    "Employee_code_ID"
  )
  
  df %<>%
    mutate_at(to_factor, factor)
  
  #drop unused (for now) columns (too many different ID's --> Need to find a solution)
  df %<>%
    select(-c(
      UniqueID,
      supplier_id,
      branch_id,
      Current_pincode_ID,
      Employee_code_ID
    ))
  
  # combine primary and secondary account information
  df[,'No.of.Accounts'] = df['PRI.NO.OF.ACCTS'] + df['SEC.NO.OF.ACCTS']
  df[,'PRI.Inactive.Accounts'] = df['PRI.NO.OF.ACCTS'] - df['PRI.ACTIVE.ACCTS']
  df[,'SEC.Inactive.Accounts'] = df['SEC.NO.OF.ACCTS'] - df['SEC.ACTIVE.ACCTS']
  df[,'Total.Inactive.Accounts'] = df['PRI.Inactive.Accounts'] + df['SEC.Inactive.Accounts']
  df[,'Total.Overdue.Accounts'] = df['PRI.OVERDUE.ACCTS'] + df['SEC.OVERDUE.ACCTS']
  df[,'Total.Current.Balance'] = df['PRI.CURRENT.BALANCE'] + df['SEC.CURRENT.BALANCE']
  df[,'Total.Sanctioned.Amount'] = df['PRI.SANCTIONED.AMOUNT'] + df['SEC.SANCTIONED.AMOUNT']
  df[,'Total.Disbursed.Amount'] = df['PRI.DISBURSED.AMOUNT'] + df['SEC.DISBURSED.AMOUNT']
  df[,'Total.Installment'] = df['PRIMARY.INSTAL.AMT'] + df['SEC.INSTAL.AMT']
  
  # drop unused Primary and Secondary Account info
  df %<>%
    select(-c(
      PRI.NO.OF.ACCTS,
      SEC.NO.OF.ACCTS,
      PRI.CURRENT.BALANCE,
      PRI.Inactive.Accounts,
      SEC.Inactive.Accounts,
      PRI.SANCTIONED.AMOUNT,
      SEC.DISBURSED.AMOUNT,
      PRI.ACTIVE.ACCTS, 
      PRI.OVERDUE.ACCTS,
      SEC.CURRENT.BALANCE,
      SEC.SANCTIONED.AMOUNT,
      SEC.OVERDUE.ACCTS,
      PRIMARY.INSTAL.AMT,
      SEC.INSTAL.AMT,
    ))
  
  return(df)
}

df = cleanUpData(df)

###save file
save(df, file = "./Code/Data_prep/preproc_data.Rdata")
