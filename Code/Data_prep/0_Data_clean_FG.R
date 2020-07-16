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

#Aggregate primary and secondary accounts:

df %<>%
  mutate(N_Age = 49 - Date.of.Birth/365.25, # transform to get age in 2019
         N_Loan_Date = DisbursalDate - 17744, #Start counting days from 1st day of data
         N_Loans_Tot = PRI.NO.OF.ACCTS + SEC.NO.OF.ACCTS,
         N_Loans_Active = PRI.ACTIVE.ACCTS + SEC.ACTIVE.ACCTS,
         N_Loans_Overdue = PRI.OVERDUE.ACCTS + SEC.OVERDUE.ACCTS,
         Q_Balance = ifelse(PRI.CURRENT.BALANCE + SEC.CURRENT.BALANCE>0, PRI.CURRENT.BALANCE + SEC.CURRENT.BALANCE, 0 ), #truncate at 0 
         Q_Loan_Hist = PRI.DISBURSED.AMOUNT + SEC.DISBURSED.AMOUNT,
         Q_Month_Pay =  ifelse( (PRIMARY.INSTAL.AMT + SEC.INSTAL.AMT) > 0.5*(Q_Balance + disbursed_amount), 
                                NA ,
                                PRIMARY.INSTAL.AMT + SEC.INSTAL.AMT) , #Incoherent monthly payment values added as Missing Values. Assumption: AT least two months to pay
         
         B_Exist_Sec = 1*(SEC.NO.OF.ACCTS > 0) )

#Add features:
df %<>%
  mutate( N_Loans_Paid = N_Loans_Tot - N_Loans_Active, 
          Q_Loans_Paid_To_Tot = (N_Loans_Tot - N_Loans_Active + 1 )/(N_Loans_Tot + 1 ),
          Q_Act_Debt_To_Total = ifelse(
                                  (Q_Balance  + disbursed_amount )/ (Q_Loan_Hist + disbursed_amount)<= 1 , 
                                  (Q_Balance  + disbursed_amount )/ (Q_Loan_Hist + disbursed_amount),
                                  0), #truncate at 1. Sometimes history larger then balance + disbursed amount --> issues in the data
          
          Q_Loan_to_Balance = disbursed_amount / (Q_Balance + disbursed_amount),
)

#Change formats:
df %<>%
  mutate( C_Id_Type = case_when( Aadhar_flag == 1 ~ "Aadhar",
                                 PAN_flag == 1 ~ "PAN",
                                 VoterID_flag == 1 ~ "Voter_ID",
                                 Driving_flag == 1 ~ "Driv_Lic",
                                 Passport_flag == 1 ~ "Passport"))


### change types to factor 
to_factor = c("branch_id", 
              "supplier_id",
              "manufacturer_id",
              "Current_pincode_ID",
              "Employment.Type",
              "State_ID", 
              "PERFORM_CNS.SCORE.DESCRIPTION",
              "Employee_code_ID",
              "loan_default",
              "C_Id_Type")

df %<>% 
  mutate_at(to_factor, factor)


#drop for sure
df %<>%
  dplyr::select(-c(PRI.NO.OF.ACCTS , 
            SEC.NO.OF.ACCTS, 
            PRI.ACTIVE.ACCTS,  
            SEC.ACTIVE.ACCTS,
            PRI.SANCTIONED.AMOUNT,  
            SEC.SANCTIONED.AMOUNT,
            PRI.OVERDUE.ACCTS,
            SEC.OVERDUE.ACCTS,
            PRI.CURRENT.BALANCE,
            SEC.CURRENT.BALANCE,
            PRI.DISBURSED.AMOUNT,
            SEC.DISBURSED.AMOUNT,
            PRIMARY.INSTAL.AMT ,
            SEC.INSTAL.AMT,
            SEC.NO.OF.ACCTS,
            Q_Loan_Hist,
            asset_cost,
            UniqueID,
            Aadhar_flag, 
            PAN_flag,
            VoterID_flag,
            Driving_flag,
            Passport_flag,
            MobileNo_Avl_Flag,
            DisbursalDate,
            Date.of.Birth)
  )

df %<>%
  dplyr::rename(
         Target_def = loan_default,
         Q_this_loan = disbursed_amount,
         Q_loan_to_value = ltv,
         C_Job = Employment.Type,
         Q_Score = PERFORM_CNS.SCORE,
         N_recent_loans = NEW.ACCTS.IN.LAST.SIX.MONTHS,
         N_recent_defaults = DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS,
         Q_AVG_loan_Age = AVERAGE.ACCT.AGE,
         Q_Credit_hist_length = CREDIT.HISTORY.LENGTH,
         N_Loan_Applis = NO.OF_INQUIRIES,
         C_Score_Desc = PERFORM_CNS.SCORE.DESCRIPTION,
         C_State_id = State_ID,
         C_brand_id = manufacturer_id,
         C_supplier_id = supplier_id, 
         C_branch_id = branch_id, 
         C_Employee_id = Employee_code_ID,
         C_Current_pincode_id = Current_pincode_ID
         ) 


####reorder columns:

df %<>%
  dplyr::select(
    Target_def,
    Q_this_loan ,
    Q_loan_to_value,
    Q_Score ,
    Q_Balance ,
    Q_Month_Pay ,
    Q_AVG_loan_Age,
    Q_Credit_hist_length ,
    N_recent_loans,
    N_recent_defaults,
    B_Exist_Sec ,
    N_Loans_Paid ,
    Q_Loans_Paid_To_Tot ,
    Q_Act_Debt_To_Total ,
    Q_Loan_to_Balance,
    N_Loan_Applis,
    N_Age ,
    N_Loan_Date,
    N_Loans_Tot ,
    N_Loans_Active ,
    N_Loans_Overdue,
    C_Job ,
    C_Score_Desc,
    C_Id_Type,
    C_State_id ,
    C_brand_id,
    C_supplier_id,
    C_branch_id,
    C_Employee_id ,
    C_Current_pincode_id)
 

###############################################################################################
#### Compute Weight of evidence (for dimension reduction of high cardinality factor variables):
###############################################################################################

WOE_Vars = c("C_State_id","C_brand_id","C_supplier_id","C_branch_id", "C_Employee_id", "C_Current_pincode_id" )  

library("InformationValue")
require(plyr)

# function for assigning WOE value to test data 
Assign_WOE_to_test = function(var_train, var_test, Target){
WOEtable = InformationValue::WOETable(var_train, Target)
var_test = mapvalues(var_test, 
                     from=WOEtable$CAT, 
                     to= WOEtable$WOE)
return(var_test)
}


### Use an additional data set for WOE estimation (other than train/ val / test)####


n = 233154
WOE_est_ind = sample(1:n, 70000)
Non_WOE_ind = c(1:n)[-c(WOE_est_ind)]
df_WOE_est = df[WOE_est_ind, ]
df_rest   = df[Non_WOE_ind, ]

#assign WOE values computed in  WOE in df_WOE_est to df_rest

df_rest$C_State_id = Assign_WOE_to_test(df_WOE_est$C_State_id,df_rest$C_State_id, df_WOE_est$Target_def )
df_rest$C_brand_id = Assign_WOE_to_test(df_WOE_est$C_brand_id,df_rest$C_brand_id,df_WOE_est$Target_def )
df_rest$C_supplier_id = Assign_WOE_to_test(df_WOE_est$C_supplier_id,df_rest$C_supplier_id,df_WOE_est$Target_def )
df_rest$C_branch_id = Assign_WOE_to_test(df_WOE_est$C_branch_id,df_rest$C_branch_id,df_WOE_est$Target_def )
df_rest$C_Employee_id =Assign_WOE_to_test(df_WOE_est$C_Employee_id,df_rest$C_Employee_id,df_WOE_est$Target_def )
df_rest$C_Current_pincode_id =Assign_WOE_to_test(df_WOE_est$C_Current_pincode_id,df_rest$C_Current_pincode_id,df_WOE_est$Target_def)

#change WOE's from factor to numeric 
df_rest %<>%
  dplyr::mutate_at(WOE_Vars ,
                   function(x) as.numeric(as.character(x)))


#replace missings that were created in the process
df_rest[, WOE_Vars] = tidyr::replace_na(df_rest[, WOE_Vars], list(  "C_State_id"=0,
                                                                                  "C_brand_id"=0,
                                                                                  "C_supplier_id"=0,
                                                                                  "C_branch_id"=0, 
                                                                                  "C_Employee_id"=0,
                                                                                  "C_Current_pincode_id"=0))

##Split df_rest into data for training and a final test set
test_ind = sample(1:nrow(df_rest), 0.3*nrow(df_rest))
df_test = df_rest[test_ind , ]
df_train_val = df_rest[-test_ind, ]

#df %<>%  dplyr::select(-c(C_State_id, C_brand_id,C_supplier_id, C_branch_id, C_Employee_id, C_Score_Desc))
###save file
save(df_train_val, file="./Code/Data_prep/train_val_data.Rdata")
save(df_test, file="./Code/Data_prep/test_data.Rdata")
save(df, file="./Code/Data_prep/total_data.Rdata")
rm(list=ls())

