library(tidyverse)
library(readxl)
library(writexl)

# New data from compstat
data_consol = read_csv("crsp_20240930-NYFED(in).csv")
unique(data_consol$permco)

Data_CRSP <- read_excel("New data from Crsp.xlsx")
Data_CRSP = Data_CRSP %>% rename(  cusip = CUSIP, ticker =`Ticker Symbol`, company_name = `Company Name`, 
                                   primary_exch= `Primary Exchange`, security_status=`Security Status`, 
                                   cusip_header =`CUSIP Header`)
unique(Data_CRSP$PERMCO)


Data_Compustat <- read_excel("New data from Compustat.xlsx") 

Data_Compustat <- Data_Compustat %>%
  rename(
    costat = `(costat) Active/Inactive Status Marker`,
    curcd = `(curcd) ISO Currency Code`,
    datafmt = `(datafmt) Data Format`,
    indfmt = `(indfmt) Industry Format`,
    consol = `(consol) Level of Consolidation`,
    cusip = `(cusip) CUSIP`,
    datadate = `(datadate) Data Date`,
    gvkey = `(gvkey) Global Company Key`,
    company_name = `(conm) Company Name`,
    ticker = `(tic) Ticker Symbol`,
    cik = `(cik) CIK Number`,
    total_assets = `(at) Assets - Total`,
    bkvlps = `(bkvlps) Book Value Per Share`,
    common_equity_total = `(ceq) Common/Ordinary Equity - Total`,
    common_shares_outstanding = `(csho) Common Shares Outstanding`
  )

unique(Data_Compustat$cusip)

Data_Compustat= Data_Compustat%>% mutate(total_assets_in_millions = total_assets * 1000000) 
Data_Compustat$year <- format(Data_Compustat$datadate, "%Y")

Data_Compustat_B = Data_Compustat %>% 
  filter(total_assets_in_millions >=100000000000)

unique(Data_Compustat_B$cusip)

Stress_test_data <- read_excel("Stress test - data collection_1.xlsx")
Stress_test_data = Stress_test_data %>% rename(company_name = `Bank Name`, year = `Year of Stress test / Number of Banks`)

unique(Stress_test_data$company_name)


Data_Compustat_B$year <- as.numeric(Data_Compustat_B$year)
Stress_test_data$year <- as.numeric(Stress_test_data$year)


library(stringr)

# Function to clean company names
clean_name <- function(x) {
  x %>%
    str_to_upper() %>%                         # make uppercase
    str_replace_all("&", "AND") %>%            # replace &
    str_replace_all("[[:punct:]]", "") %>%     # remove punctuation
    str_squish()                               # trim whitespace
} 

