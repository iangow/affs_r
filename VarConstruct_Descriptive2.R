

install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)

install.packages("openxlsx")
library("openxlsx")

install.packages("ggplot2")
library("ggplot2")

install.packages("modelsummary")
library(modelsummary)

install.packages("fixest")
library(fixest)


ROEData2023 <- read.csv("C:/Users/mpinnuck/OneDrive - The University of Melbourne/New folder/My Documents/AusData/BenchData/RData/FatData2023x.csv")

#create variables necessary for lags

ROEData2023 <-ROEData2023  %>% mutate (totaldebt = ifelse(is.na(dlc), 0, dlc) + ifelse(is.na(dltt), 0, dltt)+ifelse(is.na(pstk), 0, pstk) )
ROEData2023 <-ROEData2023  %>% mutate (financialassets = ifelse(is.na(che), 0, che) + ifelse(is.na(ivao), 0, ivao) )
ROEData2023 <-ROEData2023  %>% mutate (capital = totaldebt + ifelse(is.na(ceq), 0, ceq) + ifelse(is.na(mibt), 0, mibt))
ROEData2023 <-ROEData2023  %>% mutate (invcapital = capital-ifelse(is.na(che), 0, che))
ROEData2023 <-ROEData2023  %>% mutate (groupequity = ceq +ifelse(is.na(mibt), 0, mibt))

##create lags by company name and year

ROEData2023 <-ROEData2023  %>%
  group_by(gvkey) %>%
  arrange(datadate) %>%
  mutate (l1_at = lag(at),
          l1_ceq = lag(ceq),
          l1_sale  = lag( sale ),
          l1_invt = lag(invt),
          l1_rectr = lag(rectr),
          l1_capital = lag(capital),
          l1_invcapital = lag(invcapital),
          l1_ap=lag(ap),
          l1_capx = lag(capx),
          l1_totaldebt = lag(totaldebt),
          l1_oiadp = lag(oiadp))%>%
  ungroup()

#create profitability variables
ROEData2023 <-ROEData2023  %>% mutate(aveceq = ((ceq +l1_ceq)/2),
                                      roe = ifelse(aveceq > 0, ib/aveceq,NA),
                                      xint_atax = ifelse(is.na(xint), 0, xint*0.7),
                                      nbc = xint_atax +ifelse(is.na(dvp), 0, dvp)   ,
                                      nbc_rate = (nbc)/((totaldebt+l1_totaldebt)/2),
                                      groupprofit =ib+ifelse(is.na(mii), 0,mii )-ifelse(is.na(dvp), 0, dvp),
                                      operating_profit = groupprofit +nbc,
                                      operating_profit1 = groupprofit +nbc -ifelse(is.na(idit), 0, idit*0.7),
                                      avecapital = ((capital +l1_capital)/2),
                                      roc = ifelse(avecapital > 0,operating_profit/avecapital,NA),
                                      aveinvcapital = ((invcapital +l1_invcapital)/2),
                                      roic = ifelse(aveinvcapital > 0,oiadp/aveinvcapital,NA),
                                      salegr =ifelse (sale > 0 & l1_sale > 0, (sale/l1_sale)-1,NA),
                                      capxgr=(capx/l1_capx)-1,
                                      dayreceivable1 = ifelse(rectr > 0 & sale >0, rectr/(sale/365),NA),
                                      daysstock1 = ifelse(invt> 0 & cogs>0, invt/(cogs/365), NA),
                                      dayreceivable2 = ifelse(rectr > 0 & sale>0, ((rectr+l1_rectr)/2)/(sale/365),NA),
                                      daysstock2 = ifelse(invt> 0 &cogs > 0, ((invt+l1_invt)/2)/(cogs/365), NA),
                                      daysap = ifelse(ap> 0 & cogs > 0, ((ap+l1_ap)/2)/(cogs/365), NA),

                                      np_margin = ifelse(sale > 0, operating_profit/sale,NA),
                                      gp_margin = ifelse(sale> 0 & cogs > 0, (sale-cogs)/sale,NA),
                                      ebitda_margin = ifelse(sale> 0, oibdp/sale,NA),
                                      ebit_margin = ifelse(sale> 0, oiadp/sale,NA),
                                      ebit_post = ifelse(oiadp> 0, 1,0),
                                      leverage = totaldebt/groupequity,
                                      asset_turnover1 = sale/((at +l1_at)/2),
                                      asset_turnover2 = ifelse(avecapital > 0,sale/avecapital,NA),
                                      pbt = groupprofit + txt,
                                      etr = ifelse(pbt > 0 & txt > 0, txt / pbt, NA),
                                      depr_rate = ifelse(ppegt>0,dfxa/ppegt,NA),
                                      avgageppe =ifelse(dfxa>0, dpact/dfxa,NA),
                                      chg_oiadp = oiadp -l1_oiadp,
                                      perc_chgoiadp =ifelse(l1_oiadp!=0, chg_oiadp/abs(l1_oiadp),NA),
                                      abs_perc_chgoiadp = abs(perc_chgoiadp),
                                      abs_sales_gr =abs(salegr),
                                      op_exp = sale-oiadp,
                                      perch_chg_at = ifelse(l1_at!=0, (at/l1_at)-1, NA),
                                      intan_intense= ifelse(at >0,intan/at, NA),
                                      capx_sale=ifelse(sale> 0 & capx >0, capx/sale,0))

##create lags of profitability variables

ROEData2023 <-ROEData2023  %>%
  group_by(gvkey) %>%
  arrange(datadate) %>%
  mutate (l1_roic = lag(roic),
          l2_roic = lag(l1_roic),
          l1_salegr = lag(salegr),
          l1_gp_margin  = lag(gp_margin),
          l1_np_margin  = lag(np_margin),
          l1_op_exp = lag(op_exp),
          l1_ebit_margin  = lag(ebit_margin))%>% ungroup()

##create changesin profitability variables

ROEData2023 <-ROEData2023  %>% mutate(chg_roic = roic-l1_roic,
                                      abs_chg_roic = abs(chg_roic),
                                      perc_chg_op_exp =ifelse(l1_op_exp >0, (op_exp/l1_op_exp)-1,NA),
                                      abs_perc_chgo_opexp = abs(perc_chg_op_exp))

#create liquidity variables
ROEData2023 <-ROEData2023  %>% mutate(currentratio = ifelse(lct > 0, act / lct, NA),
                                      quickratio =ifelse(che >= 0 & rect >=0, (che+rect) / lct, NA ),
                                      ccc = daysstock1 +dayreceivable2 -daysap,
                                      fcf=oancf+ivncf,
                                      timesinterest=ifelse(oibdp>0 & xint> 0,oibdp/xint, NA))


#create accrualvariables. 
ROEData2023 <-ROEData2023  %>% mutate(accruals_ta = abs(ibc-oancf)/((at+l1_at)/2),
                                      accruals_cfo =abs(ibc-oancf)/abs(oancf),
                                      accruals_depr_cfo = abs(ibc +dp-oancf)/abs(oancf))

#create market-price variables.
ROEData2023 <-ROEData2023  %>% mutate(mktcap = end_prccd*(end_cshoc/1000000),
                                      mb = if_else(ceq > 0, (end_prccd*(end_cshoc/1000000))/ceq, NA),
                                      pe = if_else(epsexcon >0, end_prccd/epsexcon, NA))


# Getting subsample. Removing observation that do not meet criteria. 
#40203010 Asset Management & Custody Banks 40204010 Mortgage REITs 601010 Equity Real Estate  Investment Trusts  (REITs)

Benchdata <- ROEData2023 %>%
  filter(!(gsubind=="40203010" | gsubind=="40204010" | gind=="601010"|at<0 )| is.na(gsubind) | is.na(gind)| is.na(at) )%>%
  filter(fyear>1999) %>% 
  filter(roe>-3 & roe <3 & return > -3 & return < 3  )


##quantile rank of the column by group
Benchdata <- Benchdata%>% group_by(fyear) %>%
  mutate(size_group = ntile(l1_at,4),
         size_decile = ntile(l1_at,10),
         large = ifelse (size_decile > 8, 1,0))%>% ungroup()


ROESummary <-Benchdata  %>% group_by(size_group)%>%
  summarise(count = n(),mean = mean(roe ,na.rm = TRUE ), median =median(roe , na.rm = TRUE),
            min = min(roe ,na.rm = TRUE ),max = max(roe , na.rm = TRUE),
            quant25 = quantile(roe , probs = 0.25,na.rm = TRUE ), quant75 = quantile(roe , probs = 0.75, na.rm = TRUE), sd = sd(roe, na.rm = TRUE), p10 = quantile(roe, probs = 0.10,na.rm = TRUE  ), p90 = quantile(roe, probs = 0.90,na.rm = TRUE  ) )

