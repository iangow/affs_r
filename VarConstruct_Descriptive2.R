library(tidyverse)
library(modelsummary)
library(fixest)
library(duckdb)

ROEData2023_raw <- read_csv("data/FatData2023x.csv")

ROEData2023 <-
  ROEData2023_raw |>
  mutate(across(c(dlc, dltt, pstk, che, ivao, ceq, mibt), \(x) coalesce(x, 0)),
         totaldebt = dlc + dltt + pstk,
         financialassets = che + ivao,
         capital = totaldebt + ceq + mibt,
         invcapital = capital - che,
         groupequity = ceq + mibt) |>
  group_by(gvkey) |>
  arrange(datadate) |>
  mutate(across(c(at, ceq, sale, invt, capital, invcapital,
                  ap, capx, totaldebt, oiadp, rectr), lag,
                .names = "l1_{.col}")) |>
  ungroup() |>
  # create profitability variables
  mutate(aveceq = (ceq + l1_ceq) / 2,
        roe = if_else(aveceq > 0, ib/aveceq,NA),
        xint_atax = if_else(is.na(xint), 0, xint*0.7),
        nbc = xint_atax +if_else(is.na(dvp), 0, dvp)   ,
        nbc_rate = (nbc)/((totaldebt+l1_totaldebt)/2),
        groupprofit =ib+if_else(is.na(mii), 0,mii )-if_else(is.na(dvp), 0, dvp),
        operating_profit = groupprofit +nbc,
        operating_profit1 = groupprofit +nbc -if_else(is.na(idit), 0, idit*0.7),
        avecapital = ((capital +l1_capital)/2),
        roc = if_else(avecapital > 0,operating_profit/avecapital,NA),
        aveinvcapital = ((invcapital +l1_invcapital)/2),
        roic = if_else(aveinvcapital > 0,oiadp/aveinvcapital,NA),
        salegr =if_else (sale > 0 & l1_sale > 0, (sale/l1_sale)-1,NA),
        capxgr=(capx/l1_capx)-1,
        dayreceivable1 = if_else(rectr > 0 & sale >0, rectr/(sale/365),NA),
        daysstock1 = if_else(invt> 0 & cogs>0, invt/(cogs/365), NA),
        dayreceivable2 = if_else(rectr > 0 & sale>0, ((rectr+l1_rectr)/2)/(sale/365),NA),
        daysstock2 = if_else(invt> 0 &cogs > 0, ((invt+l1_invt)/2)/(cogs/365), NA),
        daysap = if_else(ap> 0 & cogs > 0, ((ap+l1_ap)/2)/(cogs/365), NA),

        np_margin = if_else(sale > 0, operating_profit/sale,NA),
        gp_margin = if_else(sale> 0 & cogs > 0, (sale-cogs)/sale,NA),
        ebitda_margin = if_else(sale> 0, oibdp/sale,NA),
        ebit_margin = if_else(sale> 0, oiadp/sale,NA),
        ebit_post = if_else(oiadp> 0, 1,0),
        leverage = totaldebt/groupequity,
        asset_turnover1 = sale/((at +l1_at)/2),
        asset_turnover2 = if_else(avecapital > 0,sale/avecapital,NA),
        pbt = groupprofit + txt,
        etr = if_else(pbt > 0 & txt > 0, txt / pbt, NA),
        depr_rate = if_else(ppegt>0,dfxa/ppegt,NA),
        avgageppe =if_else(dfxa>0, dpact/dfxa,NA),
        chg_oiadp = oiadp -l1_oiadp,
        perc_chgoiadp =if_else(l1_oiadp!=0, chg_oiadp/abs(l1_oiadp),NA),
        abs_perc_chgoiadp = abs(perc_chgoiadp),
        abs_sales_gr =abs(salegr),
        op_exp = sale-oiadp,
        perch_chg_at = if_else(l1_at!=0, (at/l1_at)-1, NA),
        intan_intense= if_else(at >0,intan/at, NA),
        capx_sale=if_else(sale> 0 & capx >0, capx/sale,0)) |>
  # create lags of profitability variables
  group_by(gvkey) |>
  arrange(datadate) |>
  mutate(across(c(roic, salegr, gp_margin, np_margin, op_exp, ebit_margin),
                lag, .names = "l1_{.col}"),
         l2_roic = lag(roic, 2)) |>
  ungroup() |>
  # create changes in profitability variables
  mutate(chg_roic = roic - l1_roic,
         abs_chg_roic = abs(chg_roic),
         perc_chg_op_exp = if_else(l1_op_exp > 0, op_exp / l1_op_exp - 1, NA),
         abs_perc_chgo_opexp = abs(perc_chg_op_exp)) |>
  # create liquidity variables
  mutate(currentratio = if_else(lct > 0, act / lct, NA),
         quickratio =if_else(che >= 0 & rect >=0, (che+rect) / lct, NA ),
         ccc = daysstock1 + dayreceivable2 - daysap,
         fcf=oancf + ivncf,
         timesinterest = if_else(oibdp > 0 & xint > 0, oibdp / xint, NA)) |>
  #create accrualvariables
  mutate(accruals_ta = abs(ibc - oancf) / ((at + l1_at) / 2),
         accruals_cfo = abs(ibc - oancf) / abs(oancf),
         accruals_depr_cfo = abs(ibc + dp - oancf) / abs(oancf)) |>
  # create market-price variables.
  # mutate(mktcap = end_prccd * (end_cshoc / 1000000),
  #       mb = if_else(ceq > 0, (end_prccd * (end_cshoc / 1000000)) / ceq, NA),
  #       pe = if_else(epsexcon >0, end_prccd / epsexcon, NA))
  # Getting subsample. Removing observation that do not meet criteria.
  #40203010 Asset Management & Custody Banks 40204010 Mortgage REITs 601010 Equity Real Estate  Investment Trusts  (REITs)
  filter(!(gsubind=="40203010" | gsubind=="40204010" | gind=="601010"| at < 0 ) |
           is.na(gsubind) | is.na(gind)| is.na(at)) |>
  filter(fyear > 1999) |>
  # filter(roe > -3 & roe < 3 & return > -3 & return < 3) |>
  # quantile rank of the column by group
  group_by(fyear) |>
  mutate(size_group = ntile(l1_at, 4),
         size_decile = ntile(l1_at, 10),
         large = if_else(size_decile > 8, 1, 0)) |>
  ungroup() |>
  compute() |>
  system_time()


ROESummary <-
  ROEData2023 |>
  filter(!is.na(size_group)) |>
  group_by(size_group) |>
  summarise(
    count = n(),
    mean = mean(roe, na.rm = TRUE ),
    median = median(roe, na.rm = TRUE),
    min = min(roe, na.rm = TRUE),
    max = max(roe, na.rm = TRUE),
    quant25 = quantile(roe, probs = 0.25, na.rm = TRUE),
    quant75 = quantile(roe, probs = 0.75, na.rm = TRUE),
    sd = sd(roe, na.rm = TRUE),
    p10 = quantile(roe, probs = 0.10,na.rm = TRUE),
    p90 = quantile(roe, probs = 0.90,na.rm = TRUE)
  )

