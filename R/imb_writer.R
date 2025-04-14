#' @export
load_imb_dict <- function() {
  dict_file <- "N:/Investment Team/REPORTING/IMB/imb-writer/imb-data-input.xlsx"
  dict <- readxl::read_excel(dict_file, "data")
  descr <- readxl::read_excel(dict_file, "descr")
  res <- list()
  res$dict <- dict
  res$descr <- descr
  return(res)
}
  
#' @export
write_imb <- function() {  
  
  as_of <- floor_date(Sys.Date(), "months") - 1
  tm10 <- as_of - years(10)
  
  res <- load_imb_dict()
  
  db <- Database$new("~/api_keys.RData", 
                     "C:/Users/asotolongo/AppData/Local/anaconda3/")
  
  locater <- list(
    pie_cht = c(left = 6.47, top = 0.95, height = 2.04, width = 2.76),
    trail_perf_ft = c(left = 0.45, top = 6),
    perf_stat_ft = c(left = 0.45, top = 3.17),
    char_ft = c(left = 0.45, top = 4.91),
    wealth_cht = c(left = 2.2, top = 3.17, height = 2.78, width = 3.6),
    capm_cht = c(left = 6, top = 3.17, height = 2.78, width = 3.6),
    sect_cht = c(left = 4.81, top = 1.10, width = 4.79, height = 1.95),
    alloct_tbl = c(left = 0.45, top = 2.05, height = 1.15),
    descr_tbl = c(left = 0.45, top = 1.10)
  )
  
  bond_pos <- locater
  bond_pos$perf_stat_ft <- c(left = 0.34, top = 2.39)
  bond_pos$descr_tbl <- c(left = 0.34, top = 0.92)
  bond_pos$alloct_tbl = c(left = 0.34, top = 1.67, height = 1)
  bond_pos$sect_cht <- c(left = 2.46, top = 1.97, height = 1.31, width = 4.2)
  bond_pos$char_ft <- c(left = 0.34, top = 4.21)
  bond_pos$wealth_cht <- c(left = 2.56, top = 3.28, height = 2.84, width = 3.36)
  bond_pos$capm_cht <- c(left = 6.17, top = 3.28, height = 2.83, width = 3.36)
  bond_pos$trail_perf_ft <- c(left = 0.34, top = 6.05)
  
  core_eq_loc <- locater
  core_eq_loc$alloct_tbl["top"] <- 2.2
  
  ms_pos <- locater
  ms_pos$alloct_tbl <- locater$sect_cht
  
  # start pres
  pres <- read_pptx("N:/Investment Team/REPORTING/IMB/imb-writer/template.pptx")
  
  print("VWSUX")
  p1 <- db$create_port_from_ids("VWSUX")
  b <- db$create_port_from_ids("BofAML Municipals 1-2 Yr") 
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,
    slide_title = "Vanguard Short-Term Tax Exempt",
    tm10 = tm10)
  rm(p1, b, rpt)
  
  print("FLTMX")
  p1 <- db$create_port_from_ids("FLTMX")
  b <- db$create_port_from_ids("BofAML Municipals 1-12 Yr")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,
    slide_title = "Fidelity Intermediate Muni",
    tm10 = tm10)
  rm(p1, b, rpt)

  print("Short Duration")
  p1 <- db$create_port_from_ids("Short Duration")
  b <- db$create_port_from_ids("BofAML U.S. Treasuries 1-3 Yr")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,
    slide_title = "Short Duration CTF",
    tm10 = tm10,
    pie_type = "Sector")
  rm(p1, b, rpt)
  
  print("Core Fixed Income")
  p1 <- db$create_port_from_ids("Core Fixed Income")
  b <- db$create_port_from_ids("Bloomberg Barclays U.S. Aggregate")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,    
    slide_title = "Core Fixed Income CTF",
    tm10 = tm10,
    pie_type = "Sector",
    is_ctf = TRUE
  )
  rm(p1, b, rpt)
  
  print("TCPNX")
  p1 <- db$create_port_from_ids("TCPNX")
  b <- db$create_port_from_ids("Bloomberg Barclays U.S. Aggregate")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,    
    slide_title = "Touchstone Total Return Bond",
    tm10 = tm10,
    pie_type = "Sector"
  )
  rm(p1, b, rpt)
  
  print("PLDTX")
  p1 <- db$create_port_from_ids("PLDTX")
  b <- db$create_port_from_ids("BofAML Municipals 1-12 Yr")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,    
    slide_title = "Pimco Low Duration II",
    tm10 = tm10,
    pie_type = "Sector")
  rm(p1, b, rpt)
  
  print("PTTRX")
  p1 <- db$create_port_from_ids("PTTRX")
  b <- db$create_port_from_ids("Bloomberg Barclays U.S. Aggregate")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,    
    slide_title = "Pimco Total Return",
    tm10 = tm10,
    pie_type = "Sector"
  )
  rm(p1, b, rpt)
  
  print("LSFYX")
  p1 <- db$create_port_from_ids("LSFYX")
  b <- db$create_port_from_ids("S&P / LSTA Leveraged Loan")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,    
    slide_title = "Loomis Sayles Floating Rate",
    tm10 = tm10,
    pie_type = "Sector"
  )
  rm(p1, b, rpt)
  
  print("CPHUX")
  p1 <- db$create_port_from_ids("CPHUX")
  b <- db$create_port_from_ids("Bloomberg Barclays U.S. Aggregate")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,
    slide_title = "Columbia Strategic Income",
    tm10 = tm10,
    pie_type = "Sector"
  )
  rm(p1, b, rpt)
  
  print("VTIP")
  p1 <- db$create_port_from_ids("VTIP")
  b <- db$create_port_from_ids("Bloomberg Barclays U.S. TIPS 0-5 Yr")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,    
    slide_title = "TIPS",
    tm10 = tm10,
    pie_type = "Sector"
  )
  rm(p1, b, rpt)
  
  print("TLT")
  p1 <- db$create_port_from_ids("TLT")
  b <- db$create_port_from_ids("Bloomberg Barclays U.S. Treasury 20+ Yr")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_bond(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = bond_pos,
    slide_title = "Long Treasuries",
    tm10 = tm10,
    pie_type = "Sector"
  )
  rm(p1, b, rpt)
  
  # equity ----
  
  print("US Core Equity")
  p1 <- db$create_port("US Core Equity", TRUE)
  b <- db$create_port_from_ids("IWV", tr_id = "Russell 3000")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_equity(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = core_eq_loc,
    slide_title = "U.S. Core Equity CTF",
    tm10 = tm10
  )
  rm(p1, b, rpt)

  print("US Core Equity MF")
  p1 <- db$create_port("US Core Equity MF", TRUE)
  b <- db$create_port_from_ids("IWV", tr_id = "Russell 3000")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_equity(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = core_eq_loc,
    slide_title = "U.S. Core Equity Strategy",
    tm10 = tm10
  )
  rm(p1, b, rpt)
  
  print("US Active Equity")
  p1 <- db$create_port("US Active Equity", TRUE)
  b <- db$create_port_from_ids("IWV", tr_id = "Russell 3000")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_equity(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = locater,
    slide_title = "U.S. Active Equity CTF",
    tm10 = tm10
  )
  rm(p1, b, rpt)
  
  print("US Active Equity MF")
  p1 <- db$create_port("US Active Equity MF", TRUE)
  b <- db$create_port_from_ids("IWV", tr_id = "Russell 3000")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_equity(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = locater,
    slide_title = "U.S. Active Equity Strategy",
    tm10 = tm10
  )
  rm(p1, b, rpt)
  
  print("International Equity")
  p1 <- db$create_port("International Equity", TRUE)
  b <- db$create_port_from_ids("ACWX", tr_id = "MSCI ACWI ex US")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_equity(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = locater,
    slide_title = "International Equity CTF",
    tm10 = tm10,
    bar_cht_opt = "region"
  )
  rm(p1, b, rpt)
  
  print("International Equity MF")
  p1 <- db$create_port("International Equity MF", TRUE)
  b <- db$create_port_from_ids("ACWX", tr_id = "MSCI ACWI ex US")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_equity(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = locater,
    slide_title = "International Equity Strategy",
    tm10 = tm10,
    bar_cht_opt = "region"
  )
  rm(p1, b, rpt)

  print("Multi-Strategy")
  p1 <- db$create_port_from_ids("Multi-Strategy")
  b <- db$create_port_from_ids("MSCI ACWI")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_multi_strat(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = ms_pos,
    slide_title = "Multi-Strategy CTF",
    tm10 = tm10
  )
  rm(p1, b, rpt)
  
  print("Multi-Strategy MF")
  p1 <- db$create_port_from_ids("Multi-Strategy MF")
  b <- db$create_port_from_ids("MSCI ACWI")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_multi_strat(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = ms_pos,
    slide_title = "Multi-Strategy Liquid",
    tm10 = tm10
  )
  rm(p1, b, rpt)
  
  print("Private Diversifiers")
  p1 <- db$create_port_from_ids("Private Diversifiers")
  b <- db$create_port_from_ids("MSCI ACWI")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_pdf(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = ms_pos,
    slide_title = "Private Diversifiers",
    tm10 = tm10
  )
  rm(p1, b, rpt)
  
  print("Private Diversifiers II")
  p1 <- db$create_port_from_ids("Private Diversifiers II")
  b <- db$create_port_from_ids("MSCI ACWI")
  rpt <- Reporter$new(list(p1), b)
  pres <- write_pdf(
    as_of = as_of,
    pres = pres,
    rpt = rpt,
    dict = res$dict,
    descr = res$descr,
    locater = ms_pos,
    slide_title = "Private Diversifiers",
    tm10 = tm10,
    lgnd_pos = "bottom"
  )
  rm(p1, b, rpt)
  
  # write out
  print(pres, "C:/Users/asotolongo/Downloads/test.pptx")
}
