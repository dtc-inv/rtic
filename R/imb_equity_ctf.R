
#' @title Write IMB Equity Slide
#' @param pres presentation object from officer
#' @param rpt report object
#' @param dict dictionary table from excel
#' @param descr description table from excel
#' @param locator list with chart and table locations
#' @param slide_title string for title
#' @param tm10 t-minus 10 years date
#' @return pres with added slide
#' @export
write_equity <- function(pres, rpt, dict, descr, locater, slide_title, tm10) {
  
  col <- rpt$col
  set_flextable_defaults(font.size = 8)
  dtc_name <- rpt$port[[1]]$name
  dict <- dict[dict$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in dictionary"))
  }
  descr <- descr[descr$Page == dtc_name, ]
  trail_perf_ft <- create_trail_perf_tbl(rpt)
  perf_stat_ft <- create_perf_tbl(rpt, tm10)
  char_ft <- create_char_tbl(rpt)
  wealth_cht <- create_wealth_cht(rpt, tm10)
  capm_cht <- create_capm_cht(rpt, tm10)
  # country / sector chart switch
  sect_cht <- create_sector_cht(rpt)
  
  
  alloc_tbl <- create_alloc_tbl(dict, col)
  descr_tbl <- create_descr_tbl(descr, col)
  
  pres <- add_slide(pres, layout = "Body Slide", master = "DTC-Theme-2021") |>
    ph_with(slide_title, ph_location_label("Text Placeholder 18")) |>
    ph_with(
      trail_perf_ft, 
      ph_location(
        left = locater$trail_perf_ft["left"],
        top = locater$trail_perf_ft["top"])) |>
    ph_with(
      perf_stat_ft, 
      ph_location(
        left = locater$perf_stat_ft["left"], 
        top = locater$perf_stat_ft["top"])) |>
    ph_with(
      char_ft, 
      ph_location(
        left = locater$char_ft["left"], 
        top = locater$char_ft["top"])) |>
    ph_with(
      wealth_cht, 
      ph_location(
        left = locater$wealth_cht["left"], 
        top = locater$wealth_cht["top"], 
        height = locater$wealth_cht["height"], 
        width = locater$wealth_cht["width"])) |>
    ph_with(
      capm_cht, 
      ph_location(
        left = locater$capm_cht["left"], 
        top = locater$capm_cht["top"], 
        height = locater$capm_cht["height"],
        width = locater$capm_cht["width"])) |>
    ph_with(
      sect_cht, 
      ph_location(
        left = locater$sect_cht["left"], 
        top = locater$sect_cht["top"], 
        width = locater$sect_cht["width"], 
        height = locater$sect_cht["height"])) |>
    ph_with(
      alloc_tbl, 
      ph_location(
        left = locater$alloct_tbl["left"], 
        top = locater$alloct_tbl["top"], 
        height = locater$alloct_tbl["height"])) |>
    ph_with(
      descr_tbl, 
      ph_location(
        left = locater$descr_tbl["left"], 
        top = locater$descr_tbl["top"]))
  
  return(pres)
}
