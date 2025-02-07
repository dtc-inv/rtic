
write_bond <- function(pres, rpt, dict, descr, locater,
                       slide_title, is_ctf = FALSE, 
                       pie_type = c("Quality", "Sector")) {
  
  pie_type <- pie_type[1]
  set_flextable_defaults(font.size = 8)
  dict <- dict[dict$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in dictionary"))
  }
  descr <- descr[descr$Page == dtc_name, ]
  if (nrow(dict) == 0) {
    stop(paste0(dtc_name, " not found in description"))
  }
  trail_perf_ft <- create_trail_perf_tbl(rpt) |>
    width(2.25, j = 1)
  perf_stat_ft <- create_perf_tbl(rpt)
  wealth_cht <- create_wealth_cht(rpt)
  capm_cht <- create_fund_capm_chart(combo, col, legend_loc = "bottom")

  char_tbl <- dict[dict$DataType == "Characteristics", 3:4]
  char_tbl <- as.data.frame(char_tbl)
  colnames(char_tbl) <- c("CHARACTERISTICS", "FUND")
  is_per <- char_tbl$CHARACTERISTICS %in% c("Expense Ratio", "SEC Yield", "YTM")
  is_cur <- char_tbl$CHARACTERISTICS %in% "AUM (MMs)"
  is_num <- char_tbl$CHARACTERISTICS %in% "Duration"
  char_tbl[is_per, 2] <- percent(as.numeric(char_tbl[is_per, 2]), 0.01)
  char_tbl[is_cur, 2] <- number(as.numeric(char_tbl[is_cur, 2]), big.mark = ",")
  char_tbl[is_num, 2] <- number(as.numeric(char_tbl[is_num, 2]), 0.1)
  char_ft <- flextable(char_tbl) |>
    theme_alafoli() |>
    font(part = "body", fontname = "Source Sans Pro Light") |>
    font(part = "header", fontname = "La Gioconda TT") |>
    color(i = 1, color = col[4], part = "header") |>
    height(0.18, i = 1:nrow(char_tbl)) |>
    width(j = 1, width = 1.2)
  
  pie_tbl <- dict[dict$DataType == pie_type, ]
  pie_tbl$Value <- as.numeric(pie_tbl$Value)
  pie_tbl$Label <- scales::percent(pie_tbl$Value / 100, 0.1)
  pie_cht <- ggplot(pie_tbl, aes(x = "", y = Value, fill = Field, label = Label)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = col) +
    labs(fill = "", title = toupper(pie_type)) +
    geom_text(position = position_stack(0.5), col = "white", size = 2) +
    theme(
      plot.title = element_text(
        family = "La Gioconda TT", 
        color = col[4], 
        size = 8
      ),
      legend.text = element_text(
        size = 6, 
        family = "Source Sans Pro Light", 
        color = "grey40"
      ),
      legend.key.size = unit(0.2, "in"),
      legend.box.spacing = unit(-10, "pt")
    )
  
  bar_tbl <- dict[dict$DataType == "Maturity", ]
  bar_tbl$Value <- as.numeric(bar_tbl$Value) / 100
  bar_tbl$Lbl <- percent(bar_tbl$Value, 0.1)
  bar_tbl$Field <- factor(bar_tbl$Field, unique(bar_tbl$Field))
  sect_cht <- ggplot(bar_tbl, aes(x = Field, y = Value, label = Lbl, fill = Field)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c(col, "grey", "seagreen", "brown")) +
    geom_text(
      aes(y = Value + 0.025), 
      size = 1.75, 
      position = position_dodge(0.9),
      color = "grey40") +
    xlab("") + ylab("") + labs(title = "QUALTIY", fill = "") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(
            size = 6, 
            family = "Source Sans Pro Light", 
            color = "grey40"
          ),
          plot.title = element_text(
            family = "La Gioconda TT", 
            color = col[4], 
            size = 8
          ),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.text.x = element_text(
            size = 5, 
            family = "Source Sans Pro Light",
            color = "grey40"
          ))
  
  descr_tbl <- create_descr_tbl(descr)
  
  pres <- add_slide(pres, layout = "Body Slide", master = "DTC-Theme-2021") |>
    ph_with(slide_title, ph_location_label("Text Placeholder 18")) |>
    ph_with(
      pie_cht,
      ph_location(
        left = locater$pie_cht["left"],
        top = locater$pie_cht["top"],
        height = locater$pie_cht["height"],
        width = locater$pie_cht["width"]
      )
    ) |>
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
      descr_tbl, 
      ph_location(
        left = locater$descr_tbl["left"], 
        top = locater$descr_tbl["top"]))
  
  return(pres)
}