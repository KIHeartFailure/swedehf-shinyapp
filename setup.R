library(shiny)
library(shinythemes)
library(shinybusy)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

select_func <- function(ef, prevhfh6mo, nyha,
                        gfrckdepi, ntprobnp = NULL, ntprobnp_af, bpsys) {
  checkempty <- c(is.null(ef), is.null(prevhfh6mo), is.null(nyha), is.null(gfrckdepi), is.null(ntprobnp), is.null(bpsys))
  if (any(checkempty)) {
    allcombsout <- 0
  } else if (any(c(
    diff(sort(as.numeric(ef))), diff(sort(as.numeric(nyha))),
    diff(sort(as.numeric(ntprobnp))), diff(sort(as.numeric(bpsys)))
  ) > 1)) {
    allcombsout <- -1
  } else {
    if (ntprobnp_af) {
      allcombsout <- allcombs %>%
        filter(shf_ef %in% paste0(sort(ef), collapse = "|") &
          shf_sos_prevhfh6mo %in% paste0(sort(prevhfh6mo), collapse = "|") &
          shf_nyha %in% paste0(sort(nyha), collapse = "|") &
          shf_gfrckdepi %in% paste0(sort(gfrckdepi), collapse = "|") &
          shf_ntprobnp_af == TRUE &
          shf_bpsys %in% paste0(sort(bpsys), collapse = "|")) %>%
        pull(n)
    } else {
      allcombsout <- allcombs %>%
        filter(shf_ef %in% paste0(sort(ef), collapse = "|") &
          shf_sos_prevhfh6mo %in% paste0(sort(prevhfh6mo), collapse = "|") &
          shf_nyha %in% paste0(sort(nyha), collapse = "|") &
          shf_gfrckdepi %in% paste0(sort(gfrckdepi), collapse = "|") &
          shf_ntprobnp_af == FALSE &
          shf_ntprobnp %in% paste0(sort(ntprobnp), collapse = "|") &
          shf_bpsys %in% paste0(sort(bpsys), collapse = "|")) %>%
        pull(n)
    }
  }

  return(allcombsout)
}

figfunc <- function(subdata) {
  subdata <- subdata %>%
    select(-r_0, -r_1) %>%
    pivot_longer(cols = c(rate_0, rate_1), names_to = "el", values_to = "rate") %>%
    filter(!is.na(rate)) %>%
    mutate(el = factor(str_remove(el, "rate_"), levels = c("0", "1"), labels = c("Not eligible", "Eligible")))

  minmax <- c(0, ceiling((max(subdata$rate) + max(subdata$rate) * .1) / 10) * 10)
  ggplot(subdata, aes(x = time, y = rate, fill = el)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(limits = minmax, breaks = minmax) +
    facet_wrap(vars(outname), ncol = 1) + # , scales = "free_y") +
    scale_fill_manual(values = c("#00688B", "darkseagreen4")) +
    theme_classic() +
    theme(
      text = element_text(face = "bold", size = 16),
      legend.position = "bottom",
      legend.box = "vertical",
      axis.text = element_text(color = "black"),
      axis.title.x = element_blank(),
      legend.title = element_blank()
    ) +
    labs(y = "Events/100 person-years") +
    geom_text(aes(label = round(rate, 0)), position = position_dodge(width = 0.9), vjust = -0.25, fontface = "bold")
}

mytheme <- bs_theme(
  # Controls the default grayscale palette
  # bg = "#DCDCDC",  fg = "#698B69",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#00688B", secondary = "#009ACD",
  # base_font = c("Grandstander", "sans-serif"),
  base_font = "'Helvetica Neue', Helvetica, sans-serif",
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  # Can also add lower-level customization
  "input-border-color" = "#00688B"
)

load("./data/shinydata.RData")
