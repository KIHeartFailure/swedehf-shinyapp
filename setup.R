select_func <- function(ef, prevhfh6mo, nyha,
                        af, gfrckdepi, ntprobnp, bpsys) {
  checkempty <- c(is.null(ef), is.null(prevhfh6mo), is.null(nyha), is.null(af), is.null(gfrckdepi), is.null(ntprobnp), is.null(bpsys))
  if (any(checkempty)) {
    allcombsout <- 0
  } else if (any(c(
    diff(sort(as.numeric(ef))), diff(sort(as.numeric(prevhfh6mo))), diff(sort(as.numeric(nyha))), diff(sort(as.numeric(af))),
    diff(sort(as.numeric(gfrckdepi))), diff(sort(as.numeric(ntprobnp))), diff(sort(as.numeric(bpsys)))
  ) > 1)) {
    allcombsout <- -1
  } else {
    allcombsout <- allcombs %>%
      filter(shf_ef %in% paste0(sort(ef), collapse = "|") &
        shf_sos_prevhfh6mo %in% paste0(sort(prevhfh6mo), collapse = "|") &
        shf_nyha %in% paste0(sort(nyha), collapse = "|") &
        shf_sos_com_af %in% paste0(sort(af), collapse = "|") &
        shf_gfrckdepi %in% paste0(sort(gfrckdepi), collapse = "|") &
        shf_ntprobnp %in% paste0(sort(ntprobnp), collapse = "|") &
        shf_bpsys %in% paste0(sort(bpsys), collapse = "|")) %>%
      pull(n)
  }

  return(allcombsout)
}

load("./data/shinydata.RData")
