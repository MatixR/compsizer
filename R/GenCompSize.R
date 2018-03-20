#' Generate CompetitorSize from a dataset of portfolio weights and a dataset of fund sizes
#'
#' @param pw data.table (or data.frame) of portfolio weights (or other measure based on which fund similarity should be calculated). Must be a long-form dataset, with columns specifying fund, stock, date, and the  corresponding portfolio weight.
#' @param size data.table (or data.frame) of fund sizes. Must be a long-form dataset, with columns corresponding to fund, date, and the size measure.
#' @param fund.id Name of the variable (supplied as a character string) containing fund identifiers. Defaults to "wficn".
#' @param date.id Name of the variable (supplied as a character string) conteining time identifiers. Defaults to "date".
#' @param stock.id Name of the variable (supplied as a character string) containing security identifiers. Defaults to "permno".
#' @param w.var Name of the variable (supplied as a character string) based on which fund similarity is to be calculated. Defaults to "w". In Jakab (2018), w.var is market-adjusted portfolio weights.
#' @param fund.size Name of the variable (supplied as a character string) based on which competitor size is to be calculated. In Jakab (2018), fund.size is FundSize, i.e. total net assets normalized by the market capitalization of all common equity in CRSP.
#'
#' @return A data.table of fund.ids, dates, and CompetitorSize (labeled comp.size).
#'
#' @export
#'
#' @import data.table
#' @importFrom pbapply pblapply pboptions
#" 
#' @examples
#' pw <- data.table(
#'   wficn  = paste0("fund", rep(1:2, each = 50)),
#'   date   = paste0("date", rep(rep(1:2, each = 25), 2)),
#'   permno = paste0("stock", rep(1:25, 4)),
#'   w      = runif(100))
#' pw[, w := w / sum(w), by = .(wficn, date)]
#' size <- data.table(
#'   wficn = paste0("fund", rep(1:2, each = 2)),
#'   date  = paste0("date", rep(1:2, 2)),
#'   fund.size = c(.1, .11, .2, .25))
#' GenCompSize(pw, size)

GenCompSize <- function(pw, size,
                        fund.id  = "wficn",
                        date.id  = "date",
                        stock.id = "permno",
                        w.var    = "w",
                        size.var = "fund.size") {

  # split pw, size by date and convert into convenient matrix form
  csl <- SplitByDate(pw, size,
                     fund.id  = fund.id,
                     date.id  = date.id,
                     stock.id = stock.id,
                     w.var    = w.var,
                     size.var = size.var)

  # helper function for date-by-date calculations
  MainFn <- function(dt) {
    # cosine similarities (the first element contains portfolio weight matrix)
    cos.sim <- CosSim(dt[[1]])
    # CompetitorSize (the second element contains fund size matrix)
    cs <- CompSize(cos.sim, dt[[2]])
    # output a data.table
    cs.dt <- data.table(cs, dt[[3]], keep.rownames = TRUE)
    setnames(cs.dt, c(fund.id, "comp.size", date.id))
    return(cs.dt)
  }

  # calculate CompetitorSize
  print("Calculating cosine similarities and CompetitorSize (takes a few min)")
  pboptions(char = "=")
  cs.list <- pblapply(csl, MainFn)

  # tidy up output
  cs.dt <- rbindlist(cs.list)
  setcolorder(cs.dt, c(fund.id, date.id, "comp.size"))
  setkeyv(cs.dt, c(fund.id, date.id))
  return(cs.dt)
}
