#' Split Weights and Sizes by Date
#'
#' @param pw data.table (or data.frame) of portfolio weights (or other measure based on which fund similarity should be calculated). Must be a long-form dataset, with columns specifying fund, stock, date, and the  corresponding portfolio weight.
#' @param size data.table (or data.frame) of fund sizes. Must be a long-form dataset, with columns corresponding to fund, date, and the size measure.
#' @param fund.id Name of the variable (supplied as a character string) containing fund identifiers. Defaults to "wficn".
#' @param date.id Name of the variable (supplied as a character string) conteining time identifiers. Defaults to "date".
#' @param stock.id Name of the variable (supplied as a character string) containing security identifiers. Defaults to "permno".
#' @param w.var Name of the variable (supplied as a character string) based on which fund similarity is to be calculated. Defaults to "w". In Jakab (2018), w.var is market-adjusted portfolio weights.
#' @param fund.size Name of the variable (supplied as a character string) based on which competitor size is to be calculated. In Jakab (2018), fund.size is FundSize, i.e. total net assets normalized by the market capitalization of all common equity in CRSP.
#'
#' @return A nested list. Each top level element corresponds to a date (reflected in the list names), and contains three sub-elements. (i) is an N x K matrix of portfolio weights (or measures used for calculating fund similarity); row names contain N stock ids, column names contain K fund ids. (ii) is a K x 1 matrix of fund sizes, with row names containing fund ids. (iii) is the date, in string format.
#'
#' @export
#'
#' @import data.table
#'
#' @examples
#' pw <- data.table(
#'   wficn  = paste0("fund", rep(1:2, each = 50)),
#'   date   = paste0("date", rep(rep(1:2, each = 25), 2)),
#'   permno = paste0("stock", rep(1:25, 4)),
#'   w      = runif(100))
#' pw[, w := w / sum(w), by = .(wficn, date)]
#' pw
#' size <- data.table(
#'   wficn = paste0("fund", rep(1:2, each = 2)),
#'   date  = paste0("date", rep(1:2, 2)),
#'   fund.size = c(.1, .11, .2, .25))
#' size
#' SplitByDate(pw, size)

SplitByDate <- function(pw, size,
                        fund.id  = "wficn",
                        date.id  = "date",
                        stock.id = "permno",
                        w.var    = "w",
                        size.var = "fund.size") {

  # Check Inputs ------------------------------------------------------

  # make sure pw and size can be readily coerced to data.table
  if (!("data.frame" %in% class(pw)) | !("data.frame" %in% class(size))) {
    stop("Please provide both pw and size in data.frame or data.table formats")
  }
  # if not already data.table, then convert
  if (!("data.table" %in% class(pw))) {
    setDT(pw)
  }
  if (!("data.table" %in% class(size))) {
    setDT(size)
  }

  # check if fund.id., date.id, stock.id available in pw
  if (any(!c(fund.id, date.id, stock.id, w.var) %in% names(pw))) {
    stop("Fund, date and stock variables must be present in pw")
  }
  # make sure fund.id, date.id, available in sizes
  if (any(!c(fund.id, date.id, size.var) %in% names(size))) {
    stop("Fund and date, and size variables must be present in size")
  }

  # check for missing values
  print("Checking for missing values...")
  if (any(is.na(pw)) | any(is.na(size))) {
    stop("pw and size should contain no missing values")
  }
  if (pw[, any(!is.finite(get(w.var)))] & size[, any(!is.finite(get(size.var)))]) {
    stop("All weights and sizes must be finite")
  }
  print("...done")

  # sort for some speed gain down the line
  setkeyv(pw, c(fund.id, date.id, stock.id))
  setkeyv(size, c(fund.id, date.id))

  # make sure there are no duplicate fund.id-date.id-stock.id in pw
  print("Checking for duplicates...")
  if (anyDuplicated(pw[, .SD, .SDcols = c(fund.id, date.id, stock.id)])) {
    stop("Duplicate fund-date-stock triples in pw dataset")
  }
  # make sure there are no duplicate fund.id-date.id pairs in size
  if (anyDuplicated(size[, .SD, .SDcols = c(fund.id, date.id)])) {
    stop("Duplicate fund-date pairs in size dataset")
  }
  print("...done")

  # Keep Common Obs ------------------------------------------------------

  # unique wficn-date pairs common to both datasets
  print("Finding fund-date pairs shared by both datasets...")
  fund.date <- size[, .SD, .SDcols = c(fund.id, date.id)][
    unique(pw[, .SD, .SDcols = c(fund.id, date.id)]),
      on = c(fund.id, date.id), nomatch = 0]
  print("...done")

  # stop if no common obs
  if (nrow(fund.date) == 0) {
    stop("No overlap between pw and date datasets")
  }

  # keep only fund-date pairs that occur in both datasets
  print("Restricting sample to common fund-date pairs...")
  size <- size[fund.date, on = c(fund.id, date.id), nomatch = 0]
  pw   <- pw[fund.date, on = c(fund.id, date.id), nomatch = 0]
  print("...done")


  # Split by Date --------------------------------------------------------------

  # sort by date
  setkeyv(size, c(date.id, fund.id))
  setkeyv(pw, c(date.id, fund.id, stock.id))

  # split portfolio weight dataset by date and arrange in a list of data.tables
  print("Splitting pw by date...")
  pw.list <- split(pw, by = date.id, keep.by = FALSE)
  print("...done")

  # split fund size dataset by date and arrange in a list of data.tables
  print("Splitting size by date...")
  size.list <- split(size, by = date.id, keep.by = FALSE)
  print("...done")


  # Convert Data --------------------------------------------------------------

  print("Reshaping portfolio weights to wide and converting list contents to matrix...")

  # formula for casting to wide
  dcast.formula <- as.formula(paste(stock.id, fund.id, sep = " ~ "))

  # loop over dates
  i.list <- seq_along(names(pw.list))
  pb <- txtProgressBar(max = length(i.list), style = 3)
  for (i in i.list) {

    # reshape portfolio weights to wide
    pw.list[[i]] <- dcast(pw.list[[i]], dcast.formula, value.var = w.var, fill = 0)
    # extract stock ids
    pw.rn <- pw.list[[i]][[stock.id]]

   # convert portfolio weights to matrix
    pw.list[[i]] <- as.matrix(
      pw.list[[i]][ , .SD, .SDcols = !names(pw.list[[i]]) %in% stock.id])
    rownames(pw.list[[i]]) <- pw.rn

    # convert size to matrix
    size.rn <- size.list[[i]][[fund.id]]
    size.list[[i]] <- as.matrix(size.list[[i]][[size.var]])
    rownames(size.list[[i]]) <- size.rn

    # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # zip together lists
  print("Zipping together lists...")
  csl <- Map(list, pw.list, size.list, names(size.list))
  print("...done")
  return(csl)
}
